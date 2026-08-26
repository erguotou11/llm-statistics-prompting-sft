import sys
import json
import subprocess
from pathlib import Path

from mlx_lm import load


# ============================================================
# 1. Configuration
# ============================================================

MODEL = "mlx-community/Meta-Llama-3-8B-Instruct-4bit"

DATA_DIR = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/train"
)

OUTPUT_DIR = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/llama3_sft"
)

TRAIN_FILE = DATA_DIR / "train.jsonl"
VALID_FILE = DATA_DIR / "valid.jsonl"

SMOKE_ADAPTER_DIR = OUTPUT_DIR / "adapters_smoke"
FINAL_ADAPTER_DIR = OUTPUT_DIR / "adapters"

RANDOM_SEED = 42

MAX_SEQ_LENGTH = 2048

# Set this to True after the smoke test succeeds
RUN_FORMAL_TRAINING = True


# ============================================================
# 2. Read JSONL
# ============================================================

def read_jsonl(path):
    """Read a JSONL file."""

    samples = []

    with open(path, "r", encoding="utf-8") as f:

        for line_number, line in enumerate(f, start=1):

            line = line.strip()

            if not line:
                continue

            try:
                sample = json.loads(line)

            except json.JSONDecodeError as e:

                raise ValueError(
                    f"Invalid JSON in {path.name}, "
                    f"line {line_number}: {e}"
                )

            samples.append(sample)

    return samples


# ============================================================
# 3. Validate one SFT sample
# ============================================================

def validate_chat_sample(sample, file_name, row_number):
    """Validate one chat-style SFT sample."""

    if "messages" not in sample:
        raise ValueError(
            f"{file_name}, row {row_number}: "
            f"missing 'messages'."
        )

    messages = sample["messages"]

    if not isinstance(messages, list):
        raise ValueError(
            f"{file_name}, row {row_number}: "
            f"'messages' must be a list."
        )

    if len(messages) != 2:
        raise ValueError(
            f"{file_name}, row {row_number}: "
            f"expected exactly 2 messages, "
            f"found {len(messages)}."
        )

    if messages[0].get("role") != "user":
        raise ValueError(
            f"{file_name}, row {row_number}: "
            f"first role must be 'user'."
        )

    if messages[1].get("role") != "assistant":
        raise ValueError(
            f"{file_name}, row {row_number}: "
            f"second role must be 'assistant'."
        )

    question = messages[0].get("content", "")
    answer = messages[1].get("content", "")

    if not str(question).strip():
        raise ValueError(
            f"{file_name}, row {row_number}: "
            f"empty user content."
        )

    if not str(answer).strip():
        raise ValueError(
            f"{file_name}, row {row_number}: "
            f"empty assistant content."
        )


# ============================================================
# 4. Validate train and validation datasets
# ============================================================

def validate_dataset(train_samples, valid_samples):
    """Check structure, duplicates and train-valid overlap."""

    for i, sample in enumerate(train_samples, start=1):
        validate_chat_sample(
            sample,
            "train.jsonl",
            i
        )

    for i, sample in enumerate(valid_samples, start=1):
        validate_chat_sample(
            sample,
            "valid.jsonl",
            i
        )

    train_questions = [
        sample["messages"][0]["content"].strip()
        for sample in train_samples
    ]

    valid_questions = [
        sample["messages"][0]["content"].strip()
        for sample in valid_samples
    ]

    train_unique = set(train_questions)
    valid_unique = set(valid_questions)

    train_duplicates = (
        len(train_questions) - len(train_unique)
    )

    valid_duplicates = (
        len(valid_questions) - len(valid_unique)
    )

    overlap = train_unique.intersection(
        valid_unique
    )

    print("\n===== Dataset Validation =====")

    print(
        f"Training samples:        "
        f"{len(train_samples)}"
    )

    print(
        f"Validation samples:      "
        f"{len(valid_samples)}"
    )

    print(
        f"Train duplicates:        "
        f"{train_duplicates}"
    )

    print(
        f"Validation duplicates:   "
        f"{valid_duplicates}"
    )

    print(
        f"Train/valid overlap:     "
        f"{len(overlap)}"
    )

    if train_duplicates > 0:
        raise ValueError(
            "Duplicated questions found in train.jsonl."
        )

    if valid_duplicates > 0:
        raise ValueError(
            "Duplicated questions found in valid.jsonl."
        )

    if overlap:
        raise ValueError(
            "Data leakage detected between "
            "train and validation sets."
        )


# ============================================================
# 5. Inspect token lengths
# ============================================================

def inspect_token_lengths(
    train_samples,
    valid_samples
):
    """
    Inspect sequence lengths using the actual model tokenizer.
    """

    print("\n===== Loading Model Tokenizer =====")
    print(f"Model: {MODEL}")

    _, tokenizer = load(
        MODEL,
        lazy=True
    )

    print("Tokenizer loaded successfully.")

    all_results = []

    def process_split(split_name, samples):

        lengths = []

        for row_number, sample in enumerate(
            samples,
            start=1
        ):

            messages = sample["messages"]

            # Apply the model's chat template
            formatted_text = (
                tokenizer.apply_chat_template(
                    messages,
                    tokenize=False,
                    add_generation_prompt=False
                )
            )

            token_ids = tokenizer.encode(
                formatted_text
            )

            token_count = len(token_ids)

            lengths.append(token_count)

            all_results.append(
                {
                    "split": split_name,
                    "row": row_number,
                    "tokens": token_count,
                }
            )

        print(f"\n===== {split_name.upper()} TOKEN LENGTH =====")

        print(
            f"Samples:     {len(lengths)}"
        )

        print(
            f"Min tokens:  {min(lengths)}"
        )

        print(
            f"Avg tokens:  "
            f"{sum(lengths) / len(lengths):.1f}"
        )

        print(
            f"Max tokens:  {max(lengths)}"
        )

        over_limit = [
            x
            for x in lengths
            if x > MAX_SEQ_LENGTH
        ]

        print(
            f"> {MAX_SEQ_LENGTH}: "
            f"{len(over_limit)} samples"
        )

    process_split(
        "train",
        train_samples
    )

    process_split(
        "valid",
        valid_samples
    )

    all_results.sort(
        key=lambda x: x["tokens"],
        reverse=True
    )

    print("\n===== Longest Samples =====")

    for item in all_results[:10]:

        print(
            f"{item['split']:5s} | "
            f"row={item['row']:3d} | "
            f"tokens={item['tokens']}"
        )

    return all_results[0]["tokens"]


# ============================================================
# 6. Run MLX-LM LoRA / QLoRA training
# ============================================================

def run_training(
    adapter_dir,
    iterations,
    steps_per_eval,
    save_every
):
    """
    Run MLX-LM training using the same Python interpreter
    currently used by PyCharm.
    """

    adapter_dir.mkdir(
        parents=True,
        exist_ok=True
    )

    command = [
        sys.executable,
        "-m",
        "mlx_lm",
        "lora",

        "--model",
        MODEL,

        "--train",

        "--data",
        str(DATA_DIR),

        "--adapter-path",
        str(adapter_dir),

        "--iters",
        str(iterations),

        "--batch-size",
        "1",

        "--learning-rate",
        "1e-5",

        "--num-layers",
        "8",

        "--max-seq-length",
        str(MAX_SEQ_LENGTH),

        "--steps-per-report",
        "5",

        "--steps-per-eval",
        str(steps_per_eval),

        "--val-batches",
        "-1",

        "--save-every",
        str(save_every),

        "--seed",
        str(RANDOM_SEED),
    ]

    print("\n===== Training Configuration =====")

    print(
        f"Python:          "
        f"{sys.executable}"
    )

    print(
        f"Model:           "
        f"{MODEL}"
    )

    print(
        f"Data directory:  "
        f"{DATA_DIR}"
    )

    print(
        f"Adapter output:  "
        f"{adapter_dir}"
    )

    print(
        f"Iterations:      "
        f"{iterations}"
    )

    print(
        f"Batch size:      "
        f"1"
    )

    print(
        f"Learning rate:   "
        f"1e-5"
    )

    print(
        f"LoRA layers:     "
        f"8"
    )

    print(
        f"Max seq length:  "
        f"{MAX_SEQ_LENGTH}"
    )

    print("\n===== Starting Training =====\n")

    subprocess.run(
        command,
        check=True
    )


# ============================================================
# 7. Main workflow
# ============================================================

if __name__ == "__main__":

    print("\n====================================")
    print("      Llama3 Statistical SFT")
    print("====================================")

    print("\nPython interpreter:")
    print(sys.executable)

    # Create output directory
    OUTPUT_DIR.mkdir(
        parents=True,
        exist_ok=True
    )

    # --------------------------------------------------------
    # Step 1: Check whether files exist
    # --------------------------------------------------------

    if not TRAIN_FILE.exists():
        raise FileNotFoundError(
            f"train.jsonl not found:\n{TRAIN_FILE}"
        )

    if not VALID_FILE.exists():
        raise FileNotFoundError(
            f"valid.jsonl not found:\n{VALID_FILE}"
        )

    print("\nDataset files found.")

    # --------------------------------------------------------
    # Step 2: Load datasets
    # --------------------------------------------------------

    train_samples = read_jsonl(
        TRAIN_FILE
    )

    valid_samples = read_jsonl(
        VALID_FILE
    )

    # --------------------------------------------------------
    # Step 3: Validate datasets
    # --------------------------------------------------------

    validate_dataset(
        train_samples,
        valid_samples
    )

    # --------------------------------------------------------
    # Step 4: Inspect token lengths
    # --------------------------------------------------------

    max_tokens = inspect_token_lengths(
        train_samples,
        valid_samples
    )

    print("\n===== Sequence Length Check =====")

    print(
        f"Maximum observed tokens: "
        f"{max_tokens}"
    )

    print(
        f"Configured max length:   "
        f"{MAX_SEQ_LENGTH}"
    )

    if max_tokens > MAX_SEQ_LENGTH:

        print(
            "\nSome samples exceed the configured "
            "maximum sequence length."
        )

        print(
            "Increase MAX_SEQ_LENGTH before training."
        )

        raise SystemExit(
            "Training stopped before smoke test."
        )

    print(
        "\nAll samples fit within "
        "MAX_SEQ_LENGTH."
    )

    # --------------------------------------------------------
    # Step 5: Smoke test
    # --------------------------------------------------------
    #
    # print("\n====================================")
    # print("          SMOKE TEST")
    # print("====================================")
    #
    # run_training(
    #     adapter_dir=SMOKE_ADAPTER_DIR,
    #     iterations=20,
    #     steps_per_eval=10,
    #     save_every=10
    # )
    #
    # print(
    #     "\nSmoke test completed successfully."
    # )

    # --------------------------------------------------------
    # Step 6: Formal training
    # --------------------------------------------------------

    if RUN_FORMAL_TRAINING:

        print("\n====================================")
        print("          FORMAL SFT")
        print("====================================")

        # run_training(
        #     adapter_dir=FINAL_ADAPTER_DIR,
        #     iterations=360,
        #     steps_per_eval=30,
        #     save_every=60
        # )
        run_training(
            adapter_dir=FINAL_ADAPTER_DIR,
            iterations=360,
            steps_per_eval=20,
            save_every=20
        )
        print(
            "\nFormal SFT completed successfully."
        )

    else:

        print("\n====================================")
        print("      FORMAL TRAINING DISABLED")
        print("====================================")

        print(
            "\nSmoke test finished."
        )

        print(
            "Check token lengths, train loss, "
            "validation loss and memory usage."
        )

        print(
            "\nIf everything looks normal, change:"
        )

        print(
            "RUN_FORMAL_TRAINING = False"
        )

        print(
            "to:"
        )

        print(
            "RUN_FORMAL_TRAINING = True"
        )

    # --------------------------------------------------------
    # Step 7: Test stage placeholder
    # --------------------------------------------------------

    print("\n====================================")
    print("           TEST STAGE")
    print("====================================")

    print(
        "Independent test-set evaluation "
        "will be added later."
    )