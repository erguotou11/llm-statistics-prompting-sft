import sys
import json
import subprocess
from pathlib import Path

from mlx_lm import load


# ============================================================
# 1. Configuration
# ============================================================

MODEL = "mlx-community/DeepSeek-R1-Distill-Qwen-7B-4bit"

DATA_DIR = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/train200_17-3"
)

OUTPUT_DIR = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/train200_17-3/deepseek_sft"
)

TRAIN_FILE = DATA_DIR / "train_deepseek.jsonl"
VALID_FILE = DATA_DIR / "valid_deepseek.jsonl"

SMOKE_ADAPTER_DIR = OUTPUT_DIR / "adapters_smoke"
FINAL_ADAPTER_DIR = OUTPUT_DIR / "adapters"

RANDOM_SEED = 42
MAX_SEQ_LENGTH = 2048

# SFT Hyperparameters
LEARNING_RATE = "5e-6"  # Optimal learning rate for small-dataset LoRA 5e-6
BATCH_SIZE = "1" ##original is 1
GRAD_ACCUM_STEPS = "2" ##afterv2
LORA_LAYERS = "16"      # Extends LoRA coverage across 16 layers

# Set to True for formal training
RUN_FORMAL_TRAINING = True


# ============================================================
# 2. Read JSONL Dataset
# ============================================================

def read_jsonl(path):
    """Read and parse a JSONL file line by line."""
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
                    f"Invalid JSON in {path.name}, line {line_number}: {e}"
                )
            samples.append(sample)
    return samples


# ============================================================
# 3. Validate SFT Sample Format
# ============================================================

def validate_chat_sample(sample, file_name, row_number):
    """Validate standard OpenAI chat format (2 messages: user & assistant)."""
    if "messages" not in sample:
        raise ValueError(f"{file_name}, row {row_number}: missing 'messages'.")

    messages = sample["messages"]

    if not isinstance(messages, list) or len(messages) != 2:
        raise ValueError(
            f"{file_name}, row {row_number}: 'messages' must be a list of 2 items."
        )

    if messages[0].get("role") != "user" or messages[1].get("role") != "assistant":
        raise ValueError(
            f"{file_name}, row {row_number}: Roles must be ['user', 'assistant']."
        )

    question = str(messages[0].get("content", "")).strip()
    answer = str(messages[1].get("content", "")).strip()

    if not question or not answer:
        raise ValueError(
            f"{file_name}, row {row_number}: User prompt or assistant answer is empty."
        )


# ============================================================
# 4. Validate Datasets and Check Data Leakage
# ============================================================

def validate_dataset(train_samples, valid_samples):
    """Verify dataset structures, check internal duplicates and train-valid overlap."""
    for i, sample in enumerate(train_samples, start=1):
        validate_chat_sample(sample, "train.jsonl", i)

    for i, sample in enumerate(valid_samples, start=1):
        validate_chat_sample(sample, "valid.jsonl", i)

    train_questions = [s["messages"][0]["content"].strip() for s in train_samples]
    valid_questions = [s["messages"][0]["content"].strip() for s in valid_samples]

    train_unique = set(train_questions)
    valid_unique = set(valid_questions)

    train_duplicates = len(train_questions) - len(train_unique)
    valid_duplicates = len(valid_questions) - len(valid_unique)
    overlap = train_unique.intersection(valid_unique)

    print("\n===== Dataset Integrity Check =====")
    print(f"Training samples:        {len(train_samples)}")
    print(f"Validation samples:      {len(valid_samples)}")
    print(f"Train duplicates:        {train_duplicates}")
    print(f"Validation duplicates:   {valid_duplicates}")
    print(f"Train/Valid overlap:     {len(overlap)}")

    if train_duplicates > 0 or valid_duplicates > 0:
        raise ValueError("Duplicated questions detected within datasets.")

    if overlap:
        raise ValueError("Data leakage detected between train and validation sets.")


# ============================================================
# 5. Inspect Token Lengths
# ============================================================

def inspect_token_lengths(train_samples, valid_samples):
    """Analyze sequence lengths using the tokenizer's chat template."""
    print("\n===== Loading Model Tokenizer =====")
    print(f"Model: {MODEL}")

    _, tokenizer = load(MODEL, lazy=True)
    print("Tokenizer loaded successfully.")

    all_results = []

    def process_split(split_name, samples):
        lengths = []
        for row_number, sample in enumerate(samples, start=1):
            messages = sample["messages"]
            formatted_text = tokenizer.apply_chat_template(
                messages, tokenize=False, add_generation_prompt=False
            )
            token_ids = tokenizer.encode(formatted_text)
            token_count = len(token_ids)

            lengths.append(token_count)
            all_results.append({
                "split": split_name,
                "row": row_number,
                "tokens": token_count,
            })

        print(f"\n===== {split_name.upper()} TOKEN DISTRIBUTION =====")
        print(f"Samples:     {len(lengths)}")
        print(f"Min tokens:  {min(lengths)}")
        print(f"Avg tokens:  {sum(lengths) / len(lengths):.1f}")
        print(f"Max tokens:  {max(lengths)}")

        over_limit = [x for x in lengths if x > MAX_SEQ_LENGTH]
        print(f"> {MAX_SEQ_LENGTH} limit: {len(over_limit)} samples")

    process_split("train", train_samples)
    process_split("valid", valid_samples)

    all_results.sort(key=lambda x: x["tokens"], reverse=True)

    print("\n===== Longest Sequences (Top 5) =====")
    for item in all_results[:5]:
        print(f"{item['split']:5s} | Row={item['row']:3d} | Tokens={item['tokens']}")

    return all_results[0]["tokens"]


# ============================================================
# 6. Optimized MLX-LM Training Function
# ============================================================

def run_training(adapter_dir, iterations, steps_per_eval, save_every):
    """
    Run MLX-LM training with critical SFT flags:
    1. --mask-prompt: Ensures loss is computed on target (assistant) tokens only.
    2. --grad-checkpoint: Memory optimization for Apple Silicon.
    """
    adapter_dir.mkdir(parents=True, exist_ok=True)

    command = [
        sys.executable,
        "-m",
        "mlx_lm",
        "lora",
        "--model", MODEL,
        "--train",
        "--data", str(DATA_DIR),
        "--adapter-path", str(adapter_dir),
        "--iters", str(iterations),
        "--batch-size", BATCH_SIZE,
        "--grad-accumulation-steps", GRAD_ACCUM_STEPS,
        "--learning-rate", LEARNING_RATE,
        "--num-layers", LORA_LAYERS,
        "--max-seq-length", str(MAX_SEQ_LENGTH),
        "--steps-per-report", "5",
        "--steps-per-eval", str(steps_per_eval),
        "--val-batches", "-1",  # Run validation on all 20 validation samples
        "--save-every", str(save_every),
        "--seed", str(RANDOM_SEED),

        # --- Crucial Optimizations ---
        "--mask-prompt",      # Calculate loss on response tokens only
        "--grad-checkpoint",  # Gradient checkpointing for RAM/VRAM efficiency
    ]

    print("\n===== Training Configuration =====")
    print(f"Python Executable: {sys.executable}")
    print(f"Model Name:        {MODEL}")
    print(f"Data Directory:    {DATA_DIR}")
    print(f"Output Path:       {adapter_dir}")
    # print(f"Iterations:        {iterations} (approx. {iterations / 170:.1f} epochs)")
    effective_updates = (
            iterations / int(GRAD_ACCUM_STEPS)
    )

    print(f"Micro Steps:       {iterations}")
    print(f"Optimizer Updates: {effective_updates:.0f}")
    print(f"Batch Size:        {BATCH_SIZE}")
    print(f"Grad Accum Steps:  {GRAD_ACCUM_STEPS}")
    print(f"Learning Rate:     {LEARNING_RATE}")
    print(f"LoRA Layers:       {LORA_LAYERS}")
    print(f"Max Seq Length:    {MAX_SEQ_LENGTH}")
    print("Mask Prompt:       Enabled (True SFT)")
    print("Grad Checkpoint:   Enabled (VRAM Efficient)")

    print("\n===== Executing MLX SFT Training =====\n")
    subprocess.run(command, check=True)


# ============================================================
# 7. Pipeline Execution Workflow
# ============================================================

if __name__ == "__main__":

    print("\n====================================")
    print("      Llama-3 Statistical SFT Pipeline")
    print("====================================")

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Step 1: Check dataset existence
    if not TRAIN_FILE.exists() or not VALID_FILE.exists():
        raise FileNotFoundError("Missing train.jsonl or valid.jsonl in the data directory.")

    # Step 2: Load datasets
    train_samples = read_jsonl(TRAIN_FILE)
    valid_samples = read_jsonl(VALID_FILE)

    # Step 3: Validate structures and check for leakage
    validate_dataset(train_samples, valid_samples)

    # Step 4: Inspect token lengths
    max_tokens = inspect_token_lengths(train_samples, valid_samples)

    if max_tokens > MAX_SEQ_LENGTH:
        print(f"\n[Error] Maximum sequence length ({max_tokens}) exceeds MAX_SEQ_LENGTH ({MAX_SEQ_LENGTH}).")
        raise SystemExit("Pipeline aborted. Increase MAX_SEQ_LENGTH before running.")

    print("\nAll sequence lengths fit within MAX_SEQ_LENGTH limits.")

    # Step 5: Formal SFT Training Execution
    if RUN_FORMAL_TRAINING:
        print("\n====================================")
        print("          STARTING FORMAL SFT")
        print("====================================")

        # 360 iterations = exactly 2 Epochs for 180 training samples
        # Evaluate every 45 steps (0.25 Epoch), save checkpoint every 90 steps (0.5 Epoch)
        run_training(
            adapter_dir=FINAL_ADAPTER_DIR,
            iterations=320,
            steps_per_eval=20,
            save_every=20
        )
        print("\nFormal SFT pipeline completed successfully.")
    else:
        print("\nFormal training skipped (RUN_FORMAL_TRAINING = False).")