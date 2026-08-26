import gc
import time
from pathlib import Path

import pandas as pd
import mlx.core as mx

from mlx_lm import load, generate
from mlx_lm.sample_utils import make_sampler


# ============================================================
# Configuration
# ============================================================

MODEL = "mlx-community/Qwen2.5-7B-Instruct-4bit"

TEST_FILE = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/test/test50_qwen.xlsx"
)

OUTPUT_FILE = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/test/"
    "test_50_qwen_base.xlsx"
)

ADAPTER_PATH = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/"
    "train200_17-3/qwen_sft/best_adapter"
)

QUESTION_COL = "question_content"

MAX_TOKENS = 1024
TEMPERATURE = 0.0

# Set to False if Base answers already exist
RUN_BASE_MODEL = True

# Set to False if you only want Base inference
RUN_POST_SFT_MODEL = False


# ============================================================
# Generate one answer
# ============================================================

def generate_answer(model, tokenizer, question):
    """
    Generate one response using the model's own chat template.
    """

    messages = [
        {
            "role": "user",
            "content": str(question).strip()
        }
    ]

    prompt = tokenizer.apply_chat_template(
        messages,
        tokenize=False,
        add_generation_prompt=True
    )

    response = generate(
        model=model,
        tokenizer=tokenizer,
        prompt=prompt,
        max_tokens=MAX_TOKENS,
        sampler=make_sampler(
            temp=TEMPERATURE
        ),
        verbose=False
    )

    return response.strip()


# ============================================================
# Load or resume test dataset
# ============================================================

if OUTPUT_FILE.exists():

    print("\nResuming existing output file...")

    df = pd.read_excel(
        OUTPUT_FILE
    )

else:

    print("\nStarting new test...")

    df = pd.read_excel(
        TEST_FILE
    )


if QUESTION_COL not in df.columns:
    raise ValueError(
        f"Missing column: {QUESTION_COL}"
    )


# ============================================================
# Initialize result columns
# ============================================================

result_columns = {
    "base_answer": "object",
    "base_time_seconds": "float64",
    "base_word_count": "float64",
    "post_sft_answer": "object",
    "post_sft_time_seconds": "float64",
    "post_sft_word_count": "float64",
}


for col, dtype in result_columns.items():

    if col not in df.columns:

        if dtype == "object":
            df[col] = pd.Series(
                "",
                index=df.index,
                dtype="object"
            )

        else:
            df[col] = pd.Series(
                float("nan"),
                index=df.index
            )


# Fix text dtype when resuming from Excel
df["base_answer"] = (
    df["base_answer"]
    .astype("object")
)

df["post_sft_answer"] = (
    df["post_sft_answer"]
    .astype("object")
)


# ============================================================
# Save progress
# ============================================================

def save():
    """
    Save current progress immediately.
    """

    df.to_excel(
        OUTPUT_FILE,
        index=False
    )


# ============================================================
# Run one model
# ============================================================

def run_model(
    model,
    tokenizer,
    label,
    answer_col,
    time_col,
    word_col
):
    """
    Run one model over all unanswered test questions.
    """

    print(
        f"\n===== {label} ====="
    )

    for i, row in df.iterrows():

        existing_answer = df.at[
            i,
            answer_col
        ]

        # Skip completed samples when resuming
        if (
            pd.notna(existing_answer)
            and str(existing_answer).strip()
        ):

            print(
                f"{label}: "
                f"{i + 1}/{len(df)} "
                f"- skipped"
            )

            continue

        question = row[
            QUESTION_COL
        ]

        print(
            f"\n{label}: "
            f"{i + 1}/{len(df)}"
        )

        start_time = (
            time.perf_counter()
        )

        answer = generate_answer(
            model=model,
            tokenizer=tokenizer,
            question=question
        )

        elapsed_time = (
            time.perf_counter()
            - start_time
        )

        word_count = len(
            answer.split()
        )

        df.at[
            i,
            answer_col
        ] = answer

        df.at[
            i,
            time_col
        ] = round(
            elapsed_time,
            3
        )

        df.at[
            i,
            word_col
        ] = word_count

        # Save immediately after each completed question
        save()

        print(
            f"Completed | "
            f"Time: {elapsed_time:.3f}s | "
            f"Words: {word_count} | "
            f"Saved"
        )


# ============================================================
# Base Qwen model
# ============================================================

if RUN_BASE_MODEL:

    print(
        "\nLoading Base Qwen2.5..."
    )

    base_model, base_tokenizer = load(
        MODEL
    )

    print(
        "Base Qwen2.5 loaded successfully."
    )

    run_model(
        model=base_model,
        tokenizer=base_tokenizer,
        label="Pre-SFT",
        answer_col="base_answer",
        time_col="base_time_seconds",
        word_col="base_word_count"
    )

    # Release Base model before loading Post-SFT
    del base_model
    del base_tokenizer

    gc.collect()
    mx.clear_cache()

    print(
        "\nBase Qwen2.5 released."
    )


# ============================================================
# Post-SFT Qwen model
# ============================================================

if RUN_POST_SFT_MODEL:

    print(
        "\nLoading Post-SFT Qwen2.5..."
    )

    print(
        f"Adapter path:\n"
        f"{ADAPTER_PATH}"
    )

    post_model, post_tokenizer = load(
        MODEL,
        adapter_path=str(
            ADAPTER_PATH
        )
    )

    print(
        "Post-SFT Qwen2.5 loaded successfully."
    )

    run_model(
        model=post_model,
        tokenizer=post_tokenizer,
        label="Post-SFT",
        answer_col="post_sft_answer",
        time_col="post_sft_time_seconds",
        word_col="post_sft_word_count"
    )


# ============================================================
# Final save and summary
# ============================================================

save()


print(
    "\n===== TEST COMPLETED ====="
)

print(
    f"Questions: {len(df)}"
)

print(
    f"Output: {OUTPUT_FILE}"
)


base_avg_time = pd.to_numeric(
    df["base_time_seconds"],
    errors="coerce"
).mean()

post_avg_time = pd.to_numeric(
    df["post_sft_time_seconds"],
    errors="coerce"
).mean()


base_avg_words = pd.to_numeric(
    df["base_word_count"],
    errors="coerce"
).mean()

post_avg_words = pd.to_numeric(
    df["post_sft_word_count"],
    errors="coerce"
).mean()


if RUN_BASE_MODEL:

    print(
        f"Average Pre-SFT time: "
        f"{base_avg_time:.3f}s"
    )

    print(
        f"Average Pre-SFT words: "
        f"{base_avg_words:.1f}"
    )


if RUN_POST_SFT_MODEL:

    print(
        f"Average Post-SFT time: "
        f"{post_avg_time:.3f}s"
    )

    print(
        f"Average Post-SFT words: "
        f"{post_avg_words:.1f}"
    )