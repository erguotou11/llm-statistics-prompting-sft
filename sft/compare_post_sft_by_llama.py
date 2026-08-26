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

BASE_MODEL = "mlx-community/Meta-Llama-3-8B-Instruct-4bit"
#MODEL = "mlx-community/Qwen2.5-7B-Instruct-4bit"

TEST_FILE = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/test/llama_50.xlsx"
)

OUTPUT_FILE = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/test/"
    "test_50_llama_pre_post_point170.xlsx"
)

ADAPTER_PATH = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/"
    "train200_17-3/llama3_sft/best_adapter"
)

QUESTION_COL = "question_content"

MAX_TOKENS = 1024
TEMPERATURE = 0.0


# ============================================================
# Generate one answer
# ============================================================

def generate_answer(model, tokenizer, question):

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
        model,
        tokenizer,
        prompt=prompt,
        max_tokens=MAX_TOKENS,
        sampler=make_sampler(temp=TEMPERATURE),
        verbose=False
    )

    # Remove repeated content after the first end-of-turn token
    response = response.split("<|eot_id|>", 1)[0]

    for token in [
        "<|start_header_id|>",
        "<|end_header_id|>",
        "<|begin_of_text|>",
        "<|end_of_text|>"
    ]:
        response = response.replace(token, "")

    return response.strip()


# ============================================================
# Load or resume dataset
# ============================================================

if OUTPUT_FILE.exists():
    print("Resuming existing output file...")
    df = pd.read_excel(OUTPUT_FILE)
else:
    print("Starting new test...")
    df = pd.read_excel(TEST_FILE)


if QUESTION_COL not in df.columns:
    raise ValueError(f"Missing column: {QUESTION_COL}")


# ============================================================
# Initialize result columns
# ============================================================

columns = {
    "base_answer": "object",
    "base_time_seconds": "float64",
    "base_word_count": "float64",
    "post_sft_answer": "object",
    "post_sft_time_seconds": "float64",
    "post_sft_word_count": "float64",
}

for col, dtype in columns.items():

    if col not in df.columns:
        if dtype == "object":
            df[col] = pd.Series("", index=df.index, dtype="object")
        else:
            df[col] = pd.Series(float("nan"), index=df.index)

# Fix dtype when resuming from Excel
df["base_answer"] = df["base_answer"].astype("object")
df["post_sft_answer"] = df["post_sft_answer"].astype("object")


def save():
    df.to_excel(
        OUTPUT_FILE,
        index=False
    )


# ============================================================
# Run one model over the dataset
# ============================================================

def run_model(
    model,
    tokenizer,
    label,
    answer_col,
    time_col,
    word_col
):

    print(f"\n===== {label} =====")

    for i, row in df.iterrows():

        existing = df.at[i, answer_col]

        if pd.notna(existing) and str(existing).strip():
            print(f"{label}: {i + 1}/{len(df)} - skipped")
            continue

        print(f"{label}: {i + 1}/{len(df)}")

        start = time.perf_counter()

        answer = generate_answer(
            model,
            tokenizer,
            row[QUESTION_COL]
        )

        elapsed = time.perf_counter() - start
        words = len(answer.split())

        df.at[i, answer_col] = answer
        df.at[i, time_col] = round(elapsed, 3)
        df.at[i, word_col] = words

        # Save immediately after every question
        save()

        print(
            f"Completed | "
            f"Time: {elapsed:.3f}s | "
            f"Words: {words} | Saved"
        )


# ============================================================
# Base model
# ============================================================

print("\nLoading Base Llama3...")

base_model, base_tokenizer = load(
    BASE_MODEL
)

run_model(
    base_model,
    base_tokenizer,
    "Pre-SFT",
    "base_answer",
    "base_time_seconds",
    "base_word_count"
)


# ============================================================
# Release base model
# ============================================================

del base_model
del base_tokenizer

gc.collect()
mx.clear_cache()


# ============================================================
# Post-SFT model
# ============================================================

print("\nLoading Post-SFT Llama3...")

post_model, post_tokenizer = load(
    BASE_MODEL,
    adapter_path=str(ADAPTER_PATH)
)

run_model(
    post_model,
    post_tokenizer,
    "Post-SFT",
    "post_sft_answer",
    "post_sft_time_seconds",
    "post_sft_word_count"
)


# ============================================================
# Final summary
# ============================================================

save()

print("\n===== TEST COMPLETED =====")
print(f"Questions: {len(df)}")
print(f"Output: {OUTPUT_FILE}")

print(
    f"Average Pre-SFT time: "
    f"{pd.to_numeric(df['base_time_seconds'], errors='coerce').mean():.3f}s"
)

print(
    f"Average Post-SFT time: "
    f"{pd.to_numeric(df['post_sft_time_seconds'], errors='coerce').mean():.3f}s"
)

print(
    f"Average Pre-SFT words: "
    f"{pd.to_numeric(df['base_word_count'], errors='coerce').mean():.1f}"
)

print(
    f"Average Post-SFT words: "
    f"{pd.to_numeric(df['post_sft_word_count'], errors='coerce').mean():.1f}"
)