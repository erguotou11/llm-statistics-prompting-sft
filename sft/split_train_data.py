import json
import pandas as pd
from pathlib import Path
from sklearn.model_selection import train_test_split


# ============================================================
# 1. Configuration
# ============================================================

data_dir = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/train200_17-3"
)

files = [
    data_dir / "deepseek_sft_200.csv",
]

train_output = data_dir / "train_deepseek.jsonl"
valid_output = data_dir / "valid_deepseek.jsonl"

RANDOM_SEED = 42


# ============================================================
# 2. Load the three datasets
# ============================================================

dfs = []

for file in files:
    df = pd.read_csv(file)
    print(f"{file.name}: {len(df)} samples")
    dfs.append(df)


# ============================================================
# 3. Check column consistency
# ============================================================

base_columns = dfs[0].columns.tolist()

for file, df in zip(files[1:], dfs[1:]):
    if df.columns.tolist() != base_columns:
        raise ValueError(
            f"Column mismatch found in {file.name}\n"
            f"Expected: {base_columns}\n"
            f"Actual:   {df.columns.tolist()}"
        )


# ============================================================
# 4. Merge the datasets
# ============================================================

df = pd.concat(
    dfs,
    axis=0,
    ignore_index=True
)

print("\n===== Merged Dataset =====")
print(f"Total samples: {len(df)}")


# ============================================================
# 5. Validate required fields
# ============================================================

required_columns = [
    "question_content",
    "model_answer",
    "question_type",
    "knowledge",
    "hard_level",
]

missing_columns = [
    col for col in required_columns
    if col not in df.columns
]

if missing_columns:
    raise ValueError(
        f"Missing required columns: {missing_columns}"
    )


# ============================================================
# 6. Check missing values
# ============================================================

if df["question_content"].isna().any():
    raise ValueError(
        "Missing values found in question_content."
    )

if df["model_answer"].isna().any():
    raise ValueError(
        "Missing values found in model_answer."
    )


# ============================================================
# 7. Check duplicated questions
# ============================================================

duplicate_count = df.duplicated(
    subset=["question_content"]
).sum()

print(f"Unique questions: {df['question_content'].nunique()}")
print(f"Duplicated questions: {duplicate_count}")

if duplicate_count > 0:
    raise ValueError(
        "Duplicated questions were found. "
        "Please remove duplicates before SFT splitting."
    )


# ============================================================
# 8. Build primary stratification label
# ============================================================

# Primary strategy: Combine question type and difficulty level
df["stratify_label"] = (
    df["question_type"].astype(str)
    + " | "
    + df["hard_level"].astype(str)
)


# ============================================================
# 9. Validate stratum counts and handle small strata (< 2 samples)
# ============================================================

# Stratified split requires at least 2 samples per category in scikit-learn
stratum_counts = df["stratify_label"].value_counts()
small_strata = stratum_counts[stratum_counts < 2].index

if len(small_strata) > 0:
    print(
        f"\nWarning: {len(small_strata)} strata contain fewer than 2 samples:"
    )
    for stratum in small_strata:
        print(f"  - {stratum}")

    # Fallback Option A: Attempt stratification by 'question_type' only
    qt_counts = df["question_type"].value_counts()
    if (qt_counts >= 2).all():
        print("\n--> Falling back to stratification by 'question_type' only.")
        df["stratify_label"] = df["question_type"].astype(str)
    else:
        # Fallback Option B: Disable stratification if 'question_type' also has single-member classes
        print(
            "\n--> Cannot stratify safely without error. "
            "Falling back to unstratified random split."
        )
        df["stratify_label"] = None
else:
    print("\nAll strata have >= 2 samples. Stratification is safe.")


# ============================================================
# 10. Perform a 90/10 random split (160 Train / 40 Valid)
# ============================================================

train_df, valid_df = train_test_split(
    df,
    test_size=0.2,  # 15% validation set yields exactly 30 samples out of 200
    random_state=RANDOM_SEED,
    shuffle=True,
    stratify=df["stratify_label"] if df["stratify_label"] is not None else None,
)


# ============================================================
# 11. Verify the split integrity
# ============================================================

assert len(train_df) == 160, f"Expected 160 train samples, got {len(train_df)}"
assert len(valid_df) == 40, f"Expected 40 valid samples, got {len(valid_df)}"

train_questions = set(train_df["question_content"])
valid_questions = set(valid_df["question_content"])

overlap = train_questions.intersection(valid_questions)

if overlap:
    raise ValueError(
        f"Data leakage detected: "
        f"{len(overlap)} questions appear in both sets."
    )

print("\n===== Dataset Split =====")
print(f"Training samples:   {len(train_df)}")
print(f"Validation samples: {len(valid_df)}")
print(f"Overlap:            {len(overlap)}")


# ============================================================
# 12. Convert each sample to OpenAI chat-style SFT format
# ============================================================

def convert_to_messages(row):
    """
    Transforms a DataFrame row into standard OpenAI/HuggingFace SFT messages format.
    """
    return {
        "messages": [
            {
                "role": "user",
                "content": str(row["question_content"]).strip()
            },
            {
                "role": "assistant",
                "content": str(row["model_answer"]).strip()
            }
        ]
    }


# ============================================================
# 13. Save DataFrame to JSONL format
# ============================================================

def save_jsonl(dataframe, output_path):
    """
    Iterates through the DataFrame and writes each sample to a JSONL file.
    """
    with open(output_path, "w", encoding="utf-8") as f:
        for _, row in dataframe.iterrows():
            sample = convert_to_messages(row)
            f.write(json.dumps(sample, ensure_ascii=False) + "\n")


# ============================================================
# 14. Save train.jsonl and valid.jsonl
# ============================================================

save_jsonl(train_df, train_output)
save_jsonl(valid_df, valid_output)


# ============================================================
# 15. Display class distributions for verification
# ============================================================

print("\n===== Question Type Distribution =====")

print("\nTraining set:")
print(train_df["question_type"].value_counts().sort_index())

print("\nValidation set:")
print(valid_df["question_type"].value_counts().sort_index())


print("\n===== Difficulty Distribution =====")

print("\nTraining set:")
print(train_df["hard_level"].value_counts().sort_index())

print("\nValidation set:")
print(valid_df["hard_level"].value_counts().sort_index())


# ============================================================
# 16. Final execution output
# ============================================================

print("\n===== Process Completed =====")
print(f"Training dataset saved to:\n  {train_output}")
print(f"Validation dataset saved to:\n  {valid_output}")