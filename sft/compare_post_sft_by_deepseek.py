import gc
import re
import time
from pathlib import Path

import pandas as pd
import mlx.core as mx

from mlx_lm import load, generate
from mlx_lm.sample_utils import make_sampler


# ============================================================
# 1. Configuration
# ============================================================

MODEL = "mlx-community/DeepSeek-R1-Distill-Qwen-7B-4bit"

TEST_FILE = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/test/"
    "test43_deepseek.xlsx"
)

OUTPUT_FILE = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/test/"
    "test43_deepseek_point204.xlsx"
)

ADAPTER_PATH = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/"
    "train200_17-3/deepseek_sft/best_adapter"
)

QUESTION_COL = "question_content"

MAX_TOKENS = 2048
TEMPERATURE = 0.0

RUN_BASE_MODEL = False
RUN_POST_SFT_MODEL = True


# ============================================================
# 2. Basic Cleaning
# ============================================================

def clean_special_tokens(text):
    if not text:
        return ""

    text = str(text)

    text = re.sub(
        r"<\|start_header_id\|>.*?<\|end_header_id\|>",
        "",
        text,
        flags=re.DOTALL
    )

    text = re.sub(
        r"<\|[^|]+\|>",
        "",
        text
    )

    text = text.replace(
        "<end_of_turn>",
        ""
    )

    return text.strip()


# ============================================================
# 3. Split DeepSeek Reasoning / Answer
# ============================================================

def split_think_and_answer(raw_output):
    """
    Supports:

    1.
    <think>
    reasoning
    </think>
    answer

    2. MLX commonly returns:
    reasoning
    </think>
    answer

    3.
    <think>
    unfinished reasoning...

    4.
    direct answer without think tags
    """

    text = clean_special_tokens(
        raw_output
    )

    if not text:
        return "", "", False, False

    has_open = "<think>" in text
    has_close = "</think>" in text

    # Closed reasoning block
    if has_close:

        before, after = text.rsplit(
            "</think>",
            1
        )

        before = before.replace(
            "<think>",
            "",
            1
        ).strip()

        return (
            before,         # think_content
            after.strip(),  # visible answer
            has_open,
            has_close
        )

    # Opened but never closed
    if has_open:

        think_content = text.split(
            "<think>",
            1
        )[1].strip()

        return (
            think_content,
            "",
            True,
            False
        )

    # No reasoning tags -> possibly direct answer
    return (
        "",
        text,
        False,
        False
    )


# ============================================================
# 4. Explicit Final Answer Extraction
# ============================================================

FINAL_PATTERNS = [

    # Final Answer: ...
    r"\bfinal\s+answer\s*[:\-]\s*([^\n]{1,400})",

    # Final Answer is ...
    r"\bfinal\s+answer\s+is\s*[:\-]?\s*([^\n]{1,400})",

    # The final answer is ...
    r"\bthe\s+final\s+answer\s+is\s*[:\-]?\s*([^\n]{1,400})",

    # The correct answer is ...
    r"\bthe\s+correct\s+answer\s+is\s*[:\-]?\s*([^\n]{1,400})",

    # The answer is ...
    r"\bthe\s+answer\s+is\s*[:\-]?\s*([^\n]{1,400})",

    # My answer is ...
    r"\bmy\s+answer\s+is\s*[:\-]?\s*([^\n]{1,400})",

    # Answer: ...
    r"\banswer\s*[:\-]\s*([^\n]{1,400})",

    # Therefore the answer/result/probability/value is ...
    r"\btherefore\s*,?\s*(?:the\s+)?"
    r"(?:answer|result|probability|value)"
    r"\s+is\s*[:\-]?\s*([^\n]{1,400})",

    # boxed result
    r"\\boxed\{([^{}]{1,400})\}",
]


def extract_explicit_answer(text):
    """
    Search all explicit answer expressions and use the LAST one,
    because DeepSeek may revise earlier results.
    """

    if not text:
        return ""

    candidates = []

    for pattern in FINAL_PATTERNS:

        for match in re.finditer(
            pattern,
            text,
            flags=re.IGNORECASE
        ):

            answer = match.group(1).strip()

            if answer:
                candidates.append(
                    (
                        match.start(),
                        answer
                    )
                )

    if not candidates:
        return ""

    candidates.sort(
        key=lambda x: x[0]
    )

    return candidates[-1][1]


# ============================================================
# 5. Detect Obviously Unfinished Generation
# ============================================================

def looks_unfinished(text):
    """
    Conservative check.

    Only mark as unfinished when there is clear evidence
    that generation stopped mid-thought.
    """

    if not text:
        return True

    text = text.strip()
    tail = text[-250:].lower()

    unfinished_endings = [

        "therefore,",
        "thus,",
        "hence,",
        "so,",

        "which gives",
        "which is",
        "equal to",

        "we get",
        "we obtain",

        "the answer is",
        "the result is",
        "the probability is",

        "the margin of",
        "the confidence interval is",
        "the lower bound is",
        "the upper bound is",
    ]

    if any(
        tail.endswith(x)
        for x in unfinished_endings
    ):
        return True

    # Ends with arithmetic/operator fragment
    if re.search(
        r"[+\-*/=,(]\s*$",
        tail
    ):
        return True

    # Dangling decimal
    if re.search(
        r"\d+\.\s*$",
        tail
    ):
        return True

    # Unclosed LaTeX
    if text.count(r"\[") > text.count(r"\]"):
        return True

    if text.count(r"\(") > text.count(r"\)"):
        return True

    return False


# ============================================================
# 6. Determine Whether Raw Output Is a Complete Direct Answer
# ============================================================

def looks_like_complete_direct_answer(text):
    """
    DeepSeek Post-SFT sometimes skips </think> entirely and
    directly gives a normal answer.

    Such outputs must NOT automatically become failed/extracted.
    """

    if not text:
        return False

    text = text.strip()

    # Tiny fragments are not enough
    if len(text.split()) < 10:
        return False

    # Explicit final answer strongly indicates completion
    if extract_explicit_answer(text):
        return True

    # Clearly unfinished -> not complete
    if looks_unfinished(text):
        return False

    # Normal prose/math ending
    if re.search(
        r"([.!?}\])]|%|\d)\s*$",
        text
    ):
        return True

    return False


# ============================================================
# 7. Robust DeepSeek Parser
# ============================================================

def parse_deepseek_output(raw_output):
    """
    Returns:

    think_content
    final_answer
    status

    status:
        complete
        extracted
        failed
    """

    raw_output = (
        str(raw_output).strip()
        if raw_output
        else ""
    )

    if not raw_output:
        return "", "", "failed"

    text = clean_special_tokens(
        raw_output
    )

    (
        think_content,
        visible_answer,
        has_open,
        has_close
    ) = split_think_and_answer(
        raw_output
    )

    # ========================================================
    # Case 1:
    # Proper closed </think> + final answer
    # ========================================================

    if has_close and visible_answer:

        return (
            think_content,
            visible_answer.strip(),
            "complete"
        )

    # ========================================================
    # Case 2:
    # No think tags -> model may simply answer directly
    # ========================================================

    if (
        not has_open
        and not has_close
        and looks_like_complete_direct_answer(text)
    ):

        return (
            "",
            text,
            "complete"
        )

    # ========================================================
    # Case 3:
    # Unfinished reasoning but an explicit final answer exists
    # ========================================================

    explicit_answer = extract_explicit_answer(
        text
    )

    if explicit_answer:

        return (
            think_content,
            explicit_answer.strip(),
            "extracted"
        )

    # ========================================================
    # Case 4:
    # Substantial direct answer without special wording
    #
    # Avoid false failure simply because it does not say
    # "Final Answer".
    # ========================================================

    if (
        not has_open
        and not has_close
        and len(text.split()) >= 15
        and not looks_unfinished(text)
    ):

        return (
            "",
            text,
            "complete"
        )

    # ========================================================
    # Otherwise genuinely unavailable / truncated
    # ========================================================

    return (
        think_content,
        "",
        "failed"
    )


# ============================================================
# 8. Generate One Answer
# ============================================================

def generate_answer(
    model,
    tokenizer,
    question
):

    messages = [
        {
            "role": "user",
            "content": str(
                question
            ).strip()
        }
    ]

    prompt = tokenizer.apply_chat_template(
        messages,
        tokenize=False,
        add_generation_prompt=True
    )

    raw_output = generate(
        model=model,
        tokenizer=tokenizer,
        prompt=prompt,
        max_tokens=MAX_TOKENS,
        sampler=make_sampler(
            temp=TEMPERATURE
        ),
        verbose=False
    ).strip()

    think_content, answer, status = (
        parse_deepseek_output(
            raw_output
        )
    )

    return (
        raw_output,
        think_content,
        answer,
        status
    )


# ============================================================
# 9. Load / Resume Dataset
# ============================================================

if OUTPUT_FILE.exists():

    print(
        "\nResuming existing output file..."
    )

    df = pd.read_excel(
        OUTPUT_FILE
    )

else:

    print(
        "\nStarting new DeepSeek test..."
    )

    # IMPORTANT:
    # Read the WHOLE original Excel.
    #
    # All original question fields are preserved.
    # We only append result columns later.
    df = pd.read_excel(
        TEST_FILE
    )


if QUESTION_COL not in df.columns:

    raise ValueError(
        f"Missing column: {QUESTION_COL}"
    )


print(
    f"Original dataset columns: "
    f"{len(df.columns)}"
)

print(
    list(df.columns)
)


# ============================================================
# 10. Add Result Columns
# ============================================================
#
# Existing question columns are NOT removed.
# These columns are only appended.
# ============================================================

text_columns = [

    # Base
    "base_raw_output",
    "base_think_content",
    "base_answer",
    "base_status",

    # Post-SFT
    "post_sft_raw_output",
    "post_sft_think_content",
    "post_sft_answer",
    "post_sft_status",
]


numeric_columns = [

    # Base
    "base_time_seconds",
    "base_word_count",

    # Post-SFT
    "post_sft_time_seconds",
    "post_sft_word_count",
]


for col in text_columns:

    if col not in df.columns:
        df[col] = ""

    df[col] = df[col].astype(
        "object"
    )


for col in numeric_columns:

    if col not in df.columns:
        df[col] = float("nan")


# ============================================================
# 11. Save
# ============================================================

def save():

    OUTPUT_FILE.parent.mkdir(
        parents=True,
        exist_ok=True
    )

    df.to_excel(
        OUTPUT_FILE,
        index=False
    )


# ============================================================
# 12. Run One Model
# ============================================================

def run_model(
    model,
    tokenizer,
    label,
    prefix
):

    print(
        f"\n===== {label} ====="
    )

    status_col = (
        f"{prefix}_status"
    )

    for i, row in df.iterrows():

        # ----------------------------------------------------
        # Resume by status
        # ----------------------------------------------------

        existing_status = (
            df.at[
                i,
                status_col
            ]
        )

        if (
            pd.notna(existing_status)
            and str(existing_status).strip()
            in {
                "complete",
                "extracted",
                "failed"
            }
        ):

            print(
                f"{label}: "
                f"{i + 1}/{len(df)} "
                f"- skipped "
                f"({existing_status})"
            )

            continue

        question = row[
            QUESTION_COL
        ]

        print(
            f"\n{label}: "
            f"{i + 1}/{len(df)}"
        )

        start = time.perf_counter()

        (
            raw_output,
            think_content,
            answer,
            status
        ) = generate_answer(
            model=model,
            tokenizer=tokenizer,
            question=question
        )

        elapsed = (
            time.perf_counter()
            - start
        )

        word_count = (
            len(answer.split())
            if answer
            else 0
        )

        # ----------------------------------------------------
        # Save everything
        # ----------------------------------------------------

        df.at[
            i,
            f"{prefix}_raw_output"
        ] = raw_output

        df.at[
            i,
            f"{prefix}_think_content"
        ] = think_content

        df.at[
            i,
            f"{prefix}_answer"
        ] = answer

        df.at[
            i,
            f"{prefix}_status"
        ] = status

        df.at[
            i,
            f"{prefix}_time_seconds"
        ] = round(
            elapsed,
            3
        )

        df.at[
            i,
            f"{prefix}_word_count"
        ] = word_count

        # Save after every question
        save()

        print(
            f"Status: {status} | "
            f"Time: {elapsed:.3f}s | "
            f"Answer words: {word_count} | "
            f"Think chars: {len(think_content)} | "
            f"Raw chars: {len(raw_output)} | "
            f"Saved"
        )


# ============================================================
# 13. Base Model
# ============================================================
#
# if RUN_BASE_MODEL:
#
#     print(
#         "\nLoading Base DeepSeek-R1-7B..."
#     )
#
#     base_model, base_tokenizer = load(
#         MODEL
#     )
#
#     print(
#         "Base DeepSeek-R1-7B loaded successfully."
#     )
#
#     run_model(
#         model=base_model,
#         tokenizer=base_tokenizer,
#         label="Pre-SFT",
#         prefix="base"
#     )
#
#     del base_model
#     del base_tokenizer
#
#     gc.collect()
#     mx.clear_cache()
#
#     print(
#         "\nBase DeepSeek-R1-7B released."
#     )


# ============================================================
# 14. Post-SFT Model
# ============================================================

if RUN_POST_SFT_MODEL:

    print(
        "\nLoading Post-SFT DeepSeek-R1-7B..."
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
        "Post-SFT DeepSeek-R1-7B "
        "loaded successfully."
    )

    run_model(
        model=post_model,
        tokenizer=post_tokenizer,
        label="Post-SFT",
        prefix="post_sft"
    )

    del post_model
    del post_tokenizer

    gc.collect()
    mx.clear_cache()

    print(
        "\nPost-SFT DeepSeek-R1-7B released."
    )


# ============================================================
# 15. Final Save
# ============================================================

save()


# ============================================================
# 16. Summary
# ============================================================

def print_summary(
    prefix,
    label
):

    status_col = (
        f"{prefix}_status"
    )

    time_col = (
        f"{prefix}_time_seconds"
    )

    word_col = (
        f"{prefix}_word_count"
    )

    print(
        f"\n===== {label} SUMMARY ====="
    )

    print(
        "\nStatus:"
    )

    print(
        df[
            status_col
        ].value_counts(
            dropna=False
        )
    )

    avg_time = pd.to_numeric(
        df[time_col],
        errors="coerce"
    ).mean()

    avg_words = pd.to_numeric(
        df[word_col],
        errors="coerce"
    ).mean()

    print(
        f"\nAverage time: "
        f"{avg_time:.3f}s"
    )

    print(
        f"Average answer words: "
        f"{avg_words:.1f}"
    )


print(
    "\n===== TEST COMPLETED ====="
)

print(
    f"Questions: {len(df)}"
)

print(
    f"Output: {OUTPUT_FILE}"
)


if RUN_BASE_MODEL:

    print_summary(
        prefix="base",
        label="PRE-SFT"
    )


if RUN_POST_SFT_MODEL:

    print_summary(
        prefix="post_sft",
        label="POST-SFT"
    )


print(
    "\nStatus definitions:"
)

print(
    "complete  = normal tagged answer "
    "or complete direct answer"
)

print(
    "extracted = unfinished reasoning, "
    "but explicit final answer recovered"
)

print(
    "failed    = no reliable final answer recovered"
)

print(
    "\n===== DONE ====="
)