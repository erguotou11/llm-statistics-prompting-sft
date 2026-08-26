import gc
import json
import re
import time
from pathlib import Path
from typing import Any, Dict, Tuple

import pandas as pd
from mlx_lm import generate, load



# ============================================================
# 1. File Paths
# ============================================================



DATASET_PATH = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/200/"
    "final dataset2/part4to5_final.csv"
)

OUTPUT_PATH = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/200/"
    "final dataset2/part4to5_llama_direct_results.csv"
)


# ============================================================
# 2. Model Settings
# ============================================================

MODELS = {
    "Llama3-8B": (
        "mlx-community/"
        "Meta-Llama-3-8B-Instruct-4bit"
    ),
}


MAX_TOKENS = 8192

# ============================================================
# 3. Shared Numerical Precision Rule
# ============================================================

#Before 4
# PRECISION_INSTRUCTION = """
# Numerical precision:
# - Do not round intermediate calculations prematurely.
# - If a z-score is required, keep full precision in all preceding calculations, then round the calculated z-score to 2 decimal places before using it to obtain a probability.
# - If a t-value is required, keep full precision in all preceding calculations, then round the calculated t-value to 3 decimal places before using it to obtain a probability.
# """.strip()

#CI RULE
# PRECISION_INSTRUCTION = """
# Numerical precision:
#
# - Do not round intermediate calculations prematurely.
# - When calculating a confidence interval, use a z critical value rounded to 2 decimal places or a t critical value rounded to 3 decimal places, as appropriate.
# - For confidence interval endpoints, round values with absolute value less than 1 to 4 decimal places; otherwise, round them to 2 decimal places.
#
# """.strip()
#
#AFTER 3
# PRECISION_INSTRUCTION = """
# Numerical precision:
#
# - Keep full precision during intermediate calculations.
# - For probability calculations, round the final z-score to 2 decimal places or t-value to 3 decimal places before obtaining the probability.
# - For confidence intervals, use z critical values to 2 decimal places or t critical values to 3 decimal places, as appropriate.
# - For confidence interval endpoints with absolute values below 1, round to a maximum of 4 decimal places and do not add unnecessary trailing zeros; otherwise, round to 2 decimal places.
# - Apply numerical reporting rules only to the final result and do not repeatedly re-check an already formatted result.
# - If uncertainty remains after reasonable verification, stop further re-analysis and report the most reasonable conclusion.
# """.strip()

#After 5: 6.Correlation
PRECISION_INSTRUCTION = """
Numerical precision:
- Round z to 2 d.p. and t to 3 d.p. before obtaining probabilities or using critical values in CIs (calculate SE first).
- P-values: Round to max 3 d.p. without trailing zeros (e.g., 0.030 → 0.03; report "p < 0.001" if p < 0.001; never write p = 0.000).
- CI Endpoints: Round to 2 d.p. if |x| ≥ 1; max 4 d.p. without trailing zeros if |x| < 1.
- Maintain full precision for all other intermediate calculations.
- Avoid repeated verification—if uncertainty remains, report the most reasonable conclusion.
""".strip()

#Correlation v2
# PRECISION_INSTRUCTION = """
# Numerical precision:
#
# - Follow any rounding requirement explicitly stated in the question.
# - For probability calculations, calculate the z- or t-statistic first, then round z to 2 decimal places or t to 3 decimal places before obtaining the probability.
# - For confidence intervals, calculate the standard error first, then use a z critical value rounded to 2 decimal places or a t critical value rounded to 3 decimal places, as appropriate.
# - P-values: Round to a maximum of 3 decimal places without unnecessary trailing zeros (for example, 0.030 → 0.03). If p < 0.001, report "p < 0.001"; never report p = 0.000.
# - Confidence interval endpoints: Round to 2 decimal places when |x| >= 1; when |x| < 1, round to a maximum of 4 decimal places without unnecessary trailing zeros.
# - For all other calculations, keep only enough precision to support the required final answer. Exact or symbolic intermediate forms may be retained when convenient, and unnecessary decimal expansion is not required.
# - Do not calculate additional decimal places once they cannot affect the required final rounding.
# - Once a value has been rounded according to a rule above, use that value in the next required step and do not return to the unrounded version to recompute the same result.
# - One reasonable numerical check is sufficient. Do not repeatedly recompute or verify an already consistent calculation.
# - If uncertainty remains after one reasonable verification, use the most internally consistent result and proceed to the final answer.
# """.strip()

# ============================================================
# 4. Question Construction
# ============================================================

def build_question_text(
    row: pd.Series,
) -> str:
    """
    Build the complete Direct prompt.

    Structure:
        Adapted Question
        + Options (if available)
        + Shared numerical precision rule

    No reasoning guidance, solution hints, or step-by-step
    instructions are added.
    """

    question = row.get(
        "Adapted Question"
    )

    if (
        pd.isna(question)
        or not str(question).strip()
    ):
        raise ValueError(
            f"Question No. {row.get('No')} "
            f"has no valid Adapted Question."
        )

    question_text = str(
        question
    ).strip()

    # --------------------------------------------------------
    # Append options when available.
    # --------------------------------------------------------

    options = row.get(
        "Options"
    )

    if (
        pd.notna(options)
        and str(options).strip()
    ):
        question_text += (
            "\n\nOptions:\n"
            + str(options).strip()
        )

    # --------------------------------------------------------
    # Append the shared precision rule.
    # --------------------------------------------------------

    question_text += (
        "\n\n"
        + PRECISION_INSTRUCTION
    )

    return question_text


# ============================================================
# 5. Special Token Cleaning
# ============================================================

def remove_special_tokens(
    text_value: str,
) -> str:
    """
    Remove model-template special tokens without changing
    statistical content.
    """

    if not text_value:
        return ""

    value = str(
        text_value
    )

    # Llama-style header tokens.
    value = re.sub(
        r"<\|start_header_id\|>.*?<\|end_header_id\|>",
        "",
        value,
        flags=re.DOTALL,
    )

    # Generic <|...|> tokens.
    value = re.sub(
        r"<\|[^|]+\|>",
        "",
        value,
    )

    # Gemma.
    value = value.replace(
        "<end_of_turn>",
        ""
    )

    return value.strip()


# ============================================================
# 6. DeepSeek Final Answer Extraction
# ============================================================

def extract_final_answer(
    text_value: str,
    model_name: str,
) -> Tuple[str, str]:
    """
    Extract the user-facing model answer.

    Returns:
        final_answer
        generation_status

    generation_status:
        complete
            Normal model output was produced.

        extracted
            DeepSeek did not close </think>, but an explicit
            answer statement was found before max_tokens.

        failed
            No usable answer could be extracted.

    Important:
        This function NEVER evaluates or corrects the model's
        statistical answer.
    """

    if not text_value:
        return "", "failed"

    value = str(
        text_value
    ).strip()

    # ========================================================
    # Standard instruct models
    # ========================================================

    if "deepseek" not in model_name.lower():

        return (
            value,
            "complete",
        )

    # ========================================================
    # DeepSeek normal case
    #
    # Everything after </think> is the normal user-facing
    # answer.
    # ========================================================

    if "</think>" in value:

        final_answer = value.rsplit(
            "</think>",
            1,
        )[1].strip()

        if final_answer:

            return (
                final_answer,
                "complete",
            )

    # ========================================================
    # DeepSeek max-token / reasoning-loop fallback
    #
    # Do NOT return the whole reasoning.
    #
    # Only search for explicit statements where the model
    # clearly states an answer.
    # ========================================================

    answer_patterns = [

        # ----------------------------------------------------
        # Final Answer: A. 0.0436
        # ----------------------------------------------------

        r"\bfinal\s+answer\s*[:\-]\s*"
        r"([^\n]{1,200})",

        # ----------------------------------------------------
        # The correct answer is A, 0.0436.
        # ----------------------------------------------------

        r"\bthe\s+correct\s+answer\s+is\s+"
        r"([^\n.]{1,200})",

        # ----------------------------------------------------
        # I think the answer is A, 0.0436.
        #
        # Put this BEFORE generic "the answer is" logically,
        # though matches are later sorted by position anyway.
        # ----------------------------------------------------

        r"\bI\s+(?:think|believe)\s+"
        r"(?:the\s+)?answer\s+is\s+"
        r"([^\n.]{1,200})",

        # ----------------------------------------------------
        # The answer is A, 0.0436.
        # ----------------------------------------------------

        r"\bthe\s+answer\s+is\s+"
        r"([^\n.]{1,200})",

        # ----------------------------------------------------
        # My answer is ...
        # ----------------------------------------------------

        r"\bmy\s+answer\s+is\s+"
        r"([^\n.]{1,200})",

        # ----------------------------------------------------
        # The probability is 0.1292.
        # ----------------------------------------------------

        r"\bthe\s+probability\s+is\s+"
        r"([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?%?)",

        # ----------------------------------------------------
        # The result is 0.1292.
        # ----------------------------------------------------

        r"\bthe\s+result\s+is\s+"
        r"([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?%?)",

        # ----------------------------------------------------
        # \boxed{0.1292}
        # ----------------------------------------------------

        r"\\boxed\{([^{}]+)\}",
    ]

    candidates = []

    for pattern in answer_patterns:

        for match in re.finditer(
            pattern,
            value,
            flags=re.IGNORECASE,
        ):

            answer_text = (
                match.group(1)
                .strip()
            )

            if answer_text:

                candidates.append(
                    (
                        match.start(),
                        answer_text,
                    )
                )

    # ========================================================
    # Use the latest explicit answer statement.
    #
    # DeepSeek may revise its answer during reasoning.
    # Therefore, the last explicit answer best represents
    # its final stated conclusion before truncation.
    # ========================================================

    if candidates:

        candidates.sort(
            key=lambda item: item[0]
        )

        final_answer = (
            candidates[-1][1]
            .strip()
        )

        return (
            final_answer,
            "extracted",
        )

    # ========================================================
    # No user-facing answer and no explicit answer statement.
    # ========================================================

    return "", "failed"


# ============================================================
# 7. Repetition Normalisation
# ============================================================

def normalise_for_repetition_check(
    text_value: str,
) -> str:
    """
    Normalise text for repetition comparison.
    """

    return re.sub(
        r"\s+",
        " ",
        str(text_value).lower(),
    ).strip()


# ============================================================
# 8. Repeated Paragraph Cleanup
# ============================================================

def truncate_consecutive_repeated_paragraphs(
    text_value: str,
) -> str:
    """
    Remove consecutive duplicate paragraphs.
    """

    if not text_value:
        return ""

    paragraphs = [
        paragraph.strip()
        for paragraph in re.split(
            r"\n\s*\n",
            str(text_value),
        )
        if paragraph.strip()
    ]

    if not paragraphs:

        return str(
            text_value
        ).strip()

    result = []

    for paragraph in paragraphs:

        if result:

            current = (
                normalise_for_repetition_check(
                    paragraph
                )
            )

            previous = (
                normalise_for_repetition_check(
                    result[-1]
                )
            )

            if current == previous:
                break

        result.append(
            paragraph
        )

    return "\n\n".join(
        result
    ).strip()


# ============================================================
# 9. Repeated Sentence Cleanup
# ============================================================

def truncate_repeated_sentence_run(
    text_value: str,
    repeat_threshold: int = 3,
) -> str:
    """
    Remove obvious consecutive sentence loops.
    """

    if not text_value:
        return ""

    value = str(
        text_value
    ).strip()

    sentences = re.split(
        r"(?<=[.!?])\s+",
        value,
    )

    if len(sentences) < repeat_threshold:
        return value

    output = []

    previous_normalised = None
    repeated_count = 0

    for sentence in sentences:

        sentence = sentence.strip()

        if not sentence:
            continue

        current_normalised = (
            normalise_for_repetition_check(
                sentence
            )
        )

        if (
            previous_normalised is not None
            and current_normalised
            == previous_normalised
        ):

            repeated_count += 1

        else:

            repeated_count = 1

        if (
            repeated_count
            >= repeat_threshold
        ):
            break

        output.append(
            sentence
        )

        previous_normalised = (
            current_normalised
        )

    return " ".join(
        output
    ).strip()


# ============================================================
# 10. Self-Dialogue Cleanup
# ============================================================

def truncate_self_dialogue_tail(
    text_value: str,
) -> str:
    """
    Remove obvious conversational tails after the answer.
    """

    if not text_value:
        return ""

    value = str(
        text_value
    )

    patterns = [
        r"\n\s*You're welcome\b",
        r"\n\s*You are welcome\b",
        r"\n\s*I'm glad I could help\b",
        r"\n\s*I am glad I could help\b",
        r"\n\s*I'm ready when you are\b",
        r"\n\s*I am ready when you are\b",
        r"\n\s*If you have any more question\b",
        r"\n\s*Feel free to ask\b",
        r"\n\s*Let me know if you have any other question\b",
    ]

    cut_positions = []

    for pattern in patterns:

        match = re.search(
            pattern,
            value,
            flags=re.IGNORECASE,
        )

        if match:

            cut_positions.append(
                match.start()
            )

    if cut_positions:

        value = value[
            :min(cut_positions)
        ]

    return value.strip()


# ============================================================
# 11. Model Answer Cleaning
# ============================================================

def clean_model_answer(
    text_value: str,
    model_name: str,
) -> Tuple[str, str]:
    """
    Clean generation artifacts without correcting statistical
    content.

    Returns:
        final_answer
        generation_status
    """

    raw_cleaned = (
        remove_special_tokens(
            text_value
        )
    )

    final_answer, generation_status = (
        extract_final_answer(
            raw_cleaned,
            model_name=model_name,
        )
    )

    if not final_answer.strip():

        return (
            "",
            "failed",
        )

    final_answer = (
        truncate_consecutive_repeated_paragraphs(
            final_answer
        )
    )

    final_answer = (
        truncate_repeated_sentence_run(
            final_answer
        )
    )

    final_answer = (
        truncate_self_dialogue_tail(
            final_answer
        )
    )

    final_answer = (
        final_answer.strip()
    )

    if not final_answer:

        return (
            "",
            "failed",
        )

    return (
        final_answer,
        generation_status,
    )


# ============================================================
# 12. Word Count
# ============================================================

def count_words(
    text_value: str,
) -> int:
    """
    Count word-like tokens in the final model answer.

    This function was missing in the previous script and caused:
        NameError: name 'count_words' is not defined
    """

    if not text_value:
        return 0

    return len(
        re.findall(
            r"\b\w+\b",
            str(text_value),
        )
    )


# ============================================================
# 13. Prompt Formatting
# ============================================================

def build_formatted_prompt(
    tokenizer: Any,
    question_text: str,
) -> Tuple[str, list]:
    """
    Apply each model's native chat template.

    All models receive exactly one user message.
    No model-specific reasoning instruction is injected.
    """

    messages = [
        {
            "role":
                "user",

            "content":
                question_text.strip(),
        }
    ]

    formatted_prompt = (
        tokenizer.apply_chat_template(
            messages,
            tokenize=False,
            add_generation_prompt=True,
        )
    )

    return (
        formatted_prompt,
        messages,
    )


# ============================================================
# 14. Core Direct Workflow
# ============================================================

def run_direct_question(
    row: pd.Series,
    model: Any,
    tokenizer: Any,
    model_name: str,
) -> Dict[str, Any]:
    """
    Run one Direct prompting question.
    """

    question_text = (
        build_question_text(
            row
        )
    )

    formatted_prompt, messages = (
        build_formatted_prompt(
            tokenizer=tokenizer,
            question_text=question_text,
        )
    )

    generation_start = (
        time.time()
    )

    raw_assistant_output = (
        generate(
            model=model,
            tokenizer=tokenizer,
            prompt=formatted_prompt,
            max_tokens=MAX_TOKENS,
            verbose=False,
        ).strip()
    )

    generation_elapsed = (
        time.time()
        - generation_start
    )

    print(
        f"[No.{row.get('No')}] "
        f"Generation: "
        f"{generation_elapsed:.2f}s | "
        f"Prompt chars: "
        f"{len(formatted_prompt)} | "
        f"Output chars: "
        f"{len(raw_assistant_output)}"
    )

    if not raw_assistant_output:

        raise ValueError(
            "Model produced an empty response."
        )

    # ========================================================
    # IMPORTANT:
    #
    # clean_model_answer() already returns generation_status.
    #
    # Do NOT treat the second return value as a Boolean.
    # ========================================================

    final_answer, generation_status = (
        clean_model_answer(
            raw_assistant_output,
            model_name=model_name,
        )
    )

    messages_with_answer = (
        messages
        + [
            {
                "role":
                    "assistant",

                "content":
                    raw_assistant_output,
            }
        ]
    )

    return {
        "question_text":
            question_text,

        "formatted_prompt":
            formatted_prompt,

        "messages":
            messages_with_answer,

        "raw_turn_outputs":
            [
                raw_assistant_output
            ],

        "final_answer":
            final_answer,

        "generation_status":
            generation_status,
    }


# ============================================================
# 15. Build Output Row
# ============================================================

def build_output_row(
    row: pd.Series,
    result: Dict[str, Any],
    model_key: str,
    model_path: str,
    elapsed: float,
) -> Dict[str, Any]:
    """
    Build one successful model execution record.
    """

    clean_answer = (
        result[
            "final_answer"
        ]
    )

    clean_prompt = (
        remove_special_tokens(
            result.get(
                "formatted_prompt",
                "",
            )
        )
    )

    word_count = (
        count_words(
            clean_answer
        )
    )

    generation_status = (
        result[
            "generation_status"
        ]
    )

    return {
        # ====================================================
        # Question Metadata
        # ====================================================

        "No":
            row.get(
                "No"
            ),

        "Source ID":
            row.get(
                "Source ID",
                "",
            ),

        "Knowledge Domain":
            row.get(
                "Knowledge Domain",
                row.get(
                    "Knowledge Area",
                    "",
                ),
            ),

        "Scenario":
            row.get(
                "Scenario",
                "",
            ),

        "Difficulty":
            row.get(
                "Difficulty",
                "",
            ),

        "Question Type":
            row.get(
                "Question Type",
                "",
            ),

        "Adapted Question":
            row.get(
                "Adapted Question",
                "",
            ),

        "Options":
            row.get(
                "Options",
                "",
            ),

        "Solution / Key Step":
            row.get(
                "Solution / Key Step",
                row.get(
                    "Solution / Key Steps",
                    "",
                ),
            ),

        # ====================================================
        # Model Metadata
        # ====================================================

        "model_name":
            model_key,

        "model_id":
            model_path,

        "prompt_structure":
            "Direct",

        # ====================================================
        # Gold Answer
        # ====================================================

        "Correct Answer":
            row.get(
                "Correct Answer",
                "",
            ),

        # ====================================================
        # Prompt / Model Output
        # ====================================================

        "full_prompt":
            clean_prompt,

        "model_answer":
            clean_answer,

        "output_words_cnt":
            word_count,

        "generation_status":
            generation_status,

        # ====================================================
        # Runtime
        # ====================================================

        "max_tokens":
            MAX_TOKENS,

        "elapsed_seconds":
            round(
                elapsed,
                3,
            ),

        # ====================================================
        # complete:
        #     normal model answer
        #
        # extracted:
        #     DeepSeek explicit answer recovered from a
        #     max-token reasoning loop
        #
        # both are valid experimental outputs.
        # ====================================================

        "act_status":
            (
                "success"
                if generation_status in {
                    "complete",
                    "extracted",
                }
                else "failed"
            ),

        "error_message":
            "",

        # ====================================================
        # Raw Generation / Conversation
        # ====================================================

        "raw_turn_outputs":
            json.dumps(
                result[
                    "raw_turn_outputs"
                ],
                ensure_ascii=False,
            ),

        "messages_json":
            json.dumps(
                result[
                    "messages"
                ],
                ensure_ascii=False,
            ),
    }


# ============================================================
# 16. Build Error Row
# ============================================================

def build_error_row(
    row: pd.Series,
    model_key: str,
    model_path: str,
    elapsed: float,
    error: Exception,
) -> Dict[str, Any]:
    """
    Build a row for a genuine execution failure.
    """

    return {
        "No":
            row.get(
                "No"
            ),

        "Source ID":
            row.get(
                "Source ID",
                "",
            ),

        "Knowledge Domain":
            row.get(
                "Knowledge Domain",
                row.get(
                    "Knowledge Area",
                    "",
                ),
            ),

        "Scenario":
            row.get(
                "Scenario",
                "",
            ),

        "Difficulty":
            row.get(
                "Difficulty",
                "",
            ),

        "Question Type":
            row.get(
                "Question Type",
                "",
            ),

        "Adapted Question":
            row.get(
                "Adapted Question",
                "",
            ),

        "Options":
            row.get(
                "Options",
                "",
            ),

        "Solution / Key Step":
            row.get(
                "Solution / Key Step",
                row.get(
                    "Solution / Key Steps",
                    "",
                ),
            ),

        "model_name":
            model_key,

        "model_id":
            model_path,

        "prompt_structure":
            "Direct",

        "Correct Answer":
            row.get(
                "Correct Answer",
                "",
            ),

        "full_prompt":
            "",

        "model_answer":
            "",

        "output_words_cnt":
            0,

        "generation_status":
            "failed",

        "max_tokens":
            MAX_TOKENS,

        "elapsed_seconds":
            round(
                elapsed,
                3,
            ),

        "act_status":
            "failed",

        "error_message":
            str(
                error
            ),

        "raw_turn_outputs":
            "[]",

        "messages_json":
            "[]",
    }


# ============================================================
# 17. Main Execution Loop
# ============================================================

if __name__ == "__main__":

    print(
        ">>> Loading dataset..."
    )

    if not DATASET_PATH.exists():

        raise FileNotFoundError(
            f"Dataset path does not exist: "
            f"{DATASET_PATH}"
        )

    df = pd.read_csv(
        DATASET_PATH
    )

    OUTPUT_PATH.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    print(
        f">>> Dataset loaded: "
        f"{len(df)} question"
    )

    print(
        ">>> Prompt strategy: Direct"
    )

    print(
        ">>> System prompt: NONE"
    )

    print(
        ">>> Reasoning instructions: NONE"
    )

    print(
        ">>> Intermediate calculations: full precision"
    )

    print(
        ">>> Z-score precision: 2 decimals"
    )

    print(
        ">>> t-value precision: 3 decimals"
    )

    print(
        ">>> External lookup: DISABLED"
    )

    print(
        ">>> DeepSeek normal reasoning: "
        "removed before model_answer"
    )

    print(
        ">>> DeepSeek reasoning-loop fallback: "
        "explicit answer extraction only"
    )

    # ========================================================
    # Model Loop
    # ========================================================

    for (
        model_key,
        model_path,
    ) in MODELS.items():

        print(
            "\n"
            "============================================================"
        )

        print(
            f">>> Loading Model: "
            f"{model_key} "
            f"({model_path})"
        )

        print(
            "============================================================"
        )

        model, tokenizer = (
            load(
                model_path
            )
        )

        # Fix Llama 3 stop tokens
        if "llama" in model_key.lower():
            eot_id = tokenizer.convert_tokens_to_ids(
                "<|eot_id|>"
            )

            end_of_text_id = tokenizer.convert_tokens_to_ids(
                "<|end_of_text|>"
            )

            tokenizer.eos_token_ids = {
                end_of_text_id,
                eot_id,
            }

        print(
            f">>> Starting Direct evaluation "
            f"for {model_key}..."
        )

        # ====================================================
        # Question Loop
        # ====================================================

        for idx, row in df.iterrows():

            question_id = (
                row.get(
                    "No"
                )
            )
            question_type = (
                row.get(
                    "Question Type",
                    ""
                )
            )

            print(
                f"\n[{model_key}] "
                f"Question "
                f"{idx + 1}/{len(df)} "
                f"(No: {question_id})"
                f"| Type: {question_type}"
            )

            start_time = (
                time.time()
            )

            try:

                result = (
                    run_direct_question(
                        row=row,
                        model=model,
                        tokenizer=tokenizer,
                        model_name=model_key,
                    )
                )

                elapsed = (
                    time.time()
                    - start_time
                )

                row_result = (
                    build_output_row(
                        row=row,
                        result=result,
                        model_key=model_key,
                        model_path=model_path,
                        elapsed=elapsed,
                    )
                )

                print(
                    f"STATUS: "
                    f"[{model_key}] "
                    f"Question No "
                    f"{question_id} | "
                    f"{result['generation_status']} | "
                    f"Elapsed: "
                    f"{elapsed:.2f}s | "
                    f"Words: "
                    f"{row_result['output_words_cnt']}"
                )

            except Exception as error:

                elapsed = (
                    time.time()
                    - start_time
                )

                print(
                    f"ERROR: "
                    f"[{model_key}] "
                    f"Question No "
                    f"{question_id}: "
                    f"| Type: {question_type}"
                    f"{error}"
                )

                row_result = (
                    build_error_row(
                        row=row,
                        model_key=model_key,
                        model_path=model_path,
                        elapsed=elapsed,
                        error=error,
                    )
                )

            # =================================================
            # Save Immediately
            # =================================================

            is_file_empty = (
                not OUTPUT_PATH.exists()
                or OUTPUT_PATH.stat().st_size == 0
            )

            single_df = pd.DataFrame(
                [row_result]
            )

            single_df.to_csv(
                OUTPUT_PATH,

                mode=(
                    "w"
                    if is_file_empty
                    else "a"
                ),

                header=is_file_empty,

                index=False,

                encoding="utf-8-sig",
            )

            print(
                f"Saved Question No "
                f"{question_id} "
                f"({model_key}) to CSV."
            )

        # ====================================================
        # Release Model Memory
        # ====================================================

        del model
        del tokenizer

        gc.collect()

        print(
            f">>> Released model: "
            f"{model_key}"
        )

    print(
        "\n"
        "============================================================"
    )

    print(
        "Experiment execution complete."
    )

    print(
        f"Output saved to:\n"
        f"{OUTPUT_PATH}"
    )

    print(
        "============================================================"
    )