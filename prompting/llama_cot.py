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
    "final dataset2/part4to5_llama_cot_results.csv"
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


# Common generation limit for all models.
MAX_TOKENS = 8192

# ============================================================
# 3. Shared Numerical Precision Rule
# ============================================================
#
# PRECISION_INSTRUCTION = """
# Numerical precision:
# - Do not round intermediate calculations prematurely.
# - If a z-score is required, keep full precision in all preceding calculations, then round the calculated z-score to 2 decimal places before using it to obtain a probability.
# - If a t-value is required, keep full precision in all preceding calculations, then round the calculated t-value to 3 decimal places before using it to obtain a probability.
# """.strip()

#CI RULE
# PRECISION_INSTRUCTION = """
# Numerical precision:

# - Do not round intermediate calculations prematurely.
# - When calculating a confidence interval, use a z critical value rounded to 2 decimal places or a t critical value rounded to 3 decimal places, as appropriate.
# - Round final confidence interval endpoints to 2 decimal places unless the question specifies otherwise.
# """.strip()



#Between 4 and 5 using
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


#6.Correlation and after
PRECISION_INSTRUCTION = """
Numerical precision:
- Round z to 2 d.p. and t to 3 d.p. before obtaining probabilities or using critical values in CIs (calculate SE first).
- P-values: Round to max 3 d.p. without trailing zeros (e.g., 0.030 → 0.03; report "p < 0.001" if p < 0.001; never write p = 0.000).
- CI Endpoints: Round to 2 d.p. if |x| ≥ 1; max 4 d.p. without trailing zeros if |x| < 1.
- Maintain full precision for all other intermediate calculations.
- Avoid repeated verification—if uncertainty remains, report the most reasonable conclusion.
""".strip()

#General Rule
# PRECISION_INSTRUCTION = """
# Numerical precision:
#
# - Keep full precision in intermediate calculations.
# - For probability calculations, round the final z-score to 2 decimal places or t-value to 3 decimal places before obtaining the probability.
# - For confidence intervals, use z critical values rounded to 2 decimal places or t critical values rounded to 3 decimal places.
# - For confidence interval endpoints, round values with absolute value less than 1 to 4 decimal places; otherwise, round them to 2 decimal places.
# - For other numerical answers, follow the precision specified in the question; if none is specified, round the final numerical answer to 4 decimal places.
# """.strip()


# ============================================================
# 4. Build Structured CoT Prompt
# ============================================================

def build_prompt(
    row: pd.Series,
) -> str:
    """
    Build the Structured Chain-of-Thought prompt.

    Primary source:
        prompt_cot

    Fallback:
        Adapted Question
        + Options
        + cot_reasoning_scaffold

    The same numerical precision rule is applied to all models.
    """

    saved_prompt = row.get(
        "prompt_cot"
    )

    # --------------------------------------------------------
    # Use existing prompt_cot when available.
    # --------------------------------------------------------

    if (
        pd.notna(saved_prompt)
        and str(saved_prompt).strip()
    ):

        prompt = str(
            saved_prompt
        ).strip()

    # --------------------------------------------------------
    # Otherwise construct the CoT prompt.
    # --------------------------------------------------------

    else:

        question = row.get(
            "Adapted Question"
        )

        if (
            pd.isna(question)
            or not str(question).strip()
        ):

            raise ValueError(
                f"Question No. {row.get('No')} "
                f"has no valid question."
            )

        prompt = str(
            question
        ).strip()

        # ----------------------------------------------------
        # Add options when available.
        # ----------------------------------------------------

        options = row.get(
            "Options"
        )

        if (
            pd.notna(options)
            and str(options).strip()
        ):

            prompt += (
                "\n\nOptions:\n"
                f"{str(options).strip()}"
            )

        # ----------------------------------------------------
        # Add task-specific reasoning scaffold.
        # ----------------------------------------------------

        scaffold = row.get(
            "cot_reasoning_scaffold"
        )

        if (
            pd.isna(scaffold)
            or not str(scaffold).strip()
        ):

            raise ValueError(
                f"Question No. {row.get('No')} "
                f"has no CoT reasoning scaffold."
            )

        prompt += (
            "\n\nFollow the reasoning stages below:\n\n"
            f"{str(scaffold).strip()}\n\n"
            "Show all necessary calculations and then provide "
            "the final answer."
        )

    # --------------------------------------------------------
    # Apply shared precision rule.
    # --------------------------------------------------------

    prompt += (
        "\n\n"
        + PRECISION_INSTRUCTION
    )

    return prompt


# ============================================================
# 5. Special Token Cleaning
# ============================================================

def remove_special_tokens(
    text_value: str,
) -> str:
    """
    Remove chat-template artifacts without changing
    the statistical content.
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

    # Gemma token.
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
) -> Tuple[str, bool]:
    """
    Extract the user-facing final answer.

    DeepSeek-R1:
    - If </think> exists, discard everything before it.
    - Keep only the formal answer after </think>.
    - If </think> is missing, attempt to preserve structured
      answer content from the generated response.

    Gemma and Qwen:
    - Preserve the generated response.
    """

    if not text_value:
        return "", False

    value = str(
        text_value
    ).strip()

    # --------------------------------------------------------
    # DeepSeek-R1
    # --------------------------------------------------------

    if "deepseek" in model_name.lower():

        # ----------------------------------------------------
        # Normal case: reasoning block is closed.
        # ----------------------------------------------------

        if "</think>" in value:

            final_answer = value.rsplit(
                "</think>",
                1,
            )[1].strip()

            if final_answer:
                return final_answer, True

        # ----------------------------------------------------
        # Missing </think>: try to locate structured answer.
        # ----------------------------------------------------

        step_match = re.search(
            r"(\d+\.\s*\*\*.*|Step\s*\d+:.*)",
            value,
            flags=re.DOTALL | re.IGNORECASE,
        )

        if step_match:

            extracted = (
                step_match.group(1).strip()
            )

            oral_patterns = [
                r"^(?:[O|o]kay,?\s*)",
                r"^(?:[A|a]lright,?\s*)",
                r"^(?:[H|h]mm,?\s*)",
                r"^(?:[L|l]et's\s+.*?[\.\n])",
            ]

            for pattern in oral_patterns:

                extracted = re.sub(
                    pattern,
                    "",
                    extracted,
                    flags=re.IGNORECASE,
                ).strip()

            if extracted:
                return extracted, True

        # ----------------------------------------------------
        # Remove an explicit opening <think> token.
        # ----------------------------------------------------

        cleaned_text = re.sub(
            r"^<think>",
            "",
            value,
            flags=re.IGNORECASE,
        ).strip()

        cleaned_text = re.sub(
            r"^(?:[O|o]kay|[A|a]lright|[H|h]mm),?\s*",
            "",
            cleaned_text,
        ).strip()

        if cleaned_text:
            return cleaned_text, True

        # ----------------------------------------------------
        # Emergency fallback.
        # ----------------------------------------------------

        lines = [
            line.strip()
            for line in value.split("\n")
            if line.strip()
        ]

        if lines:

            fallback_answer = "\n".join(
                lines[-3:]
            )

            return fallback_answer, False

        return "", False

    # --------------------------------------------------------
    # Standard instruct models
    # --------------------------------------------------------

    return value, True


# ============================================================
# 7. Repetition Normalisation
# ============================================================

def normalise_for_repetition_check(
    text_value: str,
) -> str:
    """
    Normalise whitespace and case for repetition detection.
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
    Stop exact consecutive repeated paragraphs.
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
    Stop obvious consecutive sentence repetition.
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
    Remove obvious conversational tails after the
    statistical answer has already finished.
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
# 11. Final Model Answer Cleaning
# ============================================================

def clean_model_answer(
    text_value: str,
    model_name: str,
) -> Tuple[str, bool]:
    """
    Produce the final user-facing model answer.

    No formulas, calculations, probabilities, or statistical
    conclusions are corrected.
    """

    raw_cleaned = (
        remove_special_tokens(
            text_value
        )
    )

    final_answer, has_final_answer = (
        extract_final_answer(
            raw_cleaned,
            model_name=model_name,
        )
    )

    if not final_answer.strip():
        return "", False

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

    return (
        final_answer,
        has_final_answer,
    )


# ============================================================
# 12. Word Count
# ============================================================

def count_words(
    text_value: str,
) -> int:
    """
    Count word-like tokens in the final answer.
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
    prompt: str,
) -> Tuple[str, list]:
    """
    Apply the model-specific chat template.

    Returns:
        formatted_prompt
        messages
    """

    messages = [
        {
            "role":
                "user",

            "content":
                prompt.strip(),
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
# 14. Generate One CoT Answer
# ============================================================

def generate_answer(
    model: Any,
    tokenizer: Any,
    prompt: str,
    model_name: str,
) -> Dict[str, Any]:
    """
    Generate one Structured CoT response.

    The raw generation is preserved for audit/debugging.
    """

    formatted_prompt, messages = (
        build_formatted_prompt(
            tokenizer=tokenizer,
            prompt=prompt,
        )
    )

    generation_start = (
        time.time()
    )

    raw_answer = (
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

    if not raw_answer:

        raise ValueError(
            "Model produced an empty response."
        )

    final_answer, has_final_answer = (
        clean_model_answer(
            raw_answer,
            model_name=model_name,
        )
    )

    generation_status = (
        "complete"
        if has_final_answer
        else "incomplete"
    )

    # --------------------------------------------------------
    # Preserve conversation history.
    # --------------------------------------------------------

    messages_with_answer = (
        messages
        + [
            {
                "role":
                    "assistant",

                "content":
                    raw_answer,
            }
        ]
    )

    return {
        "formatted_prompt":
            formatted_prompt,

        "messages":
            messages_with_answer,

        "raw_turn_outputs":
            [raw_answer],

        "final_answer":
            final_answer,

        "generation_status":
            generation_status,

        "generation_elapsed":
            generation_elapsed,
    }


# ============================================================
# 15. Build Successful / Incomplete Output Row
# ============================================================

def build_output_row(
    row: pd.Series,
    result: Dict[str, Any],
    model_key: str,
    model_path: str,
    elapsed: float,
) -> Dict[str, Any]:
    """
    Build the experiment output record.

    Shared fields are aligned with Direct results.
    CoT-specific fields are appended at the end.
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
            "CoT",

        # ====================================================
        # Gold Answer
        # ====================================================

        "Correct Answer":
            row.get(
                "Correct Answer",
                "",
            ),

        # ====================================================
        # Prompt and Model Output
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
        # Runtime Information
        # ====================================================

        "max_tokens":
            MAX_TOKENS,

        "elapsed_seconds":
            round(
                elapsed,
                3,
            ),

        "act_status":
            (
                "success"
                if generation_status
                == "complete"
                else "incomplete"
            ),

        "error_message":
            "",

        # ====================================================
        # Raw Generation
        # ====================================================

        "raw_turn_outputs":
            json.dumps(
                result[
                    "raw_turn_outputs"
                ],
                ensure_ascii=False,
            ),

        # ====================================================
        # CoT-Specific Fields
        # ====================================================

        "cot_reasoning_scaffold":
            row.get(
                "cot_reasoning_scaffold",
                "",
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
# 16. Build Failed Output Row
# ============================================================

def build_error_row(
    row: pd.Series,
    model_key: str,
    model_path: str,
    elapsed: float,
    error: Exception,
    prompt: str = "",
) -> Dict[str, Any]:
    """
    Build an output record for a genuine Python/model failure.
    """

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
            "CoT",

        # ====================================================
        # Gold Answer
        # ====================================================

        "Correct Answer":
            row.get(
                "Correct Answer",
                "",
            ),

        # ====================================================
        # Prompt and Output
        # ====================================================

        "full_prompt":
            prompt,

        "model_answer":
            "",

        "output_words_cnt":
            0,

        "generation_status":
            "failed",

        # ====================================================
        # Runtime Information
        # ====================================================

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

        # ====================================================
        # Raw Generation
        # ====================================================

        "raw_turn_outputs":
            "[]",

        # ====================================================
        # CoT-Specific Fields
        # ====================================================

        "cot_reasoning_scaffold":
            row.get(
                "cot_reasoning_scaffold",
                "",
            ),

        "messages_json":
            "[]",
    }


# ============================================================
# 17. Append Result to CSV
# ============================================================

def append_result(
    record: Dict[str, Any],
    output_path: Path,
) -> None:
    """
    Append one result immediately.

    This protects completed work if execution is interrupted.
    """

    result_df = pd.DataFrame(
        [record]
    )

    write_header = (
        not output_path.exists()
        or output_path.stat().st_size == 0
    )

    output_path.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    result_df.to_csv(
        output_path,
        mode=(
            "w"
            if write_header
            else "a"
        ),
        header=write_header,
        index=False,
        encoding="utf-8-sig",
    )


# ============================================================
# 18. Question ID Normalisation
# ============================================================

def normalise_question_id(
    value: Any,
) -> str:
    """
    Treat values such as 1 and 1.0 as the same question ID.
    """

    if pd.isna(value):
        return ""

    text = str(
        value
    ).strip()

    if text.endswith(
        ".0"
    ):

        text = text[
            :-2
        ]

    return text


# ============================================================
# 19. Load Completed Tasks
# ============================================================

def load_completed_tasks(
    output_path: Path,
) -> set:
    """
    Load model-question combinations already completed.

    Only rows where:
        act_status == success
        generation_status == complete

    are considered finished.
    """

    if not output_path.exists():
        return set()

    try:

        existing_results = (
            pd.read_csv(
                output_path
            )
        )

        if existing_results.empty:
            return set()

        if (
            "act_status"
            not in existing_results.columns
        ):

            return set()

        successful_results = (
            existing_results[
                existing_results[
                    "act_status"
                ]
                == "success"
            ]
        )

        if (
            "generation_status"
            in successful_results.columns
        ):

            successful_results = (
                successful_results[
                    successful_results[
                        "generation_status"
                    ]
                    == "complete"
                ]
            )

        completed = set()

        for _, result_row in (
            successful_results.iterrows()
        ):

            completed.add(
                (
                    str(
                        result_row[
                            "model_name"
                        ]
                    ).strip(),

                    normalise_question_id(
                        result_row[
                            "No"
                        ]
                    ),
                )
            )

        return completed

    except Exception as error:

        print(
            f"Could not read existing results: "
            f"{error}"
        )

        return set()


# ============================================================
# 20. Run One Model
# ============================================================

def run_one_model(
    dataset: pd.DataFrame,
    model_name: str,
    model_id: str,
    completed_tasks: set,
) -> None:
    """
    Run the Structured CoT experiment for one model.
    """

    print(
        "\n"
        + "=" * 70
    )

    print(
        f"Loading model: "
        f"{model_name}"
    )

    print(
        f"Model ID: "
        f"{model_id}"
    )

    print(
        "=" * 70
    )

    # --------------------------------------------------------
    # Avoid loading a model if all of its tasks are complete.
    # --------------------------------------------------------

    pending_exists = False

    for _, row in dataset.iterrows():

        task_key = (
            model_name,

            normalise_question_id(
                row.get(
                    "No"
                )
            ),
        )

        if (
            task_key
            not in completed_tasks
        ):

            pending_exists = True
            break

    if not pending_exists:

        print(
            f"{model_name}: "
            f"all question are already complete."
        )

        return

    # --------------------------------------------------------
    # Load model once.
    # --------------------------------------------------------

    model, tokenizer = (
        load(
            model_id
        )
    )

    # Fix Llama 3 stop tokens
    if "llama" in model_name.lower():
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

    total_questions = (
        len(
            dataset
        )
    )

    # ========================================================
    # Question Loop
    # ========================================================

    for position, (_, row) in enumerate(
        dataset.iterrows(),
        start=1,
    ):

        question_id = (
            row.get(
                "No"
            )
        )
        question_type = (
            row.get(
                "Question Type"
            )
        )
        task_key = (
            model_name,

            normalise_question_id(
                question_id
            ),
        )

        # ----------------------------------------------------
        # Skip already completed tasks.
        # ----------------------------------------------------

        if task_key in completed_tasks:

            print(
                f"[{model_name}] "
                f"Question "
                f"{position}/{total_questions} "
                f"(No: {question_id}) "
                f"already completed. Skipped."
            )

            continue

        print(
            f"\n[{model_name}] "
            f"Question "
            f"{position}/{total_questions} "
            f"(No: {question_id})"
            f"(question_type: {question_type})"
        )

        start_time = (
            time.time()
        )

        prompt = ""

        try:

            # ------------------------------------------------
            # Build Structured CoT prompt.
            # ------------------------------------------------

            prompt = (
                build_prompt(
                    row
                )
            )

            # ------------------------------------------------
            # Generate answer.
            # ------------------------------------------------

            result = (
                generate_answer(
                    model=model,
                    tokenizer=tokenizer,
                    prompt=prompt,
                    model_name=model_name,
                )
            )

            elapsed = (
                time.time()
                - start_time
            )

            # ------------------------------------------------
            # Both complete and incomplete generations are
            # valid experiment outcomes.
            # ------------------------------------------------

            record = (
                build_output_row(
                    row=row,
                    result=result,
                    model_key=model_name,
                    model_path=model_id,
                    elapsed=elapsed,
                )
            )

            print(
                f"STATUS: "
                f"[{model_name}] "
                f"Question No "
                f"{question_id} | "
                f"{result['generation_status']} | "
                f"Elapsed: "
                f"{elapsed:.2f}s | "
                f"Words: "
                f"{record['output_words_cnt']}"
            )

        except Exception as error:

            elapsed = (
                time.time()
                - start_time
            )

            record = (
                build_error_row(
                    row=row,
                    model_key=model_name,
                    model_path=model_id,
                    elapsed=elapsed,
                    error=error,
                    prompt=prompt,
                )
            )

            print(
                f"ERROR: "
                f"[{model_name}] "
                f"Question No "
                f"{question_id}: "
                f"{error}"
            )

        # ----------------------------------------------------
        # Save immediately.
        # ----------------------------------------------------

        append_result(
            record=record,
            output_path=OUTPUT_PATH,
        )

        print(
            f"Saved Question No "
            f"{question_id} "
            f"({model_name}) to CSV."
        )

        # ----------------------------------------------------
        # Only complete results are considered finished.
        # ----------------------------------------------------

        if (
            record[
                "act_status"
            ]
            == "success"
            and record[
                "generation_status"
            ]
            == "complete"
        ):

            completed_tasks.add(
                task_key
            )

    # ========================================================
    # Release Model Memory
    # ========================================================

    del model
    del tokenizer

    gc.collect()

    print(
        f"\n{model_name} completed."
    )


# ============================================================
# 21. Main Function
# ============================================================

def main():
    """
    Execute the complete Structured CoT experiment.
    """

    print(
        "Reading dataset..."
    )

    if not DATASET_PATH.exists():

        raise FileNotFoundError(
            f"Dataset file does not exist:\n"
            f"{DATASET_PATH}"
        )

    dataset = pd.read_csv(
        DATASET_PATH
    )

    print(
        f"Dataset path: "
        f"{DATASET_PATH}"
    )

    print(
        f"Number of question: "
        f"{len(dataset)}"
    )

    print(
        f"Output path: "
        f"{OUTPUT_PATH}"
    )

    print(
        f"Common safety token limit: "
        f"{MAX_TOKENS}"
    )

    # --------------------------------------------------------
    # Validate required columns.
    # --------------------------------------------------------

    required_columns = [
        "No",
        "Adapted Question",
        "Correct Answer",
        "cot_reasoning_scaffold",
    ]

    missing_columns = [
        column
        for column in required_columns
        if column not in dataset.columns
    ]

    if missing_columns:

        raise ValueError(
            "Dataset is missing required columns: "
            f"{missing_columns}"
        )

    # --------------------------------------------------------
    # Resume support.
    # --------------------------------------------------------

    completed_tasks = (
        load_completed_tasks(
            OUTPUT_PATH
        )
    )

    print(
        f"Previously completed tasks: "
        f"{len(completed_tasks)}"
    )

    # --------------------------------------------------------
    # Run models sequentially.
    # --------------------------------------------------------

    for (
        model_name,
        model_id,
    ) in MODELS.items():

        run_one_model(
            dataset=dataset,
            model_name=model_name,
            model_id=model_id,
            completed_tasks=completed_tasks,
        )

    print(
        "\n"
        + "=" * 70
    )

    print(
        "Structured CoT experiment completed."
    )

    print(
        f"Results saved to:\n"
        f"{OUTPUT_PATH}"
    )

    print(
        "=" * 70
    )


# ============================================================
# 22. Entry Point
# ============================================================

if __name__ == "__main__":
    main()