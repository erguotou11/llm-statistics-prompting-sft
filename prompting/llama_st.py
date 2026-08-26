import ast
import csv
import gc
import json
import os
import re
import time

from pathlib import Path
from typing import Any, Dict, Tuple

import pandas as pd
from mlx_lm import generate, load
from openai import OpenAI


# ============================================================
# 1. Configuration
# ============================================================



DATASET_PATH = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/200/"
    "final dataset2/part4to5_final.csv"
)

OUTPUT_PATH = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/200/"
    "final dataset2/part4to5_llama_st_results.csv"
)


STUDENT_MODELS = {
    "Llama3-8B": (
        "mlx-community/"
        "Meta-Llama-3-8B-Instruct-4bit"
    ),
}

STUDENT_MAX_TOKENS = 8192
TUTOR_MAX_TURNS = 3

TUTOR_MODEL = "qwen-plus"
TUTOR_TEMPERATURE = 0.0
TUTOR_ENABLE_THINKING = False
TUTOR_MAX_RETRIES = 3
TUTOR_RETRY_DELAY_SECONDS = 2


# ============================================================
# 2. Tutor Client
# ============================================================

DASHSCOPE_API_KEY = os.getenv("DASHSCOPE_API_KEY")

if not DASHSCOPE_API_KEY:
    raise ValueError("DASHSCOPE_API_KEY is not set.")

TUTOR_CLIENT = OpenAI(
    api_key=DASHSCOPE_API_KEY,
    base_url=(
        "https://dashscope.aliyuncs.com/"
        "compatible-mode/v1"
    ),
)


# ============================================================
# 3. Shared Instructions
# ============================================================

PRECISION_INSTRUCTION = """
Numerical precision:
- Round z to 2 d.p. and t to 3 d.p. before obtaining probabilities or using critical values in CIs (calculate SE first).
- P-values: Round to max 3 d.p. without trailing zeros (e.g., 0.030 → 0.03; report "p < 0.001" if p < 0.001; never write p = 0.000).
- CI Endpoints: Round to 2 d.p. if |x| ≥ 1; max 4 d.p. without trailing zeros if |x| < 1.
- Maintain full precision for all other intermediate calculations.
- Avoid repeated verification—if uncertainty remains, report the most reasonable conclusion.
""".strip()


STUDENT_INSTRUCTION = """
You are answering a statistics problem.

Attempt the entire problem independently and do as much as you can.

Provide a complete response to the original problem, including any
reasoning, calculations, option evaluations, or conclusions that you
believe are necessary to support your answer.

Do not output role prefixes such as:
"Student:", "Student response:", or "Response:".

Do not simulate tutor feedback or future conversational turns.

Do not discuss the tutoring system, reference answer,
or evaluation_data process.

When tutor feedback is provided, use it to reconsider and revise your
complete solution.

If the Tutor identifies a calculation or reasoning error, re-check the
affected part independently rather than mechanically reusing the values
or conclusions from your previous response.

Do not preserve a previous numerical result merely because it appeared
in your earlier answer. Recompute or re-derive the relevant part when
the Tutor indicates that it may be incorrect.

If a numerical calculation becomes unstable or repetitive,
do not repeatedly recompute the same expression.

After one careful recalculation, if you cannot reliably improve
the numerical result, retain your best previous complete solution
and proceed.

""".strip()


TUTOR_SYSTEM_INSTRUCTION = """
You are an expert statistics tutor supervising a Student model.

The Student first attempts the entire problem independently.

Your role is adaptive: evaluate both the requirements of the specific
problem and the quality of the Student's current response, then identify
what, if anything, most needs improvement.

Use these five dimensions as an evaluation_data framework when relevant:

1. Answer correctness
2. Reasoning completeness
3. Logical consistency
4. Explanation clarity
5. Statistical interpretation

Do not treat the five dimensions as a fixed checklist.
Different question require different levels and types of reasoning.

First determine what an adequate solution to THIS problem requires.
Then determine which of those requirements the Student has already
satisfied and which important requirement, if any, remains unresolved.

The amount and type of Tutor feedback should therefore depend on:
- the question type and complexity;
- what the question explicitly asks for;
- the Student's current method, calculations, reasoning, and conclusion;
- errors or omissions that remain after previous feedback.

A response should contain the minimum sufficient reasoning needed
to justify its answer.

For simple problems, do not demand unnecessary elaboration.
However, a bare answer or option selection is not sufficient when
a calculation or statistical justification is necessary to support it.

For complex problems, require the reasoning, calculations, and
interpretation that are genuinely necessary for that task.

The private reference solution is a correctness reference, not the
only acceptable solution method.

Accept alternative methods when they are statistically and
mathematically valid and lead to the correct conclusion.

Do not mark a response as incorrect merely because its method,
formulation, or intermediate calculations differ from the reference
solution. Evaluate the Student's method on its own mathematical and
statistical validity.

When evaluating later turns, consider correct work already established
by the Student in earlier turns. Do not ask the Student to repeat correct
reasoning unless the current response contradicts, abandons, or makes it
unclear.

When the Student makes an arithmetic error, identify the specific
expression or step that appears inconsistent.

Do not independently provide or estimate the corrected numerical
value. Ask the Student to recompute the expression itself.

Do not instruct the Student to use a calculator, software, database,
or any other unavailable tool.

Set ready_for_final=true when the Student has adequately solved the
original problem and there is no important unresolved error or omission.

If improvement is still needed:
- identify only the most important remaining issue;
- provide targeted guidance for that issue;
- adapt the guidance to the Student's current response;
- do not mechanically repeat previous feedback;
- do not provide the complete solution;
- do not reveal the private reference answer;
- do not reveal a missing correct option;
- do not provide a new correct numerical value that the Student has
  not already obtained correctly.

The assessment field is private.
The next_prompt field is shown directly to the Student.

Return exactly one JSON object:

{
  "assessment": "...",
  "error_type": "...",
  "next_prompt": "...",
  "ready_for_final": false
}

Allowed error_type:
none, concept, method, calculation, reasoning,
tail_conversion, interpretation, incomplete, multiple
""".strip()




ALLOWED_ERROR_TYPES = {
    "none",
    "concept",
    "method",
    "calculation",
    "reasoning",
    "tail_conversion",
    "interpretation",
    "incomplete",
    "multiple",
}


# ============================================================
# 4. General Utilities
# ============================================================

def safe_text(value: Any) -> str:
    if value is None:
        return ""

    try:
        if pd.isna(value):
            return ""
    except Exception:
        pass

    return str(value).strip()


def count_words(text: str) -> int:
    if not text:
        return 0

    return len(
        re.findall(
            r"\b\w+\b",
            str(text),
        )
    )


def flatten_text(value: Any) -> str:
    """
    Keep CSV text on one physical line.
    """

    if value is None:
        return ""

    text = str(value)
    text = text.replace("\r\n", "\n")
    text = text.replace("\r", "\n")
    text = re.sub(r"\s*\n+\s*", " ", text)
    text = re.sub(r"[ \t]+", " ", text)

    return text.strip()


def json_for_csv(value: Any) -> str:
    """
    Serialize list/dict fields without physical CSV line breaks.
    """

    text = json.dumps(
        value,
        ensure_ascii=False,
    )

    return (
        text
        .replace("\r", "\\r")
        .replace("\n", "\\n")
    )


def normalise_question_id(value: Any) -> str:
    text = safe_text(value)

    if text.endswith(".0"):
        text = text[:-2]

    return text


# ============================================================
# 5. Model Output Cleaning
# ============================================================

def remove_special_tokens(text: str) -> str:
    if not text:
        return ""

    value = str(text)

    value = re.sub(
        r"<\|start_header_id\|>.*?<\|end_header_id\|>",
        "",
        value,
        flags=re.DOTALL,
    )

    value = re.sub(
        r"<\|[^|]+\|>",
        "",
        value,
    )

    value = (
        value
        .replace("<end_of_turn>", "")
        .replace("<start_of_turn>", "")
    )

    return value.strip()


def normalise_text(text: str) -> str:
    return re.sub(
        r"\s+",
        " ",
        str(text).lower(),
    ).strip()


def truncate_repeated_paragraphs(text: str) -> str:
    if not text:
        return ""

    paragraphs = [
        p.strip()
        for p in re.split(
            r"\n\s*\n",
            str(text),
        )
        if p.strip()
    ]

    result = []

    for paragraph in paragraphs:
        if (
            result
            and normalise_text(paragraph)
            == normalise_text(result[-1])
        ):
            break

        result.append(paragraph)

    return "\n\n".join(result).strip()


def truncate_repeated_sentences(
    text: str,
    threshold: int = 3,
) -> str:
    if not text:
        return ""

    sentences = re.split(
        r"(?<=[.!?])\s+",
        str(text).strip(),
    )

    result = []
    previous = None
    repeat_count = 0

    for sentence in sentences:
        sentence = sentence.strip()

        if not sentence:
            continue

        current = normalise_text(sentence)

        if current == previous:
            repeat_count += 1
        else:
            repeat_count = 1

        if repeat_count >= threshold:
            break

        result.append(sentence)
        previous = current

    return " ".join(result).strip()


def truncate_role_bleeding(text: str) -> str:
    """
    Stop Student output if it starts simulating Tutor interaction.
    """

    if not text:
        return ""

    patterns = [
        r"\n\s*Tutor\s*:",
        r"\n\s*Tutor feedback\s*:",
        r"\n\s*Tutor guidance\s*:",
        r"\n\s*Tutor assessment\s*:",
        r"\n\s*Original problem\s*:",
        r"\n\s*Evaluation\s*:",
    ]

    positions = []

    for pattern in patterns:
        match = re.search(
            pattern,
            text,
            flags=re.IGNORECASE,
        )

        if match:
            positions.append(
                match.start()
            )

    if positions:
        text = text[:min(positions)]

    text = re.sub(
        r"^(?:Student response|Student|Response)\s*:\s*",
        "",
        text,
        flags=re.IGNORECASE,
    )

    return text.strip()


def truncate_self_dialogue_tail(text: str) -> str:
    """
    Remove obvious conversational tails after the answer.
    """

    if not text:
        return ""

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

    positions = []

    for pattern in patterns:
        match = re.search(
            pattern,
            text,
            flags=re.IGNORECASE,
        )

        if match:
            positions.append(
                match.start()
            )

    if positions:
        text = text[:min(positions)]

    return text.strip()


# ============================================================
# 6. DeepSeek Answer Extraction
# ============================================================

def extract_student_answer(
    text: str,
    model_name: str,
) -> Tuple[str, bool]:
    """
    Return:
        extracted_answer,
        has_reliable_user_facing_answer

    DeepSeek:
    1. Prefer content after </think>.
    2. If </think> is missing, recover only an obvious answer
       section or clearly structured worked solution.
    3. Never return unrestricted internal reasoning.
    """

    if not text:
        return "", False

    value = str(text).strip()

    if "deepseek" not in model_name.lower():
        return value, True

    # --------------------------------------------------------
    # Normal DeepSeek case
    # --------------------------------------------------------

    if "</think>" in value:
        answer = value.rsplit(
            "</think>",
            1,
        )[1].strip()

        if answer:
            return answer, True

    # --------------------------------------------------------
    # Explicit answer headings
    # --------------------------------------------------------

    answer_patterns = [
        r"(?:^|\n)\s*\*\*Final Answer\s*:?\*\*\s*(.*)",
        r"(?:^|\n)\s*Final Answer\s*:\s*(.*)",
        r"(?:^|\n)\s*\*\*Answer\s*:?\*\*\s*(.*)",
        r"(?:^|\n)\s*Answer\s*:\s*(.*)",
        r"(?:^|\n)\s*\*\*Conclusion\s*:?\*\*\s*(.*)",
        r"(?:^|\n)\s*Conclusion\s*:\s*(.*)",
    ]

    for pattern in answer_patterns:
        match = re.search(
            pattern,
            value,
            flags=re.IGNORECASE | re.DOTALL,
        )

        if match:
            recovered = match.group(0).strip()

            if recovered:
                return recovered, True

    # --------------------------------------------------------
    # Structured multi-step solution fallback
    # --------------------------------------------------------

    step_match = re.search(
        r"(?:^|\n)"
        r"(\d+\.\s*(?:\*\*)?.+?"
        r"(?:\n\d+\.\s*(?:\*\*)?.+?)+)"
        r"(?=\n(?:Therefore|Thus|Hence|Answer|Final Answer|Conclusion)"
        r"|$)",
        value,
        flags=re.IGNORECASE | re.DOTALL,
    )

    if step_match:
        recovered = step_match.group(1).strip()

        if len(recovered.split()) >= 15:
            return recovered, True

    return "", False


def clean_student_output(
    raw_answer: str,
    model_name: str,
) -> Tuple[str, bool]:
    """
    Shared cleaning function.

    Used both for:
    - final Student answer;
    - stored Student turn output.

    This replaces the previous duplicate:
    clean_student_answer()
    +
    prepare_student_output_for_storage()
    """

    text = remove_special_tokens(
        raw_answer
    )

    answer, complete = extract_student_answer(
        text,
        model_name,
    )

    if not answer:
        return "", False

    cleaners = (
        truncate_role_bleeding,
        truncate_repeated_paragraphs,
        truncate_repeated_sentences,
        truncate_self_dialogue_tail,
    )

    for cleaner in cleaners:
        answer = cleaner(answer)

    answer = answer.strip()

    return (
        answer,
        bool(answer and complete),
    )


# ============================================================
# 7. Question / Prompt Builders
# ============================================================

def build_question_text(row: pd.Series) -> str:
    question = safe_text(
        row.get(
            "Adapted Question",
            "",
        )
    )

    if not question:
        raise ValueError(
            f"No.{row.get('No')} has no Adapted Question."
        )

    options = safe_text(
        row.get(
            "Options",
            "",
        )
    )

    if options:
        question += (
            "\n\nOptions:\n"
            + options
        )

    return question.strip()


def build_student_prompt(
    question_text: str,
    previous_response: str = "",
    tutor_guidance: str = "",
    final_revision: bool = False,
) -> str:
    parts = [
        STUDENT_INSTRUCTION,
        "",
        "Original problem:",
        question_text,
        "",
        PRECISION_INSTRUCTION,
    ]

    if previous_response:
        parts.extend(
            [
                "",
                "Your previous response:",
                previous_response,
            ]
        )

    if tutor_guidance:
        parts.extend(
            [
                "",
                "Tutor feedback:",
                tutor_guidance,
            ]
        )

    if final_revision:
        parts.extend(
            [
                "",
                (
                    "Revise the solution one final time. "
                    "Give a complete answer to the original problem, "
                    "including the necessary reasoning and calculations."
                ),
            ]
        )

    elif tutor_guidance:
        parts.extend(
            [
                "",
                (
                    "Revise your complete solution to the original "
                    "problem using the Tutor feedback."
                ),
            ]
        )

    return "\n".join(parts).strip()


def format_student_prompt(
    tokenizer: Any,
    prompt: str,
) -> str:
    return tokenizer.apply_chat_template(
        [
            {
                "role": "user",
                "content": prompt,
            }
        ],
        tokenize=False,
        add_generation_prompt=True,
    )


# ============================================================
# 8. Student Generation
# ============================================================

def call_student_model(
    model: Any,
    tokenizer: Any,
    prompt: str,
    model_name: str,
) -> Dict[str, Any]:
    formatted_prompt = format_student_prompt(
        tokenizer,
        prompt,
    )

    start = time.perf_counter()

    raw_answer = generate(
        model=model,
        tokenizer=tokenizer,
        prompt=formatted_prompt,
        max_tokens=STUDENT_MAX_TOKENS,
        verbose=False,
    )

    elapsed = (
        time.perf_counter()
        - start
    )

    raw_answer = str(
        raw_answer
    ).strip()

    final_answer, complete = clean_student_output(
        raw_answer,
        model_name,
    )

    return {
        "formatted_prompt": formatted_prompt,
        "raw_answer": raw_answer,

        # Cleaned user-facing output.
        "stored_answer": final_answer,

        "final_answer": final_answer,

        "generation_status": (
            "complete"
            if complete
            else "incomplete"
        ),

        "elapsed_seconds": elapsed,
    }


# ============================================================
# 9. Tutor Prompt
# ============================================================
#
# def build_tutor_prompt(
#     question_text: str,
#     reference_answer: str,
#     reference_solution: str,
#     student_response: str,
#     turn_number: int,
# ) -> str:
#     return f"""
# Original statistical problem:
# {question_text}
#
# PRIVATE reference answer:
# {reference_answer}
#
# PRIVATE reference solution:
# {reference_solution}
#
# Current tutoring turn:
# {turn_number} of {TUTOR_MAX_TURNS}
#
# Student's CURRENT response:
# {student_response}
#
# Evaluate only the CURRENT response.
#
# Remember:
#
# - assessment is private;
# - next_prompt is shown to the Student;
# - never reveal a missing correct option;
# - never reveal a new correct numerical value;
# - do not give the complete solution;
# - give focused guidance only.
#
# Return exactly one JSON object.
# """.strip()
#v2
def build_tutor_prompt(
    question_text: str,
    reference_answer: str,
    reference_solution: str,
    student_response: str,
    turn_number: int,
) -> str:
    return f"""
Original statistical problem:
{question_text}

Numerical precision rules for this problem:
{PRECISION_INSTRUCTION}

PRIVATE reference answer:
{reference_answer}

PRIVATE reference solution:
{reference_solution}

Current tutoring turn:
{turn_number} of {TUTOR_MAX_TURNS}

Student's CURRENT response:
{student_response}

Evaluate only the CURRENT response.

When evaluating numerical calculations:
- Apply the numerical precision rules above.
- Do not identify a numerical result as incorrect solely because the Student
  follows the specified rounding rules.
- Do not require greater numerical precision than specified unless the
  original problem explicitly requires it.

Remember:

- assessment is private;
- next_prompt is shown to the Student;
- never reveal a missing correct option;
- never reveal a new correct numerical value;
- do not give the complete solution;
- give focused guidance only.

Return exactly one JSON object.
""".strip()

# ============================================================
# 10. Tutor JSON Parsing
# ============================================================

def extract_json_object(
    raw_text: str,
) -> Dict[str, Any]:
    text = safe_text(
        raw_text
    )

    if text.startswith("```"):
        text = re.sub(
            r"^```(?:json)?\s*",
            "",
            text,
            flags=re.IGNORECASE,
        )

        text = re.sub(
            r"\s*```$",
            "",
            text,
        ).strip()

    match = re.search(
        r"\{.*\}",
        text,
        flags=re.DOTALL,
    )

    candidate = (
        match.group(0)
        if match
        else text
    )

    try:
        parsed = json.loads(
            candidate
        )

    except Exception:
        parsed = ast.literal_eval(
            candidate
        )

    required = {
        "assessment",
        "error_type",
        "next_prompt",
        "ready_for_final",
    }

    if (
        not isinstance(parsed, dict)
        or not required.issubset(parsed)
    ):
        raise ValueError(
            "Invalid Tutor JSON."
        )

    error_type = safe_text(
        parsed["error_type"]
    ).lower()

    if error_type not in ALLOWED_ERROR_TYPES:
        error_type = "multiple"

    ready = parsed[
        "ready_for_final"
    ]

    if isinstance(ready, str):
        ready = (
            ready.strip().lower()
            == "true"
        )
    else:
        ready = bool(ready)

    # Tutor cannot be "ready" while still reporting an error.
    if ready and error_type != "none":
        ready = False

    next_prompt = safe_text(
        parsed["next_prompt"]
    )

    if not ready and not next_prompt:
        next_prompt = (
            "Re-check the most important remaining issue "
            "in your current solution."
        )

    if ready:
        next_prompt = ""

    return {
        "assessment": safe_text(
            parsed["assessment"]
        ),
        "error_type": error_type,
        "next_prompt": next_prompt,
        "ready_for_final": ready,
    }


# ============================================================
# 11. Tutor API
# ============================================================

def call_qwen_tutor(
    client: OpenAI,
    tutor_prompt: str,
) -> Tuple[Dict[str, Any], str]:
    last_error = None

    for attempt in range(
        1,
        TUTOR_MAX_RETRIES + 1,
    ):
        try:
            response = client.chat.completions.create(
                model=TUTOR_MODEL,

                messages=[
                    {
                        "role": "system",
                        "content": TUTOR_SYSTEM_INSTRUCTION,
                    },
                    {
                        "role": "user",
                        "content": tutor_prompt,
                    },
                ],

                temperature=TUTOR_TEMPERATURE,
                stream=False,

                extra_body={
                    "enable_thinking":
                        TUTOR_ENABLE_THINKING
                },
            )

            raw = (
                response
                .choices[0]
                .message
                .content
                .strip()
            )

            return (
                extract_json_object(raw),
                raw,
            )

        except Exception as error:
            last_error = error

            if attempt < TUTOR_MAX_RETRIES:
                time.sleep(
                    TUTOR_RETRY_DELAY_SECONDS
                    * attempt
                )

    raise RuntimeError(
        f"Tutor failed: {last_error}"
    )

# # ============================================================
# # 12. Interaction Result Helper
# # ============================================================

def make_interaction_result(
    final_answer: str,
    status: str,
    turns: int,
    history: list,
    student_outputs: list,
    student_prompts: list,
    tutor_outputs: list,
    final_prompt: str,
    generation_seconds: float,
) -> Dict[str, Any]:
    """
    Shared return structure for all Student-Tutor exit paths.
    """

    return {
        "final_answer": final_answer,
        "generation_status": status,
        "tutoring_turns": turns,
        "interaction_history": history,
        "student_raw_turn_outputs": student_outputs,
        "student_formatted_prompts": student_prompts,
        "tutor_raw_outputs": tutor_outputs,
        "final_formatted_prompt": final_prompt,
        "student_generation_seconds": generation_seconds,
    }


# ============================================================
# 13. Student-Tutor Interaction
# ============================================================

def run_student_tutor(
    row: pd.Series,
    student_model: Any,
    student_tokenizer: Any,
    student_model_name: str,
    tutor_client: OpenAI,
) -> Dict[str, Any]:
    question_text = build_question_text(
        row
    )

    reference_answer = safe_text(
        row.get(
            "Correct Answer",
            "",
        )
    )

    reference_solution = safe_text(
        row.get(
            "Solution / Key Step",
            row.get(
                "Solution / Key Steps",
                "",
            ),
        )
    )

    history = []
    student_outputs = []
    student_prompts = []
    tutor_outputs = []

    generation_seconds = 0.0

    previous_response = ""
    tutor_guidance = ""

    # Last complete Student answer.
    # Used if a later DeepSeek turn gets trapped inside <think>.
    last_complete = None


    # ========================================================
    # Normal tutoring rounds
    # ========================================================

    for turn in range(
        1,
        TUTOR_MAX_TURNS + 1,
    ):
        student_prompt = build_student_prompt(
            question_text=question_text,
            previous_response=previous_response,
            tutor_guidance=tutor_guidance,
        )

        student_result = call_student_model(
            model=student_model,
            tokenizer=student_tokenizer,
            prompt=student_prompt,
            model_name=student_model_name,
        )

        generation_seconds += (
            student_result[
                "elapsed_seconds"
            ]
        )

        # Store only cleaned user-facing output.
        # DeepSeek <think> content is excluded.
        student_outputs.append(
            student_result[
                "stored_answer"
            ]
        )

        student_prompts.append(
            student_result[
                "formatted_prompt"
            ]
        )

        # ----------------------------------------------------
        # Current Student generation failed/incomplete.
        # ----------------------------------------------------

        if (
            student_result[
                "generation_status"
            ]
            != "complete"
        ):
            # Keep an earlier complete answer instead of
            # replacing the entire question with an empty answer.
            if last_complete:
                return make_interaction_result(
                    final_answer=
                        last_complete["answer"],

                    status=
                        "complete",

                    turns=
                        last_complete["turn"],

                    history=
                        history,

                    student_outputs=
                        student_outputs,

                    student_prompts=
                        student_prompts,

                    tutor_outputs=
                        tutor_outputs,

                    final_prompt=
                        last_complete["prompt"],

                    generation_seconds=
                        generation_seconds,
                )

            # No usable Student answer has ever been produced.
            return make_interaction_result(
                final_answer="",
                status="incomplete",
                turns=turn,
                history=history,
                student_outputs=student_outputs,
                student_prompts=student_prompts,
                tutor_outputs=tutor_outputs,
                final_prompt=student_result[
                    "formatted_prompt"
                ],
                generation_seconds=generation_seconds,
            )

        # ----------------------------------------------------
        # Complete Student response.
        # ----------------------------------------------------

        current_response = (
            student_result[
                "final_answer"
            ]
        )

        last_complete = {
            "answer":
                current_response,

            "prompt":
                student_result[
                    "formatted_prompt"
                ],

            "turn":
                turn,
        }

        # ----------------------------------------------------
        # Tutor evaluation_data.
        # ----------------------------------------------------

        tutor_prompt = build_tutor_prompt(
            question_text=
                question_text,

            reference_answer=
                reference_answer,

            reference_solution=
                reference_solution,

            student_response=
                current_response,

            turn_number=
                turn,
        )

        tutor_result, raw_tutor = (
            call_qwen_tutor(
                tutor_client,
                tutor_prompt,
            )
        )

        tutor_outputs.append(
            raw_tutor
        )

        history.append(
            {
                "turn":
                    turn,

                "student_response":
                    current_response,

                "tutor_assessment":
                    tutor_result[
                        "assessment"
                    ],

                "error_type":
                    tutor_result[
                        "error_type"
                    ],

                "next_prompt":
                    tutor_result[
                        "next_prompt"
                    ],

                "ready_for_final":
                    tutor_result[
                        "ready_for_final"
                    ],
            }
        )

        # ----------------------------------------------------
        # Tutor accepts current response.
        # ----------------------------------------------------

        if tutor_result[
            "ready_for_final"
        ]:
            return make_interaction_result(
                final_answer=
                    current_response,

                status=
                    "complete",

                turns=
                    turn,

                history=
                    history,

                student_outputs=
                    student_outputs,

                student_prompts=
                    student_prompts,

                tutor_outputs=
                    tutor_outputs,

                final_prompt=
                    student_result[
                        "formatted_prompt"
                    ],

                generation_seconds=
                    generation_seconds,
            )

        previous_response = (
            current_response
        )

        tutor_guidance = (
            tutor_result[
                "next_prompt"
            ]
        )


    # ========================================================
    # Final Revision
    #
    # Only reached if Tutor did not approve within max turns.
    # ========================================================

    final_prompt = build_student_prompt(
        question_text=
            question_text,

        previous_response=
            previous_response,

        tutor_guidance=
            tutor_guidance,

        final_revision=
            True,
    )

    final_result = call_student_model(
        model=student_model,
        tokenizer=student_tokenizer,
        prompt=final_prompt,
        model_name=student_model_name,
    )

    generation_seconds += (
        final_result[
            "elapsed_seconds"
        ]
    )

    student_outputs.append(
        final_result[
            "stored_answer"
        ]
    )

    student_prompts.append(
        final_result[
            "formatted_prompt"
        ]
    )

    # --------------------------------------------------------
    # Final revision succeeded.
    # --------------------------------------------------------

    if (
        final_result[
            "generation_status"
        ]
        == "complete"
    ):
        return make_interaction_result(
            final_answer=
                final_result[
                    "final_answer"
                ],

            status=
                "complete",

            turns=
                TUTOR_MAX_TURNS,

            history=
                history,

            student_outputs=
                student_outputs,

            student_prompts=
                student_prompts,

            tutor_outputs=
                tutor_outputs,

            final_prompt=
                final_result[
                    "formatted_prompt"
                ],

            generation_seconds=
                generation_seconds,
        )

    # --------------------------------------------------------
    # Final revision failed but earlier answer exists.
    # --------------------------------------------------------

    if last_complete:
        return make_interaction_result(
            final_answer=
                last_complete[
                    "answer"
                ],

            status=
                "complete",

            turns=
                last_complete[
                    "turn"
                ],

            history=
                history,

            student_outputs=
                student_outputs,

            student_prompts=
                student_prompts,

            tutor_outputs=
                tutor_outputs,

            final_prompt=
                last_complete[
                    "prompt"
                ],

            generation_seconds=
                generation_seconds,
        )

    # --------------------------------------------------------
    # Nothing usable exists.
    # --------------------------------------------------------

    return make_interaction_result(
        final_answer="",
        status="incomplete",
        turns=TUTOR_MAX_TURNS,
        history=history,
        student_outputs=student_outputs,
        student_prompts=student_prompts,
        tutor_outputs=tutor_outputs,
        final_prompt=final_result[
            "formatted_prompt"
        ],
        generation_seconds=generation_seconds,
    )


# ============================================================
# 14. Output Builders
# ============================================================

def build_base_output_row(
    row: pd.Series,
    model_name: str,
    model_id: str,
) -> Dict[str, Any]:
    """
    Shared metadata for success and failure rows.
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
            flatten_text(
                row.get(
                    "Adapted Question",
                    "",
                )
            ),

        "Options":
            flatten_text(
                row.get(
                    "Options",
                    "",
                )
            ),

        "Solution / Key Step":
            flatten_text(
                row.get(
                    "Solution / Key Step",
                    row.get(
                        "Solution / Key Steps",
                        "",
                    ),
                )
            ),

        "model_name":
            model_name,

        "model_id":
            model_id,

        "prompt_structure":
            "Student-Tutor",

        "Correct Answer":
            flatten_text(
                row.get(
                    "Correct Answer",
                    "",
                )
            ),
    }


def build_output_row(
    row: pd.Series,
    result: Dict[str, Any],
    model_name: str,
    model_id: str,
    elapsed: float,
) -> Dict[str, Any]:
    record = build_base_output_row(
        row,
        model_name,
        model_id,
    )

    answer = result[
        "final_answer"
    ]

    status = result[
        "generation_status"
    ]

    final_prompt = remove_special_tokens(
        result.get(
            "final_formatted_prompt",
            "",
        )
    )

    record.update(
        {
            "full_prompt":
                flatten_text(
                    final_prompt
                ),

            "model_answer":
                flatten_text(
                    answer
                ),

            "output_words_cnt":
                count_words(
                    answer
                ),

            "generation_status":
                status,

            "max_tokens":
                STUDENT_MAX_TOKENS,

            "elapsed_seconds":
                round(
                    elapsed,
                    3,
                ),

            "act_status":
                (
                    "success"
                    if status == "complete"
                    else "incomplete"
                ),

            "error_message":
                "",

            "tutoring_turns":
                result[
                    "tutoring_turns"
                ],

            "interaction_history":
                json_for_csv(
                    result[
                        "interaction_history"
                    ]
                ),

            "student_raw_turn_outputs":
                json_for_csv(
                    result[
                        "student_raw_turn_outputs"
                    ]
                ),

            "student_formatted_prompts":
                json_for_csv(
                    result[
                        "student_formatted_prompts"
                    ]
                ),

            "tutor_raw_outputs":
                json_for_csv(
                    result[
                        "tutor_raw_outputs"
                    ]
                ),

            "student_generation_seconds":
                round(
                    result[
                        "student_generation_seconds"
                    ],
                    3,
                ),
        }
    )

    return record


def build_error_row(
    row: pd.Series,
    model_name: str,
    model_id: str,
    elapsed: float,
    error: Exception,
) -> Dict[str, Any]:
    record = build_base_output_row(
        row,
        model_name,
        model_id,
    )

    record.update(
        {
            "full_prompt":
                "",

            "model_answer":
                "",

            "output_words_cnt":
                0,

            "generation_status":
                "failed",

            "max_tokens":
                STUDENT_MAX_TOKENS,

            "elapsed_seconds":
                round(
                    elapsed,
                    3,
                ),

            "act_status":
                "failed",

            "error_message":
                flatten_text(
                    str(error)
                ),

            "tutoring_turns":
                0,

            "interaction_history":
                "[]",

            "student_raw_turn_outputs":
                "[]",

            "student_formatted_prompts":
                "[]",

            "tutor_raw_outputs":
                "[]",

            "student_generation_seconds":
                0.0,
        }
    )

    return record


# ============================================================
# 15. CSV Persistence
# ============================================================

def append_result(
    record: Dict[str, Any],
) -> None:
    OUTPUT_PATH.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    write_header = (
        not OUTPUT_PATH.exists()
        or OUTPUT_PATH.stat().st_size == 0
    )

    pd.DataFrame(
        [record]
    ).to_csv(
        OUTPUT_PATH,
        mode=(
            "w"
            if write_header
            else "a"
        ),
        header=write_header,
        index=False,
        encoding="utf-8-sig",
        quoting=csv.QUOTE_ALL,
    )


# ============================================================
# 16. Resume Support
# ============================================================

def load_completed_tasks() -> set:
    if (
        not OUTPUT_PATH.exists()
        or OUTPUT_PATH.stat().st_size == 0
    ):
        return set()

    try:
        df = pd.read_csv(
            OUTPUT_PATH,
            keep_default_na=False,
        )

        required = {
            "model_name",
            "No",
            "act_status",
            "generation_status",
        }

        if (
            df.empty
            or not required.issubset(
                df.columns
            )
        ):
            return set()

        successful = df[
            (df["act_status"] == "success")
            &
            (df["generation_status"] == "complete")
        ]

        return {
            (
                safe_text(
                    row["model_name"]
                ),

                normalise_question_id(
                    row["No"]
                ),
            )

            for _, row
            in successful.iterrows()
        }

    except Exception as error:
        print(
            f"Resume check failed: "
            f"{error}"
        )

        return set()


# ============================================================
# 17. Model Runner
# ============================================================

def run_one_model(
    dataset: pd.DataFrame,
    model_name: str,
    model_id: str,
    completed_tasks: set,
) -> None:
    print(
        "\n"
        + "=" * 70
    )

    print(
        f"Loading: "
        f"{model_name}"
    )

    print(
        "=" * 70
    )

    # --------------------------------------------------------
    # Avoid loading a model if all its question are complete.
    # --------------------------------------------------------

    pending_exists = any(
        (
            model_name,
            normalise_question_id(
                row.get(
                    "No"
                )
            ),
        )
        not in completed_tasks

        for _, row
        in dataset.iterrows()
    )

    if not pending_exists:
        print(
            f"{model_name}: "
            "all question already completed."
        )

        return

    model, tokenizer = load(
        model_id
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

    total = len(
        dataset
    )

    for position, (_, row) in enumerate(
        dataset.iterrows(),
        start=1,
    ):
        question_id = (
            normalise_question_id(
                row.get(
                    "No"
                )
            )
        )
        question_type = row.get(
            "Question Type",
            ""
        )
        task_key = (
            model_name,
            question_id,
        )

        if task_key in completed_tasks:
            print(
                f"[{model_name}] "
                f"{position}/{total} "
                f"No.{question_id} skipped."
            )

            continue

        print(
            f"\n[{model_name}] "
            f"{position}/{total} "
            f"No.{question_id}"
            f"| Type: {question_type}"
        )

        start = time.perf_counter()

        try:
            result = run_student_tutor(
                row=row,
                student_model=model,
                student_tokenizer=tokenizer,
                student_model_name=model_name,
                tutor_client=TUTOR_CLIENT,
            )

            elapsed = (
                time.perf_counter()
                - start
            )

            record = build_output_row(
                row=row,
                result=result,
                model_name=model_name,
                model_id=model_id,
                elapsed=elapsed,
            )

            print(
                f"Status={record['generation_status']} | "
                f"Turns={record['tutoring_turns']} | "
                f"Words={record['output_words_cnt']} | "
                f"{elapsed:.2f}s"
            )

        except Exception as error:
            elapsed = (
                time.perf_counter()
                - start
            )

            record = build_error_row(
                row=row,
                model_name=model_name,
                model_id=model_id,
                elapsed=elapsed,
                error=error,
            )

            print(
                f"ERROR: "
                f"{error}"
            )

        # Save immediately after every question.
        append_result(
            record
        )

        if (
            record["act_status"] == "success"
            and
            record["generation_status"] == "complete"
        ):
            completed_tasks.add(
                task_key
            )

    del model
    del tokenizer

    gc.collect()

    print(
        f"\n{model_name} completed."
    )


# ============================================================
# 18. Main
# ============================================================

def main() -> None:
    if not DATASET_PATH.exists():
        raise FileNotFoundError(
            DATASET_PATH
        )

    dataset = pd.read_csv(
        DATASET_PATH,
        keep_default_na=False,
    )

    required = {
        "No",
        "Adapted Question",
        "Correct Answer",
    }

    missing = (
        required
        - set(
            dataset.columns
        )
    )

    if missing:
        raise ValueError(
            f"Missing columns: "
            f"{sorted(missing)}"
        )

    completed_tasks = (
        load_completed_tasks()
    )

    print(
        f"Questions: "
        f"{len(dataset)}"
    )

    print(
        f"Previously completed: "
        f"{len(completed_tasks)}"
    )

    print(
        f"Student max tokens: "
        f"{STUDENT_MAX_TOKENS}"
    )

    print(
        f"Tutor max turns: "
        f"{TUTOR_MAX_TURNS}"
    )

    for model_name, model_id in (
        STUDENT_MODELS.items()
    ):
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
        "Student-Tutor experiment completed."
    )

    print(
        f"Results saved to:\n"
        f"{OUTPUT_PATH}"
    )

    print(
        "=" * 70
    )


# ============================================================
# 19. Entry Point
# ============================================================

if __name__ == "__main__":
    main()