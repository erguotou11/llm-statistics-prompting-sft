#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
OpenIntro Exercise Parser

This script extracts OpenIntro Statistics end-of-chapter exercises from
review_exercises.tex files and creates a clean master dataset.

Output:
    ~/Desktop/openintro_master.csv
    ~/Desktop/openintro_master.jsonl

Final fields:
    id
    source
    chapter
    openintro_exercise_no
    local_exercise_no
    part_no
    title
    task
    context
    sub_question
    original_text
    raw_latex

Field generation logic:
    id:
        Generated automatically using chapter + openintro_exercise_no + part_no.
    source:
        Constant value: OpenIntro Statistics.
    chapter:
        Extracted from the folder name in the OpenIntro repository.
    openintro_exercise_no:
        Extracted from the preceding LaTeX comment, e.g. % 35.
    local_exercise_no:
        Sequential exercise index within the current review_exercises.tex file.
    part_no:
        Generated from the order of \\item entries within the parts environment.
        Empty if the exercise has no sub-questions.
    title:
        Extracted from \\qt{} after removing LaTeX labels and formatting commands.
    task:
        Automatically classified using keyword-based rules.
    context:
        Extracted from the exercise body after removing the title and parts environment,
        followed by LaTeX cleaning.
    sub_question:
        Extracted from each \\item within the parts environment.
        Empty if the exercise has only one question.
    original_text:
        Constructed by concatenating the cleaned title, context, and sub_question
        if applicable. No prompt engineering is applied at this stage.
    raw_latex:
        Directly preserved from the \\eoce{} block for traceability, debugging,
        and future re-parsing.
"""

import csv
import json
import re
import subprocess
from pathlib import Path


# ============================================================
# 1. Settings
# ============================================================

REPO_URL = "https://github.com/OpenIntroStat/openintro-statistics.git"

DESKTOP = Path.home() / "Desktop"
REPO_DIR = DESKTOP / "openintro-statistics"

CSV_OUTPUT = DESKTOP / "openintro_master.csv"
JSONL_OUTPUT = DESKTOP / "openintro_master.jsonl"

FIELDNAMES = [
    "id",
    "source",
    "chapter",
    "openintro_exercise_no",
    "local_exercise_no",
    "part_no",
    "title",
    "task",
    "context",
    "sub_question",
    "original_text",
    "raw_latex",
]


# ============================================================
# 2. Download / update repository
# ============================================================

def clone_or_update_repo() -> None:
    if not REPO_DIR.exists():
        print(f"Cloning OpenIntro repository to {REPO_DIR} ...")
        subprocess.run(["git", "clone", REPO_URL, str(REPO_DIR)], check=True)
    else:
        print(f"Repository already exists: {REPO_DIR}")
        print("Pulling latest changes...")
        subprocess.run(["git", "-C", str(REPO_DIR), "pull"], check=False)


# ============================================================
# 3. General LaTeX helpers
# ============================================================

def remove_latex_comments(text: str) -> str:
    """
    Remove LaTeX comments.
    Escaped percent signs such as \\% are preserved.
    """
    return re.sub(r"(?<!\\)%.*", "", text)


def remove_latex_labels(text: str) -> str:
    """
    Remove LaTeX labels such as \\label{xxx}.
    """
    return re.sub(r"\\label\{[^{}]*\}", "", text)


def clean_basic_latex(text: str) -> str:
    """
    Convert common LaTeX markup into readable plain text.
    This is intentionally conservative.
    """
    if not text:
        return ""

    text = remove_latex_comments(text)
    text = remove_latex_labels(text)

    # Commands where the argument should be preserved.
    keep_arg_commands = [
        "textit",
        "textbf",
        "emph",
        "underline",
        "texttt",
        "mathrm",
        "mathbf",
        "mathit",
        "mbox",
        "url",
    ]

    for cmd in keep_arg_commands:
        text = re.sub(rf"\\{cmd}\{{([^{{}}]*)\}}", r"\1", text)

    # Citations / references
    text = re.sub(r"\\footfullcite\{([^{}]*)\}", r"[source: \1]", text)
    text = re.sub(r"\\cite\{([^{}]*)\}", r"[source: \1]", text)
    text = re.sub(r"\\ref\{([^{}]*)\}", r"[ref: \1]", text)

    # Escaped characters
    replacements = {
        r"\%": "%",
        r"\$": "$",
        r"\_": "_",
        r"\&": "&",
        r"\#": "#",
        r"\{": "{",
        r"\}": "}",
    }
    for old, new in replacements.items():
        text = text.replace(old, new)

    # Remove simple spacing commands
    text = re.sub(r"\\vspace\*?\{[^{}]*\}", "", text)
    text = re.sub(r"\\hspace\*?\{[^{}]*\}", "", text)

    # Remove remaining one-argument commands while preserving argument
    text = re.sub(
        r"\\[a-zA-Z]+\*?(?:\[[^\]]*\])?\{([^{}]*)\}",
        r"\1",
        text,
    )

    # Remove remaining commands
    text = re.sub(r"\\[a-zA-Z]+\*?(?:\[[^\]]*\])?", "", text)

    # Remove remaining braces
    text = text.replace("{", "").replace("}", "")

    # Whitespace cleanup
    text = re.sub(r"[ \t]+", " ", text)
    text = re.sub(r"\n\s*\n+", "\n\n", text)

    return text.strip()


# ============================================================
# 4. Table handling
# ============================================================

def clean_table_cell(cell: str) -> str:
    cell = clean_basic_latex(cell)
    cell = re.sub(r"\s+", " ", cell)
    return cell.strip()


def latex_tabular_to_markdown(tabular_latex: str) -> str:
    """
    Convert a simple LaTeX tabular block to a Markdown table.

    Complex tables may not be perfect, but raw_latex is preserved so the
    original can always be checked later.
    """
    text = tabular_latex

    text = re.sub(r"\\begin\{tabular\}\{[^{}]*\}", "", text)
    text = re.sub(r"\\end\{tabular\}", "", text)

    # Table rules
    text = re.sub(r"\\toprule|\\midrule|\\bottomrule|\\hline", "", text)
    text = re.sub(r"\\cline\{[^{}]*\}", "", text)

    # Keep multicolumn contents
    text = re.sub(
        r"\\multicolumn\{[^{}]*\}\{[^{}]*\}\{([^{}]*)\}",
        r"\1",
        text,
    )

    raw_rows = re.split(r"\\\\", text)
    rows = []

    for raw_row in raw_rows:
        raw_row = raw_row.strip()
        if not raw_row:
            continue

        cells = [clean_table_cell(c) for c in raw_row.split("&")]
        cells = [c for c in cells if c != ""]

        if cells:
            rows.append(cells)

    if not rows:
        return ""

    max_cols = max(len(row) for row in rows)
    rows = [row + [""] * (max_cols - len(row)) for row in rows]

    md_lines = []
    md_lines.append("| " + " | ".join(rows[0]) + " |")
    md_lines.append("| " + " | ".join(["---"] * max_cols) + " |")

    for row in rows[1:]:
        md_lines.append("| " + " | ".join(row) + " |")

    return "\n".join(md_lines)


def replace_tabular_with_markdown(text: str) -> str:
    pattern = r"\\begin\{tabular\}\{[^{}]*\}.*?\\end\{tabular\}"

    def repl(match: re.Match) -> str:
        raw_table = match.group(0)
        md_table = latex_tabular_to_markdown(raw_table)
        if md_table:
            return "\n\nTable:\n" + md_table + "\n\n"
        return "\n\nTable LaTeX:\n" + raw_table + "\n\n"

    return re.sub(pattern, repl, text, flags=re.DOTALL)


def clean_latex_text(text: str) -> str:
    """
    Main text cleaner used for title, context, and sub_question.
    """
    if not text:
        return ""

    # Convert tables before removing commands.
    text = replace_tabular_with_markdown(text)

    # Remove common environments but keep contents.
    for env in ["center", "small", "footnotesize", "scriptsize"]:
        text = re.sub(rf"\\begin\{{{env}\}}", "", text)
        text = re.sub(rf"\\end\{{{env}\}}", "", text)

    # Convert LaTeX row breaks and remaining table separators.
    text = text.replace("\\\\", "\n")
    text = text.replace("&", " | ")

    text = clean_basic_latex(text)

    # Final whitespace cleanup.
    text = re.sub(r"[ \t]+", " ", text)
    text = re.sub(r"\n\s*\n+", "\n\n", text)

    return text.strip()


# ============================================================
# 5. Balanced block parser
# ============================================================

def extract_eoce_blocks_with_numbers(content: str) -> list[dict]:
    """
    Extract each \\eoce{...} block and its preceding OpenIntro exercise number.

    Example source:
        % 35
        \\eoce{...}

    openintro_exercise_no = 35
    """
    results = []
    pattern = r"\\eoce\{"

    for match in re.finditer(pattern, content):
        before = content[:match.start()]

        # Use the nearest previous comment line like "% 35".
        no_matches = re.findall(
            r"^\s*%\s*(\d+)\s*$",
            before,
            flags=re.MULTILINE,
        )
        openintro_no = no_matches[-1] if no_matches else ""

        start = match.end()
        brace_count = 1
        i = start

        while i < len(content) and brace_count > 0:
            ch = content[i]

            if ch == "\\":
                i += 2
                continue

            if ch == "{":
                brace_count += 1
            elif ch == "}":
                brace_count -= 1

            i += 1

        if brace_count == 0:
            block = content[start:i - 1]
            results.append(
                {
                    "openintro_exercise_no": openintro_no,
                    "raw_latex": block.strip(),
                }
            )

    return results


# ============================================================
# 6. Title parser
# ============================================================

def extract_qt_title(raw_latex: str) -> str:
    """
    Extract exercise title from \\qt{...}.

    Example:
        \\qt{Roulette winnings\\label{roulette_winnings}}
    becomes:
        Roulette winnings
    """
    marker = r"\qt{"
    start = raw_latex.find(marker)

    if start == -1:
        return ""

    i = start + len(marker)
    brace_count = 1
    chars = []

    while i < len(raw_latex) and brace_count > 0:
        ch = raw_latex[i]

        if ch == "\\":
            chars.append(ch)
            i += 1
            if i < len(raw_latex):
                chars.append(raw_latex[i])
            i += 1
            continue

        if ch == "{":
            brace_count += 1
        elif ch == "}":
            brace_count -= 1
            if brace_count == 0:
                break

        chars.append(ch)
        i += 1

    title_raw = "".join(chars)
    title_raw = remove_latex_labels(title_raw)
    return clean_latex_text(title_raw)


def remove_qt_title(raw_latex: str) -> str:
    """
    Remove the first \\qt{...} block from raw_latex.
    """
    marker = r"\qt{"
    start = raw_latex.find(marker)

    if start == -1:
        return raw_latex

    i = start + len(marker)
    brace_count = 1

    while i < len(raw_latex) and brace_count > 0:
        ch = raw_latex[i]

        if ch == "\\":
            i += 2
            continue

        if ch == "{":
            brace_count += 1
        elif ch == "}":
            brace_count -= 1

        i += 1

    return raw_latex[:start] + raw_latex[i:]


# ============================================================
# 7. parts / item parser
# ============================================================

def extract_parts_body(raw_latex: str) -> str:
    """
    Extract content inside the first \\begin{parts} ... \\end{parts}.
    """
    start_marker = r"\begin{parts}"
    end_marker = r"\end{parts}"

    start = raw_latex.find(start_marker)
    if start == -1:
        return ""

    body_start = start + len(start_marker)
    end = raw_latex.find(end_marker, body_start)

    if end == -1:
        return ""

    return raw_latex[body_start:end]


def remove_parts_environment(raw_latex: str) -> str:
    """
    Remove the first parts environment from raw_latex.
    """
    pattern = r"\\begin\{parts\}.*?\\end\{parts\}"
    return re.sub(pattern, "", raw_latex, flags=re.DOTALL)


def extract_sub_questions(raw_latex: str) -> list[str]:
    """
    Extract each \\item inside the parts environment.
    """
    parts_body = extract_parts_body(raw_latex)

    if not parts_body:
        return []

    raw_items = re.split(r"\\item", parts_body)
    sub_questions = []

    for raw_item in raw_items:
        raw_item = raw_item.strip()
        if not raw_item:
            continue

        cleaned = clean_latex_text(raw_item)
        if cleaned:
            sub_questions.append(cleaned)

    return sub_questions


# ============================================================
# 8. context / original_text
# ============================================================

def extract_context(raw_latex: str) -> str:
    """
    Extract context:
        exercise body after removing the title and parts environment.
    """
    text = remove_qt_title(raw_latex)
    text = remove_parts_environment(text)
    return clean_latex_text(text)


def build_original_text(title: str, context: str, sub_question: str) -> str:
    """
    Construct original_text:
        cleaned title + context + sub_question if applicable.

    No prompt engineering is applied at this stage.
    """
    chunks = []

    if title:
        chunks.append(title)

    if context:
        chunks.append(context)

    if sub_question:
        chunks.append(sub_question)

    return "\n\n".join(chunks).strip()


# ============================================================
# 9. Task classification
# ============================================================

def classify_task(text: str) -> str:
    """
    Coarse statistical task category using keyword-based rules.
    """
    t = text.lower()

    if any(k in t for k in ["confidence interval", "margin of error"]):
        return "confidence_interval"

    if any(k in t for k in [
        "hypothesis", "p-value", "p value", "significance level",
        "null hypothesis", "alternative hypothesis", "reject the null"
    ]):
        return "hypothesis_testing"

    if any(k in t for k in [
        "regression", "least squares", "slope", "intercept",
        "residual", "r-squared", "r squared"
    ]):
        return "regression"

    if any(k in t for k in ["correlation", "association"]):
        return "correlation"

    if any(k in t for k in ["anova", "analysis of variance"]):
        return "anova"

    if any(k in t for k in [
        "normal distribution", "normal model", "nearly normal",
        "normal probability"
    ]):
        return "normal_distribution"

    if any(k in t for k in [
        "probability", "probabilities", "chance", "expected value",
        "random variable"
    ]):
        return "probability"

    if any(k in t for k in [
        "binomial", "poisson", "geometric", "uniform distribution",
        "density", "distribution"
    ]):
        return "distribution"

    if any(k in t for k in ["sample", "sampling", "bootstrap", "simulation"]):
        return "sampling"

    if any(k in t for k in ["mean", "average", "standard deviation", "variance"]):
        return "mean_variance"

    return "unknown"


# ============================================================
# 10. Chapter extraction
# ============================================================

def get_chapter(file_path: Path) -> str:
    """
    Expected path:
        .../ch_probability/TeX/review_exercises.tex

    Returns:
        ch_probability
    """
    parts = list(file_path.parts)

    if "TeX" in parts:
        tex_idx = parts.index("TeX")
        if tex_idx > 0:
            return parts[tex_idx - 1]

    if len(parts) >= 3:
        return parts[-3]

    return "unknown"


# ============================================================
# 11. Parse one file
# ============================================================

def parse_review_exercises_file(file_path: Path) -> list[dict]:
    chapter = get_chapter(file_path)

    with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
        content = f.read()

    blocks = extract_eoce_blocks_with_numbers(content)
    records = []

    for local_idx, block_info in enumerate(blocks, start=1):
        raw_latex = block_info["raw_latex"]
        openintro_no = block_info["openintro_exercise_no"] or str(local_idx)

        title = extract_qt_title(raw_latex)
        context = extract_context(raw_latex)
        sub_questions = extract_sub_questions(raw_latex)

        if sub_questions:
            for part_idx, sub_q in enumerate(sub_questions, start=1):
                original_text = build_original_text(title, context, sub_q)
                task = classify_task(original_text)

                records.append(
                    {
                        "id": f"{chapter}_{openintro_no}_{part_idx}",
                        "source": "OpenIntro Statistics",
                        "chapter": chapter,
                        "openintro_exercise_no": openintro_no,
                        "local_exercise_no": local_idx,
                        "part_no": part_idx,
                        "title": title,
                        "task": task,
                        "context": context,
                        "sub_question": sub_q,
                        "original_text": original_text,
                        "raw_latex": raw_latex,
                    }
                )
        else:
            original_text = build_original_text(title, context, "")
            task = classify_task(original_text)

            records.append(
                {
                    "id": f"{chapter}_{openintro_no}",
                    "source": "OpenIntro Statistics",
                    "chapter": chapter,
                    "openintro_exercise_no": openintro_no,
                    "local_exercise_no": local_idx,
                    "part_no": "",
                    "title": title,
                    "task": task,
                    "context": context,
                    "sub_question": "",
                    "original_text": original_text,
                    "raw_latex": raw_latex,
                }
            )

    return records


# ============================================================
# 12. Save outputs
# ============================================================

def save_csv(records: list[dict], output_path: Path) -> None:
    with open(output_path, "w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=FIELDNAMES)
        writer.writeheader()

        for row in records:
            writer.writerow({field: row.get(field, "") for field in FIELDNAMES})


def save_jsonl(records: list[dict], output_path: Path) -> None:
    with open(output_path, "w", encoding="utf-8") as f:
        for row in records:
            f.write(json.dumps(row, ensure_ascii=False) + "\n")


# ============================================================
# 13. Main
# ============================================================

def main() -> None:
    clone_or_update_repo()

    review_files = sorted(REPO_DIR.rglob("review_exercises.tex"))
    print(f"Found {len(review_files)} review_exercises.tex files")

    all_records = []

    for file_path in review_files:
        print(f"Parsing: {file_path}")
        records = parse_review_exercises_file(file_path)
        print(f"  Extracted {len(records)} records")
        all_records.extend(records)

    # Remove empty records.
    all_records = [
        row for row in all_records
        if len(row.get("original_text", "").strip()) > 30
    ]

    save_csv(all_records, CSV_OUTPUT)
    save_jsonl(all_records, JSONL_OUTPUT)

    print("\nDone.")
    print(f"Total records: {len(all_records)}")
    print(f"CSV saved to: {CSV_OUTPUT}")
    print(f"JSONL saved to: {JSONL_OUTPUT}")

    print("\nPreview:")
    for row in all_records[:5]:
        print("\n--------------------")
        print("id:", row["id"])
        print("openintro_exercise_no:", row["openintro_exercise_no"])
        print("local_exercise_no:", row["local_exercise_no"])
        print("part_no:", row["part_no"])
        print("title:", row["title"])
        print("task:", row["task"])
        print("original_text:")
        print(row["original_text"][:500])


if __name__ == "__main__":
    main()
