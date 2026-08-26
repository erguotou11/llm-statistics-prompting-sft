import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 0. Plot switches
# ============================================================

PLOT_CORRECTNESS_GAIN = True
PLOT_REASONING_GAIN = True


# ============================================================
# 1. Paths
# ============================================================

data_path = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis/question_type_score.csv"
)

output_dir = data_path.parent


# ============================================================
# 2. Load data
# ============================================================

df = pd.read_csv(data_path)

print("\nColumns:")
print(df.columns.tolist())

print("\nQuestion types:")
print(df["question_type"].unique())

print("\nPrompt strategies:")
print(df["prompt_strategy"].unique())


# ============================================================
# 3. Standardise prompting names
# ============================================================

df["prompt_strategy"] = df["prompt_strategy"].replace({
    "Chain-of-Thought": "CoT",
    "Student-Tutor": "Student–Tutor",
    "Student--Tutor": "Student–Tutor"
})


# ============================================================
# 4. Orders
# ============================================================

MODEL_ORDER = [
    "Llama3-8B",
    "Qwen2.5-7B",
    "DeepSeek-R1-7B"
]

QUESTION_TYPE_ORDER = [
    "Calculation",
    "Interpretation",
    "Multiple Choice",
    "Short Answer",
    "Single Choice"
]

PROMPT_ORDER = [
    "CoT",
    "Student–Tutor"
]


# ============================================================
# 5. Calculate gains relative to Direct
# ============================================================

records = []

for model in MODEL_ORDER:

    model_df = df[
        df["model_name"] == model
    ]

    for question_type in QUESTION_TYPE_ORDER:

        task_df = model_df[
            model_df["question_type"] == question_type
        ]

        direct_rows = task_df[
            task_df["prompt_strategy"] == "Direct"
        ]

        if direct_rows.empty:
            print(
                f"WARNING: Missing Direct for "
                f"{model} / {question_type}"
            )
            continue

        direct = direct_rows.iloc[0]

        for prompt in PROMPT_ORDER:

            prompt_rows = task_df[
                task_df["prompt_strategy"] == prompt
            ]

            if prompt_rows.empty:
                print(
                    f"WARNING: Missing {prompt} for "
                    f"{model} / {question_type}"
                )
                continue

            current = prompt_rows.iloc[0]

            correct_gain = (
                float(current["avg_correct_score"])
                - float(direct["avg_correct_score"])
            )

            reasoning_gain = (
                float(current["avg_reasoning_score"])
                - float(direct["avg_reasoning_score"])
            )

            records.append({
                "model_name": model,
                "question_type": question_type,
                "prompt_strategy": prompt,
                "correct_gain": correct_gain,
                "reasoning_gain": reasoning_gain
            })


gain_df = pd.DataFrame(records)


# ============================================================
# 6. Save calculated gains
# ============================================================

gain_output = (
    output_dir
    / "question_type_dimension_gains.csv"
)

gain_df.to_csv(
    gain_output,
    index=False
)

print("\n============================================")
print("Question-type gains")
print("============================================")

print(
    gain_df
    .round(3)
    .to_string(index=False)
)

print(
    f"\nGain data saved to:\n{gain_output}"
)


# ============================================================
# 7. Common plotting function
# ============================================================

def plot_gain_by_question_type(
    metric_column,
    ylabel,
    output_filename
):

    # --------------------------------------------------------
    # 3 panels:
    # Llama / Qwen / DeepSeek
    # --------------------------------------------------------

    fig, axes = plt.subplots(
        nrows=1,
        ncols=3,
        figsize=(15, 5.5),
        sharey=True
    )

    x = np.arange(
        len(QUESTION_TYPE_ORDER)
    )

    bar_width = 0.34

    all_values = gain_df[
        metric_column
    ].dropna()

    y_min = all_values.min()
    y_max = all_values.max()

    # --------------------------------------------------------
    # Dynamic Y-axis
    # --------------------------------------------------------

    top_padding = max(
        0.08,
        abs(y_max) * 0.15
    )

    if y_min < 0:
        bottom_padding = max(
            0.05,
            abs(y_min) * 0.25
        )

        y_lower = (
            y_min - bottom_padding
        )
    else:
        y_lower = 0

    y_upper = (
        y_max + top_padding
    )


    # --------------------------------------------------------
    # Draw one panel per model
    # --------------------------------------------------------

    for ax, model in zip(
        axes,
        MODEL_ORDER
    ):

        model_gain_df = gain_df[
            gain_df["model_name"] == model
        ].copy()

        for i, prompt in enumerate(
            PROMPT_ORDER
        ):

            prompt_df = model_gain_df[
                model_gain_df["prompt_strategy"] == prompt
            ].copy()

            prompt_df = (
                prompt_df
                .set_index("question_type")
                .reindex(QUESTION_TYPE_ORDER)
            )

            values = prompt_df[
                metric_column
            ].values

            positions = (
                x
                + (
                    -bar_width / 2
                    if i == 0
                    else bar_width / 2
                )
            )

            bars = ax.bar(
                positions,
                values,
                width=bar_width,
                label=(
                    "CoT vs Direct"
                    if prompt == "CoT"
                    else "Student–Tutor vs Direct"
                )
            )

            # ------------------------------------------------
            # Value labels
            # ------------------------------------------------

            for bar, value in zip(
                bars,
                values
            ):

                if pd.isna(value):
                    continue

                if value >= 0:

                    label_y = (
                        value + 0.015
                    )

                    va = "bottom"

                else:

                    label_y = (
                        value - 0.015
                    )

                    va = "top"

                ax.text(
                    bar.get_x()
                    + bar.get_width() / 2,

                    label_y,

                    f"{value:+.2f}",

                    ha="center",
                    va=va,

                    fontsize=8
                )


        # ----------------------------------------------------
        # Panel formatting
        # ----------------------------------------------------

        ax.set_title(
            model,
            fontsize=11
        )

        ax.set_xticks(x)

        ax.set_xticklabels(
            [
                "Calculation",
                "Interpretation",
                "Multiple\nChoice",
                "Short\nAnswer",
                "Single\nChoice"
            ],
            fontsize=8.5
        )

        ax.set_ylim(
            y_lower,
            y_upper
        )

        ax.axhline(
            0,
            linewidth=0.9
        )

        ax.grid(
            axis="y",
            linestyle="--",
            linewidth=0.6,
            alpha=0.25
        )

        ax.spines[
            "top"
        ].set_visible(False)

        ax.spines[
            "right"
        ].set_visible(False)


    # ========================================================
    # Shared Y label
    # ========================================================

    axes[0].set_ylabel(
        ylabel,
        fontsize=11
    )


    # ========================================================
    # Shared X label
    # ========================================================

    fig.supxlabel(
        "Question Type",
        fontsize=11,
        y=0.03
    )


    # ========================================================
    # Shared legend
    # ========================================================

    handles, labels = (
        axes[0]
        .get_legend_handles_labels()
    )

    fig.legend(
        handles,
        labels,

        loc="upper center",

        ncol=2,

        frameon=False,

        fontsize=9,

        bbox_to_anchor=(
            0.5,
            1.00
        )
    )


    # ========================================================
    # Layout
    # ========================================================

    plt.tight_layout(
        rect=[
            0,
            0.05,
            1,
            0.94
        ]
    )


    # ========================================================
    # Save
    # ========================================================

    output_path = (
        output_dir
        / output_filename
    )

    plt.savefig(
        output_path,
        dpi=300,
        bbox_inches="tight",
        pad_inches=0.08
    )

    plt.show()

    plt.close(fig)

    print(
        f"\nFigure saved to:\n"
        f"{output_path}"
    )


# ============================================================
# 8. Figure 1:
# Correctness improvement by question type
# ============================================================

if PLOT_CORRECTNESS_GAIN:

    plot_gain_by_question_type(
        metric_column="correct_gain",

        ylabel=(
            "Improvement in Answer Correctness"
        ),

        output_filename=(
            "correctness_gain_by_question_type.png"
        )
    )


# ============================================================
# 9. Figure 2:
# Reasoning improvement by question type
# ============================================================

if PLOT_REASONING_GAIN:

    plot_gain_by_question_type(
        metric_column="reasoning_gain",

        ylabel=(
            "Improvement in Reasoning Completeness"
        ),

        output_filename=(
            "reasoning_gain_by_question_type.png"
        )
    )