import pymysql
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 0. Figure switches
# ============================================================

PLOT_FIVE_DIMENSION_HEATMAP = True
PLOT_CORRECT_REASONING_FULL_SCORE = True
PLOT_CORRECT_REASONING_ALIGNMENT = True


# ============================================================
# 1. MySQL configuration
# ============================================================

MYSQL_USER = "root"
MYSQL_PASSWORD = "12345678"
MYSQL_HOST = "localhost"
MYSQL_PORT = 3306
MYSQL_DATABASE = "paper"

# 如果你的真实表名还是 eval，就改成：
# TABLE_NAME = "eval"
TABLE_NAME = "eval"


# ============================================================
# 2. Output directory
# ============================================================

OUTPUT_DIR = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis"
)

OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


# ============================================================
# 3. Experiment order
# ============================================================

MODEL_ORDER = [
    "Llama3-8B",
    "Qwen2.5-7B",
    "DeepSeek-R1-7B"
]

PROMPT_ORDER = [
    "Direct",
    "CoT",
    "Student–Tutor"
]


# ============================================================
# 4. Connect to MySQL
# ============================================================

conn = pymysql.connect(
    host=MYSQL_HOST,
    port=MYSQL_PORT,
    user=MYSQL_USER,
    password=MYSQL_PASSWORD,
    database=MYSQL_DATABASE,
    charset="utf8mb4"
)


# ============================================================
# 5. Read evaluation data
# ============================================================

sql = f"""
SELECT
    model_name,
    prompt_strategy,
    correct_score,
    reasoning_score,
    logical_score,
    explanation_score,
    statistics_interpretation_score,
    weighted_score
FROM {TABLE_NAME}
WHERE model_name IN (
    'Llama3-8B',
    'Qwen2.5-7B',
    'DeepSeek-R1-7B'
)
AND prompt_strategy IN (
    'Direct',
    'CoT',
    'Student-Tutor'  
)
"""

df = pd.read_sql(sql, conn)

conn.close()


# ============================================================
# 6. Standardise prompt names
# ============================================================
#
# df["prompt_strategy"] = df["prompt_strategy"].replace({
#     "Chain-of-Thought": "CoT",
#     "Student-Tutor": "Student–Tutor",
#     "Student--Tutor": "Student–Tutor"
# })


# Convert score fields to numeric
score_columns = [
    "correct_score",
    "reasoning_score",
    "logical_score",
    "explanation_score",
    "statistics_interpretation_score",
    "weighted_score"
]

for col in score_columns:
    df[col] = pd.to_numeric(df[col], errors="coerce")


print("Rows loaded:", len(df))

print(
    df.groupby(
        ["model_name", "prompt_strategy"]
    ).size()
)


# ============================================================
# 7. Common combination order
# ============================================================

combination_order = []

for model in MODEL_ORDER:
    for prompt in PROMPT_ORDER:
        combination_order.append(
            f"{model}\n{prompt}"
        )


# ============================================================
# FIGURE 1
# Five-dimensional evaluation heatmap
# ============================================================

if PLOT_FIVE_DIMENSION_HEATMAP:

    dimensions = {
        "Correctness": "correct_score",
        "Reasoning": "reasoning_score",
        "Logic": "logical_score",
        "Clarity": "explanation_score",
        "Interpretation": "statistics_interpretation_score"
    }

    # Aggregate mean scores
    heatmap_df = (
        df.groupby(
            ["model_name", "prompt_strategy"]
        )[list(dimensions.values())]
        .mean()
        .reset_index()
    )

    heatmap_df["combination"] = (
        heatmap_df["model_name"]
        + "\n"
        + heatmap_df["prompt_strategy"]
    )

    heatmap_df["combination"] = pd.Categorical(
        heatmap_df["combination"],
        categories=combination_order,
        ordered=True
    )

    heatmap_df = (
        heatmap_df
        .sort_values("combination")
        .set_index("combination")
    )

    heatmap_matrix = heatmap_df[
        list(dimensions.values())
    ].values

    # --------------------------------------------------------
    # Plot
    # --------------------------------------------------------

    fig, ax = plt.subplots(figsize=(9.2, 7.2))

    im = ax.imshow(
        heatmap_matrix,
        aspect="auto",
        vmin=0,
        vmax=5
    )

    # Axis labels
    ax.set_xticks(
        np.arange(len(dimensions))
    )

    ax.set_xticklabels(
        list(dimensions.keys()),
        fontsize=10
    )

    ax.set_yticks(
        np.arange(len(heatmap_df.index))
    )

    ax.set_yticklabels(
        heatmap_df.index,
        fontsize=9
    )

    # Values inside cells
    for i in range(
        heatmap_matrix.shape[0]
    ):
        for j in range(
            heatmap_matrix.shape[1]
        ):
            value = heatmap_matrix[i, j]

            ax.text(
                j,
                i,
                f"{value:.2f}",
                ha="center",
                va="center",
                fontsize=9
            )

    # Colour bar
    cbar = fig.colorbar(
        im,
        ax=ax,
        fraction=0.035,
        pad=0.03
    )

    cbar.set_label(
        "Mean Score (0–5)",
        fontsize=10
    )

    ax.set_xlabel(
        "Evaluation Dimension",
        fontsize=11
    )

    ax.set_ylabel(
        "Model and Prompting Strategy",
        fontsize=11
    )

    plt.tight_layout()

    output_path = (
        OUTPUT_DIR
        / "five_dimension_evaluation_heatmap.png"
    )

    plt.savefig(
        output_path,
        dpi=300,
        bbox_inches="tight"
    )

    plt.show()
    plt.close(fig)

    print(
        f"\nHeatmap saved to:\n{output_path}"
    )


# ============================================================
# FIGURE 2
# Full-score rates:
# Correctness = 5 and Reasoning = 5
# ============================================================

if PLOT_CORRECT_REASONING_FULL_SCORE:

    full_score_df = (
        df.groupby(
            ["model_name", "prompt_strategy"]
        )
        .agg(
            total_questions=(
                "correct_score",
                "count"
            ),
            correctness_full=(
                "correct_score",
                lambda x: (x == 5).sum()
            ),
            reasoning_full=(
                "reasoning_score",
                lambda x: (x == 5).sum()
            )
        )
        .reset_index()
    )

    full_score_df[
        "correctness_full_rate"
    ] = (
        full_score_df["correctness_full"]
        / full_score_df["total_questions"]
        * 100
    )

    full_score_df[
        "reasoning_full_rate"
    ] = (
        full_score_df["reasoning_full"]
        / full_score_df["total_questions"]
        * 100
    )

    full_score_df["combination"] = (
        full_score_df["model_name"]
        + "\n"
        + full_score_df["prompt_strategy"]
    )

    full_score_df["combination"] = pd.Categorical(
        full_score_df["combination"],
        categories=combination_order,
        ordered=True
    )

    full_score_df = (
        full_score_df
        .sort_values("combination")
    )

    # Export values for later writing
    full_score_df.to_csv(
        OUTPUT_DIR
        / "correctness_reasoning_full_score_rates.csv",
        index=False
    )

    # --------------------------------------------------------
    # Plot grouped bar chart
    # --------------------------------------------------------

    fig, ax = plt.subplots(
        figsize=(11, 5.8)
    )

    x = np.arange(
        len(full_score_df)
    )

    bar_width = 0.36

    bars_correct = ax.bar(
        x - bar_width / 2,
        full_score_df[
            "correctness_full_rate"
        ],
        width=bar_width,
        label="Correctness = 5"
    )

    bars_reason = ax.bar(
        x + bar_width / 2,
        full_score_df[
            "reasoning_full_rate"
        ],
        width=bar_width,
        label="Reasoning = 5"
    )

    # Value labels
    for bars in [
        bars_correct,
        bars_reason
    ]:
        for bar in bars:
            value = bar.get_height()

            ax.text(
                bar.get_x()
                + bar.get_width() / 2,
                value + 1,
                f"{value:.1f}%",
                ha="center",
                va="bottom",
                fontsize=8
            )

    ax.set_xticks(x)

    ax.set_xticklabels(
        full_score_df["combination"],
        fontsize=8
    )

    ax.set_ylabel(
        "Full-score Rate (%)",
        fontsize=11
    )

    ax.set_xlabel(
        "Model and Prompting Strategy",
        fontsize=11
    )

    ax.set_ylim(
        0,
        105
    )

    ax.legend(
        frameon=False,
        fontsize=9
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

    plt.tight_layout()

    output_path = (
        OUTPUT_DIR
        / "correctness_reasoning_full_score_rates.png"
    )

    plt.savefig(
        output_path,
        dpi=300,
        bbox_inches="tight"
    )

    plt.show()
    plt.close(fig)

    print(
        f"\nCorrectness/Reasoning figure saved to:\n"
        f"{output_path}"
    )


# ============================================================
# FIGURE 3
# Correctness–Reasoning alignment
#
# Both = 5:
# correct_score = 5 AND reasoning_score = 5
#
# Correct only:
# correct_score = 5 AND reasoning_score < 5
# ============================================================

if PLOT_CORRECT_REASONING_ALIGNMENT:

    temp_df = df.copy()

    temp_df[
        "both_full"
    ] = (
        (temp_df["correct_score"] == 5)
        &
        (temp_df["reasoning_score"] == 5)
    )

    temp_df[
        "correct_only"
    ] = (
        (temp_df["correct_score"] == 5)
        &
        (temp_df["reasoning_score"] < 5)
    )

    alignment_df = (
        temp_df.groupby(
            ["model_name", "prompt_strategy"]
        )
        .agg(
            total_questions=(
                "correct_score",
                "count"
            ),
            both_full=(
                "both_full",
                "sum"
            ),
            correct_only=(
                "correct_only",
                "sum"
            )
        )
        .reset_index()
    )

    alignment_df[
        "both_full_rate"
    ] = (
        alignment_df["both_full"]
        / alignment_df["total_questions"]
        * 100
    )

    alignment_df[
        "correct_only_rate"
    ] = (
        alignment_df["correct_only"]
        / alignment_df["total_questions"]
        * 100
    )

    alignment_df["combination"] = (
        alignment_df["model_name"]
        + "\n"
        + alignment_df["prompt_strategy"]
    )

    alignment_df["combination"] = pd.Categorical(
        alignment_df["combination"],
        categories=combination_order,
        ordered=True
    )

    alignment_df = (
        alignment_df
        .sort_values("combination")
    )

    alignment_df.to_csv(
        OUTPUT_DIR
        / "correctness_reasoning_alignment.csv",
        index=False
    )

    # --------------------------------------------------------
    # Plot
    # --------------------------------------------------------

    fig, ax = plt.subplots(
        figsize=(11, 5.8)
    )

    x = np.arange(
        len(alignment_df)
    )

    bar_width = 0.36

    bars_both = ax.bar(
        x - bar_width / 2,
        alignment_df[
            "both_full_rate"
        ],
        width=bar_width,
        label="Correctness = 5 & Reasoning = 5"
    )

    bars_correct_only = ax.bar(
        x + bar_width / 2,
        alignment_df[
            "correct_only_rate"
        ],
        width=bar_width,
        label="Correctness = 5 & Reasoning < 5"
    )

    for bars in [
        bars_both,
        bars_correct_only
    ]:
        for bar in bars:
            value = bar.get_height()

            ax.text(
                bar.get_x()
                + bar.get_width() / 2,
                value + 1,
                f"{value:.1f}%",
                ha="center",
                va="bottom",
                fontsize=8
            )

    ax.set_xticks(x)

    ax.set_xticklabels(
        alignment_df["combination"],
        fontsize=8
    )

    ax.set_ylabel(
        "Percentage of Questions (%)",
        fontsize=11
    )

    ax.set_xlabel(
        "Model and Prompting Strategy",
        fontsize=11
    )

    ax.set_ylim(
        0,
        105
    )

    ax.legend(
        frameon=False,
        fontsize=9
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

    plt.tight_layout()

    output_path = (
        OUTPUT_DIR
        / "correctness_reasoning_alignment.png"
    )

    plt.savefig(
        output_path,
        dpi=300,
        bbox_inches="tight"
    )

    plt.show()
    plt.close(fig)

    print(
        f"\nCorrectness–Reasoning alignment figure saved to:\n"
        f"{output_path}"
    )