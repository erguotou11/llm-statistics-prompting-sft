import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 0. Plot switches
# ============================================================

PLOT_WEIGHTED_SCORE = True
PLOT_DIMENSION_CHANGES = True


# ============================================================
# 1. File paths
# ============================================================

data_path = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis/sft.csv"
)

output_dir = data_path.parent


# ============================================================
# 2. Load data
# ============================================================

df = pd.read_csv(data_path)

print("\nColumns:")
print(df.columns.tolist())

print("\nData:")
print(df)


# ============================================================
# 3. Basic settings
# ============================================================

MODEL_ORDER = [
    "Llama3-8B",
    "Qwen2.5-7B",
    "DeepSeek-R1-7B"
]

# Direct = Base Model / Pre-SFT
PRE_LABEL = "Direct"

# SFT-Direct = Post-SFT
POST_LABEL = "SFT-Direct"


# ============================================================
# 4. Evaluation dimensions
# ============================================================

DIMENSIONS = {
    "Correctness": "avg_correct_score",
    "Reasoning": "avg_reasoning_score",
    "Logic": "avg_logical_score",
    "Clarity": "avg_explanation_score",
    "Interpretation": "avg_statistics_interpretation_score"
}


# ============================================================
# 5. Convert score columns to numeric
# ============================================================

numeric_columns = [
    "avg_weighted_score",
    "avg_correct_score",
    "avg_reasoning_score",
    "avg_logical_score",
    "avg_explanation_score",
    "avg_statistics_interpretation_score"
]

for col in numeric_columns:

    df[col] = pd.to_numeric(
        df[col],
        errors="coerce"
    )


# ============================================================
# 6. Prepare Pre-SFT and Post-SFT data
# ============================================================

pre_df = (
    df[
        df["prompt_strategy"] == PRE_LABEL
    ]
    .set_index("model_name")
    .reindex(MODEL_ORDER)
)

post_df = (
    df[
        df["prompt_strategy"] == POST_LABEL
    ]
    .set_index("model_name")
    .reindex(MODEL_ORDER)
)


# ============================================================
# Basic validation
# ============================================================

if pre_df["avg_weighted_score"].isna().any():

    print(
        "\nWARNING: Missing Pre-SFT data for one or more models."
    )

if post_df["avg_weighted_score"].isna().any():

    print(
        "\nWARNING: Missing Post-SFT data for one or more models."
    )


# ============================================================
# FIGURE 1
# Pre-SFT vs Post-SFT Weighted Score
# ============================================================

if PLOT_WEIGHTED_SCORE:

    pre_scores = pre_df[
        "avg_weighted_score"
    ].values

    post_scores = post_df[
        "avg_weighted_score"
    ].values

    weighted_improvements = (
        post_scores
        - pre_scores
    )


    # --------------------------------------------------------
    # Create figure
    # --------------------------------------------------------

    fig, ax = plt.subplots(
        figsize=(10, 5.5)
    )

    x = np.arange(
        len(MODEL_ORDER)
    )

    width = 0.32


    # --------------------------------------------------------
    # Draw bars
    # --------------------------------------------------------

    bars_pre = ax.bar(
        x - width / 2,
        pre_scores,
        width,
        label="Pre-SFT"
    )

    bars_post = ax.bar(
        x + width / 2,
        post_scores,
        width,
        label="Post-SFT"
    )


    # --------------------------------------------------------
    # Pre-SFT value labels
    # --------------------------------------------------------

    for bar, value in zip(
        bars_pre,
        pre_scores
    ):

        ax.text(
            bar.get_x()
            + bar.get_width() / 2,

            value + 0.035,

            f"{value:.2f}",

            ha="center",
            va="bottom",

            fontsize=10
        )


    # --------------------------------------------------------
    # Post-SFT value labels + delta
    # --------------------------------------------------------

    for bar, value, improvement in zip(
        bars_post,
        post_scores,
        weighted_improvements
    ):

        # Post-SFT score
        ax.text(
            bar.get_x()
            + bar.get_width() / 2,

            value + 0.035,

            f"{value:.2f}",

            ha="center",
            va="bottom",

            fontsize=10
        )

        # Delta
        ax.text(
            bar.get_x()
            + bar.get_width() / 2,

            value + 0.20,

            f"$\\Delta$ {improvement:+.2f}",

            ha="center",
            va="bottom",

            fontsize=10
        )


    # --------------------------------------------------------
    # Formatting
    # --------------------------------------------------------

    ax.set_ylabel(
        "Mean Weighted Score (0–5)",
        fontsize=11
    )

    ax.set_xlabel(
        "Student Model",
        fontsize=11
    )

    ax.set_xticks(x)

    ax.set_xticklabels(
        MODEL_ORDER,
        fontsize=10
    )

    ax.set_ylim(
        0,
        5.15
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


    # --------------------------------------------------------
    # Legend
    # --------------------------------------------------------

    ax.legend(
        loc="upper center",

        bbox_to_anchor=(
            0.5,
            1.10
        ),

        ncol=2,

        frameon=False,

        fontsize=10,

        handlelength=1.8,
        handleheight=1.1,

        columnspacing=2.2
    )


    # --------------------------------------------------------
    # Layout
    # --------------------------------------------------------

    plt.tight_layout(
        rect=[
            0,
            0,
            1,
            0.95
        ]
    )


    # --------------------------------------------------------
    # Save
    # --------------------------------------------------------

    output_path_1 = (
        output_dir
        / "pre_post_sft_weighted_score.png"
    )

    plt.savefig(
        output_path_1,

        dpi=300,

        bbox_inches="tight",

        pad_inches=0.08
    )

    plt.show()

    plt.close(fig)

    print(
        f"\nFigure 1 saved to:\n"
        f"{output_path_1}"
    )


# ============================================================
# FIGURE 2
# Evaluation-Dimension Changes
#
# Post-SFT minus Pre-SFT
# ============================================================

if PLOT_DIMENSION_CHANGES:

    change_data = {}


    # --------------------------------------------------------
    # Calculate score differences
    # --------------------------------------------------------

    for model in MODEL_ORDER:

        changes = []

        for dimension, column in DIMENSIONS.items():

            pre_value = pre_df.loc[
                model,
                column
            ]

            post_value = post_df.loc[
                model,
                column
            ]

            change = (
                post_value
                - pre_value
            )

            changes.append(
                change
            )

        change_data[
            model
        ] = changes


    # --------------------------------------------------------
    # Print differences
    # --------------------------------------------------------

    print(
        "\n============================================"
    )

    print(
        "Post-SFT minus Pre-SFT Dimension Changes"
    )

    print(
        "============================================"
    )

    for model in MODEL_ORDER:

        print(
            f"\n{model}"
        )

        for dimension, value in zip(
            DIMENSIONS.keys(),
            change_data[model]
        ):

            print(
                f"{dimension}: {value:+.2f}"
            )


    # --------------------------------------------------------
    # Create figure
    # --------------------------------------------------------

    fig, axes = plt.subplots(
        nrows=1,
        ncols=3,

        figsize=(15, 5.5),

        sharey=True
    )

    dimension_names = list(
        DIMENSIONS.keys()
    )

    x = np.arange(
        len(dimension_names)
    )


    # --------------------------------------------------------
    # Plot each model
    # --------------------------------------------------------

    for ax, model in zip(
        axes,
        MODEL_ORDER
    ):

        values = np.array(
            change_data[model]
        )


        # ----------------------------------------------------
        # Draw bars
        # ----------------------------------------------------

        bars = ax.bar(
            x,
            values,
            width=0.65
        )


        # ----------------------------------------------------
        # Value labels
        # ----------------------------------------------------

        for bar, value in zip(
            bars,
            values
        ):

            if value >= 0:

                label_y = (
                    value + 0.035
                )

                va = "bottom"

            else:

                label_y = (
                    value - 0.035
                )

                va = "top"


            ax.text(
                bar.get_x()
                + bar.get_width() / 2,

                label_y,

                f"{value:+.2f}",

                ha="center",
                va=va,

                fontsize=9.5
            )


        # ----------------------------------------------------
        # Zero line
        # ----------------------------------------------------

        ax.axhline(
            y=0,
            linewidth=1
        )


        # ----------------------------------------------------
        # Panel title
        # ----------------------------------------------------

        ax.set_title(
            model,
            fontsize=11
        )


        # ----------------------------------------------------
        # X-axis
        # ----------------------------------------------------

        ax.set_xticks(x)

        ax.set_xticklabels(
            [
                "Correctness",
                "Reasoning",
                "Logic",
                "Clarity",
                "Interpretation"
            ],

            rotation=25,

            ha="right",

            fontsize=8.5
        )


        # ----------------------------------------------------
        # Grid
        # ----------------------------------------------------

        ax.grid(
            axis="y",
            linestyle="--",
            linewidth=0.6,
            alpha=0.25
        )


        # ----------------------------------------------------
        # Remove unnecessary borders
        # ----------------------------------------------------

        ax.spines[
            "top"
        ].set_visible(False)

        ax.spines[
            "right"
        ].set_visible(False)


    # ========================================================
    # Shared Y-axis label
    # ========================================================

    axes[0].set_ylabel(
        "Score Change (Post-SFT − Pre-SFT)",
        fontsize=11
    )


    # ========================================================
    # Shared Y-axis range
    # ========================================================

    axes[0].set_ylim(
        -0.40,
        1.05
    )


    # ========================================================
    # Shared X-axis label
    # ========================================================

    fig.supxlabel(
        "Evaluation Dimension",

        fontsize=11,

        y=0.02
    )


    # ========================================================
    # Layout
    # ========================================================

    plt.tight_layout(
        rect=[
            0,
            0.05,
            1,
            1
        ]
    )


    # ========================================================
    # Save
    # ========================================================

    output_path_2 = (
        output_dir
        / "pre_post_sft_dimension_changes.png"
    )

    plt.savefig(
        output_path_2,

        dpi=300,

        bbox_inches="tight",

        pad_inches=0.08
    )

    plt.show()

    plt.close(fig)

    print(
        f"\nFigure 2 saved to:\n"
        f"{output_path_2}"
    )


# ============================================================
# Finished
# ============================================================

print(
    "\nAll SFT figures generated successfully."
)