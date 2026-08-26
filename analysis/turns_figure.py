import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 0. Plot switches
# ============================================================

PLOT_QUESTION_TYPE_TURNS = True
PLOT_PERFORMANCE_BY_TURNS = True
PLOT_HARD_LEVEL_TURNS = True


# ============================================================
# 1. File paths
# ============================================================

analysis_dir = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis"
)

# model_name + question_type + turns
question_type_path = (
    analysis_dir
    / "question_type_turns.csv"
)

# model_name + turns
performance_path = (
    analysis_dir
    / "turns.csv"
)

# model_name + hard_level + turns
hard_level_path = (
    analysis_dir
    / "hard_level_turns.csv"
)

output_dir = analysis_dir


# ============================================================
# 2. Basic settings
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

HARD_LEVEL_ORDER = [
    "Easy",
    "Medium",
    "Hard"
]

TURN_ORDER = [
    1,
    2,
    3
]


# ============================================================
# 3. Helper function
# Shared legend formatting for turn-distribution figures
# ============================================================

def add_turn_legend(fig, axes):

    handles, labels = (
        axes[0]
        .get_legend_handles_labels()
    )

    fig.legend(
        handles,
        labels,

        loc="upper center",
        ncol=3,
        frameon=False,

        fontsize=11,

        bbox_to_anchor=(
            0.5,
            0.98
        ),

        handlelength=1.8,
        handleheight=1.2,

        columnspacing=2.5,
        handletextpad=0.7
    )


# ============================================================
# FIGURE 1
# Interaction-turn distribution by question type
# ============================================================

if PLOT_QUESTION_TYPE_TURNS:

    # --------------------------------------------------------
    # 4. Load question-type data
    # --------------------------------------------------------

    question_df = pd.read_csv(
        question_type_path
    )

    print("\n============================================")
    print("Question-type turns data")
    print("============================================")

    print(question_df.head())

    for col in [
        "turns",
        "cnt"
    ]:

        question_df[col] = pd.to_numeric(
            question_df[col],
            errors="coerce"
        )


    # --------------------------------------------------------
    # 5. Create figure
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


    # --------------------------------------------------------
    # 6. Plot each model
    # --------------------------------------------------------

    for ax, model in zip(
        axes,
        MODEL_ORDER
    ):

        model_df = question_df[
            question_df["model_name"] == model
        ].copy()


        # ----------------------------------------------------
        # Count table
        # ----------------------------------------------------

        count_table = (
            model_df
            .pivot_table(
                index="question_type",
                columns="turns",
                values="cnt",
                aggfunc="sum",
                fill_value=0
            )
            .reindex(
                index=QUESTION_TYPE_ORDER,
                columns=TURN_ORDER,
                fill_value=0
            )
        )


        # ----------------------------------------------------
        # Convert count to percentage
        # ----------------------------------------------------

        row_totals = count_table.sum(
            axis=1
        )

        percent_table = (
            count_table
            .div(
                row_totals.replace(
                    0,
                    np.nan
                ),
                axis=0
            )
            * 100
        ).fillna(0)


        # ----------------------------------------------------
        # Stacked bars
        # ----------------------------------------------------

        bottom = np.zeros(
            len(QUESTION_TYPE_ORDER)
        )

        for turn in TURN_ORDER:

            values = percent_table[
                turn
            ].values

            bars = ax.bar(
                x,
                values,
                bottom=bottom,
                width=0.68,
                label=f"{turn} Turn"
            )


            # ------------------------------------------------
            # Percentage labels
            # ------------------------------------------------

            for i, (
                bar,
                value
            ) in enumerate(
                zip(
                    bars,
                    values
                )
            ):

                if value >= 7:

                    ax.text(
                        bar.get_x()
                        + bar.get_width() / 2,

                        bottom[i]
                        + value / 2,

                        f"{value:.0f}%",

                        ha="center",
                        va="center",

                        fontsize=8
                    )

            bottom += values


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
            0,
            100
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
    # Shared labels
    # --------------------------------------------------------

    axes[0].set_ylabel(
        "Distribution of Interaction Turns (%)",
        fontsize=11
    )

    fig.supxlabel(
        "Question Type",
        fontsize=11,
        y=0.03
    )


    # --------------------------------------------------------
    # Shared legend
    # --------------------------------------------------------

    add_turn_legend(
        fig,
        axes
    )


    # --------------------------------------------------------
    # Layout
    # --------------------------------------------------------

    plt.tight_layout(
        rect=[
            0,
            0.05,
            1,
            0.94
        ]
    )


    # --------------------------------------------------------
    # Save
    # --------------------------------------------------------

    output_path = (
        output_dir
        / "interaction_turn_distribution_by_question_type.png"
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
        f"\nQuestion-type figure saved to:\n"
        f"{output_path}"
    )


# ============================================================
# FIGURE 2
# Performance by Student–Tutor interaction turns
# Grouped bar chart
#
# IMPORTANT:
# turns.csv is already aggregated directly at:
#
#     model_name + turns
#
# Therefore NO further aggregation is performed here.
# ============================================================

if PLOT_PERFORMANCE_BY_TURNS:

    # --------------------------------------------------------
    # Load performance-by-turn data
    # --------------------------------------------------------

    performance_df = pd.read_csv(
        performance_path
    )

    print("\n============================================")
    print("Performance by turns data")
    print("============================================")

    print(
        performance_df
        .to_string(index=False)
    )


    # --------------------------------------------------------
    # Numeric conversion
    # --------------------------------------------------------

    numeric_columns = [
        "turns",
        "cnt",
        "avg_weighted_score",
        "avg_correct_score",
        "avg_reasoning_score"
    ]

    for col in numeric_columns:

        performance_df[col] = pd.to_numeric(
            performance_df[col],
            errors="coerce"
        )


    # --------------------------------------------------------
    # Create figure
    # --------------------------------------------------------

    fig, axes = plt.subplots(
        nrows=1,
        ncols=3,
        figsize=(14.5, 5.3),
        sharey=True
    )


    METRICS = {
        "Weighted Score": "avg_weighted_score",
        "Correctness": "avg_correct_score",
        "Reasoning": "avg_reasoning_score"
    }

    metric_names = list(
        METRICS.keys()
    )

    x = np.arange(
        len(TURN_ORDER)
    )

    bar_width = 0.23


    # --------------------------------------------------------
    # Plot each student model
    # --------------------------------------------------------

    for ax, model in zip(
        axes,
        MODEL_ORDER
    ):

        model_df = (
            performance_df[
                performance_df[
                    "model_name"
                ] == model
            ]
            .set_index("turns")
            .reindex(TURN_ORDER)
        )


        # ----------------------------------------------------
        # Draw grouped bars
        # ----------------------------------------------------

        for i, (
            label,
            column
        ) in enumerate(
            METRICS.items()
        ):

            values = model_df[
                column
            ].values

            positions = (
                x
                + (
                    i - 1
                ) * bar_width
            )

            bars = ax.bar(
                positions,
                values,
                width=bar_width,
                label=label
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

                ax.text(
                    bar.get_x()
                    + bar.get_width() / 2,

                    value + 0.035,

                    f"{value:.2f}",

                    ha="center",
                    va="bottom",

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
                "1",
                "2",
                "3"
            ],
            fontsize=9
        )

        # ----------------------------------------------------
        # Y-axis
        #
        # Keep the full 0–5 scale because these are
        # evaluation scores.
        # ----------------------------------------------------

        ax.set_ylim(
            0,
            5.35
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
        "Mean Evaluation Score (0–5)",
        fontsize=11
    )


    # ========================================================
    # Shared X label
    # ========================================================

    fig.supxlabel(
        "Number of Student–Tutor Interaction Turns",
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

        ncol=3,

        frameon=False,

        fontsize=10,

        bbox_to_anchor=(
            0.5,
            0.985
        ),

        handlelength=1.8,
        handleheight=1.1,

        columnspacing=2.2,
        handletextpad=0.7
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
        / "performance_by_interaction_turns.png"
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
        f"\nPerformance-by-turns figure saved to:\n"
        f"{output_path}"
    )

# ============================================================
# FIGURE 3
# Interaction-turn distribution by difficulty level
# ============================================================

if PLOT_HARD_LEVEL_TURNS:

    # --------------------------------------------------------
    # 9. Load difficulty data
    # --------------------------------------------------------

    hard_df = pd.read_csv(
        hard_level_path
    )

    print("\n============================================")
    print("Difficulty-level turns data")
    print("============================================")

    print(
        hard_df.head()
    )


    for col in [
        "turns",
        "cnt"
    ]:

        hard_df[col] = pd.to_numeric(
            hard_df[col],
            errors="coerce"
        )


    # --------------------------------------------------------
    # 10. Create figure
    # --------------------------------------------------------

    fig, axes = plt.subplots(
        nrows=1,
        ncols=3,
        figsize=(13, 5.5),
        sharey=True
    )

    x = np.arange(
        len(HARD_LEVEL_ORDER)
    )


    # --------------------------------------------------------
    # 11. Plot each model
    # --------------------------------------------------------

    for ax, model in zip(
        axes,
        MODEL_ORDER
    ):

        model_df = hard_df[
            hard_df["model_name"] == model
        ].copy()


        # ----------------------------------------------------
        # Build count table
        # ----------------------------------------------------

        count_table = (
            model_df
            .pivot_table(
                index="hard_level",
                columns="turns",
                values="cnt",
                aggfunc="sum",
                fill_value=0
            )
            .reindex(
                index=HARD_LEVEL_ORDER,
                columns=TURN_ORDER,
                fill_value=0
            )
        )


        # ----------------------------------------------------
        # Convert counts into percentages
        # ----------------------------------------------------

        row_totals = count_table.sum(
            axis=1
        )

        percent_table = (
            count_table
            .div(
                row_totals.replace(
                    0,
                    np.nan
                ),
                axis=0
            )
            * 100
        ).fillna(0)


        # ----------------------------------------------------
        # Stacked bars
        # ----------------------------------------------------

        bottom = np.zeros(
            len(HARD_LEVEL_ORDER)
        )

        for turn in TURN_ORDER:

            values = percent_table[
                turn
            ].values

            bars = ax.bar(
                x,
                values,
                bottom=bottom,

                width=0.58,

                label=f"{turn} Turn"
            )


            # ------------------------------------------------
            # Percentage labels
            # ------------------------------------------------

            for i, (
                bar,
                value
            ) in enumerate(
                zip(
                    bars,
                    values
                )
            ):

                if value >= 7:

                    ax.text(
                        bar.get_x()
                        + bar.get_width() / 2,

                        bottom[i]
                        + value / 2,

                        f"{value:.0f}%",

                        ha="center",
                        va="center",

                        fontsize=8.5
                    )

            bottom += values


        # ----------------------------------------------------
        # Panel formatting
        # ----------------------------------------------------

        ax.set_title(
            model,
            fontsize=11
        )

        ax.set_xticks(x)

        ax.set_xticklabels(
            HARD_LEVEL_ORDER,
            fontsize=9
        )

        ax.set_ylim(
            0,
            100
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
    # Shared labels
    # --------------------------------------------------------

    axes[0].set_ylabel(
        "Distribution of Interaction Turns (%)",
        fontsize=11
    )

    fig.supxlabel(
        "Difficulty Level",
        fontsize=11,
        y=0.03
    )


    # --------------------------------------------------------
    # Shared legend
    # --------------------------------------------------------

    add_turn_legend(
        fig,
        axes
    )


    # --------------------------------------------------------
    # Layout
    # --------------------------------------------------------

    plt.tight_layout(
        rect=[
            0,
            0.05,
            1,
            0.94
        ]
    )


    # --------------------------------------------------------
    # Save
    # --------------------------------------------------------

    output_path = (
        output_dir
        / "interaction_turn_distribution_by_difficulty.png"
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
        f"\nDifficulty-level figure saved to:\n"
        f"{output_path}"
    )