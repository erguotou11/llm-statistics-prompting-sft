import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 0. Figure switches
# ============================================================

PLOT_DIMENSION_GAIN = True
PLOT_CONTRIBUTION_SHARE = True


# ============================================================
# 1. File paths
# ============================================================

data_path = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis/overall_results.csv"
)

output_dir = data_path.parent


# ============================================================
# 2. Load aggregated data
# ============================================================

df = pd.read_csv(data_path)

print("\nColumns:")
print(df.columns.tolist())

print("\nOriginal data:")
print(df)


# ============================================================
# 3. Standardise prompting names
# ============================================================

df["prompt_strategy"] = df["prompt_strategy"].replace({
    "Chain-of-Thought": "CoT",
    "Student-Tutor": "Student–Tutor",
    "Student--Tutor": "Student–Tutor"
})


# ============================================================
# 4. Experiment settings
# ============================================================

MODEL_ORDER = [
    "Llama3-8B",
    "Qwen2.5-7B",
    "DeepSeek-R1-7B"
]

PROMPT_ORDER = [
    "CoT",
    "Student–Tutor"
]


# ============================================================
# 5. Evaluation dimensions and weights
# ============================================================

DIMENSIONS = {
    "Correctness": {
        "column": "avg_correct_score",
        "weight": 0.40
    },
    "Reasoning": {
        "column": "avg_reasoning_score",
        "weight": 0.20
    },
    "Logic": {
        "column": "avg_logical_score",
        "weight": 0.15
    },
    "Clarity": {
        "column": "avg_explanation_score",
        "weight": 0.15
    },
    "Interpretation": {
        "column": "avg_statistics_interpretation_score",
        "weight": 0.10
    }
}

DIMENSION_ORDER = list(DIMENSIONS.keys())


# ============================================================
# 6. Calculate dimension gains and contributions
#
# Both CoT and Student–Tutor are compared against Direct.
# ============================================================

records = []

for model in MODEL_ORDER:

    model_df = df[
        df["model_name"] == model
    ].copy()

    # Direct is always the baseline
    direct_rows = model_df[
        model_df["prompt_strategy"] == "Direct"
    ]

    if direct_rows.empty:
        print(
            f"WARNING: Direct baseline missing for {model}"
        )
        continue

    direct = direct_rows.iloc[0]

    for prompt in PROMPT_ORDER:

        prompt_rows = model_df[
            model_df["prompt_strategy"] == prompt
        ]

        if prompt_rows.empty:
            print(
                f"WARNING: {prompt} missing for {model}"
            )
            continue

        current = prompt_rows.iloc[0]

        temp_records = []

        # ----------------------------------------------------
        # Step 1:
        # Dimension score difference
        #
        # ΔD = Prompt - Direct
        # ----------------------------------------------------

        for dimension, info in DIMENSIONS.items():

            column = info["column"]
            weight = info["weight"]

            direct_score = float(
                direct[column]
            )

            prompt_score = float(
                current[column]
            )

            score_diff = (
                prompt_score
                - direct_score
            )

            # ------------------------------------------------
            # Step 2:
            # Weighted contribution
            #
            # contribution = ΔD × weight
            # ------------------------------------------------

            weighted_contribution = (
                score_diff * weight
            )

            temp_records.append({
                "model_name": model,
                "prompt_strategy": prompt,
                "dimension": dimension,
                "direct_score": direct_score,
                "prompt_score": prompt_score,
                "score_diff": score_diff,
                "weight": weight,
                "weighted_contribution":
                    weighted_contribution
            })

        # ----------------------------------------------------
        # Reconstructed total weighted-score gain
        # ----------------------------------------------------

        total_contribution = sum(
            row["weighted_contribution"]
            for row in temp_records
        )

        # ----------------------------------------------------
        # Actual weighted-score gain in CSV
        # Used only for checking
        # ----------------------------------------------------

        actual_weighted_gain = (
            float(current["avg_weighted_score"])
            - float(direct["avg_weighted_score"])
        )

        # ----------------------------------------------------
        # Step 3:
        # Contribution share
        #
        # All five dimensions sum to 100%.
        # Negative contribution is retained.
        # ----------------------------------------------------

        for row in temp_records:

            if abs(total_contribution) > 1e-12:

                contribution_share = (
                    row["weighted_contribution"]
                    / total_contribution
                    * 100
                )

            else:

                contribution_share = np.nan

            row["contribution_share"] = (
                contribution_share
            )

            row["total_contribution"] = (
                total_contribution
            )

            row["actual_weighted_gain"] = (
                actual_weighted_gain
            )

            records.append(row)


result_df = pd.DataFrame(records)


# ============================================================
# 7. Save calculation results
# ============================================================

csv_output = (
    output_dir
    / "dimension_gain_and_contribution.csv"
)

result_df.to_csv(
    csv_output,
    index=False
)

print("\n============================================")
print("Dimension Gain and Contribution")
print("============================================")

print(
    result_df[
        [
            "model_name",
            "prompt_strategy",
            "dimension",
            "direct_score",
            "prompt_score",
            "score_diff",
            "weight",
            "weighted_contribution",
            "contribution_share"
        ]
    ]
    .round(4)
    .to_string(index=False)
)

print(
    f"\nResults saved to:\n{csv_output}"
)


# ============================================================
# 8. Consistency check
# ============================================================

check_df = (
    result_df
    .groupby(
        [
            "model_name",
            "prompt_strategy"
        ]
    )
    .agg(
        reconstructed_gain=(
            "weighted_contribution",
            "sum"
        ),
        contribution_share_sum=(
            "contribution_share",
            "sum"
        ),
        actual_weighted_gain=(
            "actual_weighted_gain",
            "first"
        )
    )
    .reset_index()
)

check_df["gain_difference"] = (
    check_df["reconstructed_gain"]
    - check_df["actual_weighted_gain"]
)

print("\n============================================")
print("Consistency Check")
print("============================================")

print(
    check_df
    .round(4)
    .to_string(index=False)
)


# ============================================================
# 9. Plot order
# ============================================================

GROUP_ORDER = [
    "Llama3-8B\nCoT",
    "Llama3-8B\nStudent–Tutor",
    "Qwen2.5-7B\nCoT",
    "Qwen2.5-7B\nStudent–Tutor",
    "DeepSeek-R1-7B\nCoT",
    "DeepSeek-R1-7B\nStudent–Tutor"
]

result_df["group"] = (
    result_df["model_name"]
    + "\n"
    + result_df["prompt_strategy"]
)


# ============================================================
# FIGURE 1
# Raw dimension score gain
# ============================================================

if PLOT_DIMENSION_GAIN:

    fig, ax = plt.subplots(
        figsize=(13, 5.3)
    )

    x = np.arange(
        len(GROUP_ORDER)
    )

    n_dimensions = len(
        DIMENSION_ORDER
    )

    total_width = 0.82

    bar_width = (
        total_width
        / n_dimensions
    )

    all_values = []

    for i, dimension in enumerate(
        DIMENSION_ORDER
    ):

        dimension_df = result_df[
            result_df["dimension"] == dimension
        ].copy()

        dimension_df = (
            dimension_df
            .set_index("group")
            .reindex(GROUP_ORDER)
        )

        values = dimension_df[
            "score_diff"
        ].values

        all_values.extend(
            [
                value
                for value in values
                if pd.notna(value)
            ]
        )

        positions = (
            x
            - total_width / 2
            + bar_width / 2
            + i * bar_width
        )

        bars = ax.bar(
            positions,
            values,
            width=bar_width,
            label=dimension
        )

        # ----------------------------------------------------
        # Horizontal value labels
        # ----------------------------------------------------

        for bar, value in zip(
            bars,
            values
        ):

            if pd.isna(value):
                continue

            if value >= 0:
                label_y = value + 0.012
                va = "bottom"
            else:
                label_y = value - 0.012
                va = "top"

            ax.text(
                bar.get_x()
                + bar.get_width() / 2,

                label_y,

                f"{value:+.2f}",

                ha="center",
                va=va,

                fontsize=7.5,

                rotation=0
            )

    # --------------------------------------------------------
    # Zero line
    # --------------------------------------------------------

    ax.axhline(
        0,
        linewidth=0.9
    )

    # --------------------------------------------------------
    # Tight Y-axis
    # --------------------------------------------------------

    valid_values = pd.Series(
        all_values
    ).dropna()

    y_min = valid_values.min()
    y_max = valid_values.max()

    if y_min < 0:
        lower = y_min - 0.05
    else:
        lower = 0

    upper = y_max + 0.08

    ax.set_ylim(
        lower,
        upper
    )

    # --------------------------------------------------------
    # Axis labels
    # --------------------------------------------------------

    ax.set_xticks(x)

    ax.set_xticklabels(
        GROUP_ORDER,
        fontsize=9
    )

    ax.set_ylabel(
        "Score Gain over Direct",
        fontsize=11
    )

    ax.set_xlabel(
        "Student Model and Prompting Strategy",
        fontsize=11
    )

    # --------------------------------------------------------
    # Legend
    # --------------------------------------------------------

    ax.legend(
        title="Evaluation Dimension",
        frameon=False,
        fontsize=8,
        title_fontsize=9,
        ncol=5,
        loc="upper center",
        bbox_to_anchor=(0.5, 1.11)
    )

    # --------------------------------------------------------
    # Grid
    # --------------------------------------------------------

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
    # Compact layout
    # --------------------------------------------------------

    plt.tight_layout()

    output_path = (
        output_dir
        / "dimension_score_gain_over_direct.png"
    )

    plt.savefig(
        output_path,
        dpi=300,
        bbox_inches="tight"
    )

    plt.show()
    plt.close(fig)

    print(
        f"\nDimension gain figure saved to:\n"
        f"{output_path}"
    )


# ============================================================
# FIGURE 2
# Contribution share to weighted-score gain
# ============================================================

if PLOT_CONTRIBUTION_SHARE:

    # --------------------------------------------------------
    # IMPORTANT:
    # Reduced height from 6.5 to 5.3
    #
    # This makes bars visually taller and reduces
    # unnecessary white space.
    # --------------------------------------------------------

    fig, ax = plt.subplots(
        figsize=(13, 5.3)
    )

    x = np.arange(
        len(GROUP_ORDER)
    )

    n_dimensions = len(
        DIMENSION_ORDER
    )

    total_width = 0.82

    bar_width = (
        total_width
        / n_dimensions
    )

    all_values = []

    for i, dimension in enumerate(
        DIMENSION_ORDER
    ):

        dimension_df = result_df[
            result_df["dimension"] == dimension
        ].copy()

        dimension_df = (
            dimension_df
            .set_index("group")
            .reindex(GROUP_ORDER)
        )

        values = dimension_df[
            "contribution_share"
        ].values

        all_values.extend(
            [
                value
                for value in values
                if pd.notna(value)
            ]
        )

        positions = (
            x
            - total_width / 2
            + bar_width / 2
            + i * bar_width
        )

        bars = ax.bar(
            positions,
            values,
            width=bar_width,
            label=dimension
        )

        # ----------------------------------------------------
        # Horizontal percentage labels
        #
        # rotation = 0
        # ----------------------------------------------------

        for bar, value in zip(
            bars,
            values
        ):

            if pd.isna(value):
                continue

            if value >= 0:

                label_y = (
                    value + 1.0
                )

                va = "bottom"

            else:

                label_y = (
                    value - 1.0
                )

                va = "top"

            ax.text(
                bar.get_x()
                + bar.get_width() / 2,

                label_y,

                f"{value:+.1f}%",

                ha="center",
                va=va,

                fontsize=7.5,

                rotation=0
            )

    # --------------------------------------------------------
    # Zero reference line
    # --------------------------------------------------------

    ax.axhline(
        0,
        linewidth=0.9
    )

    # --------------------------------------------------------
    # IMPORTANT:
    # Tight automatic Y-axis
    #
    # Example:
    # max = 63.6
    # upper ≈ 69
    #
    # Instead of forcing the chart up to 80.
    # --------------------------------------------------------

    valid_values = pd.Series(
        all_values
    ).dropna()

    y_min = valid_values.min()
    y_max = valid_values.max()

    # Top padding:
    # around 8% of maximum value,
    # but at least 4 percentage points.
    top_padding = max(
        4,
        abs(y_max) * 0.08
    )

    # Bottom padding only when
    # negative values actually exist.
    if y_min < 0:

        bottom_padding = max(
            3,
            abs(y_min) * 0.20
        )

        lower = (
            y_min - bottom_padding
        )

    else:

        lower = 0

    upper = (
        y_max + top_padding
    )

    ax.set_ylim(
        lower,
        upper
    )

    # --------------------------------------------------------
    # Axis labels
    # --------------------------------------------------------

    ax.set_xticks(x)

    ax.set_xticklabels(
        GROUP_ORDER,
        fontsize=9
    )

    ax.set_ylabel(
        "Contribution to Weighted Score Gain (%)",
        fontsize=11
    )

    ax.set_xlabel(
        "Student Model and Prompting Strategy",
        fontsize=11
    )

    # --------------------------------------------------------
    # IMPORTANT:
    # Legend moved closer to plotting area.
    #
    # Was 1.16
    # Now 1.09
    # --------------------------------------------------------

    ax.legend(
        title="Evaluation Dimension",
        frameon=False,
        fontsize=8,
        title_fontsize=9,
        ncol=5,
        loc="upper center",
        bbox_to_anchor=(0.5, 1.09)
    )

    # --------------------------------------------------------
    # Grid
    # --------------------------------------------------------

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
    # IMPORTANT:
    # No rect=[0,0,1,0.91]
    #
    # That was creating excessive blank space.
    # --------------------------------------------------------

    plt.tight_layout()

    # --------------------------------------------------------
    # Save
    # --------------------------------------------------------

    output_path = (
        output_dir
        / "dimension_contribution_share.png"
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
        f"\nContribution-share figure saved to:\n"
        f"{output_path}"
    )