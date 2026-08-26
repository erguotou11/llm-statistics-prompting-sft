import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 1. File path
# ============================================================

data_path = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis/overall_results.csv"
)

output_path = (
    data_path.parent
    / "dimension_contribution_pie.png"
)


# ============================================================
# 2. Load aggregated results
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
# 4. Model order
# ============================================================

MODEL_ORDER = [
    "Llama3-8B",
    "Qwen2.5-7B",
    "DeepSeek-R1-7B"
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
# 6. Calculate contribution shares
#
# Both:
# CoT vs Direct
# Student–Tutor vs Direct
# ============================================================

records = []

for model in MODEL_ORDER:

    model_df = df[
        df["model_name"] == model
    ].copy()

    direct_rows = model_df[
        model_df["prompt_strategy"] == "Direct"
    ]

    if direct_rows.empty:
        print(
            f"WARNING: Direct baseline missing for {model}"
        )
        continue

    direct = direct_rows.iloc[0]

    for prompt in [
        "CoT",
        "Student–Tutor"
    ]:

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
        # dimension difference
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
            # weighted contribution
            # ------------------------------------------------

            weighted_contribution = (
                score_diff
                * weight
            )

            temp_records.append({
                "model_name": model,
                "prompt_strategy": prompt,
                "dimension": dimension,
                "score_diff": score_diff,
                "weighted_contribution":
                    weighted_contribution
            })

        # ----------------------------------------------------
        # Total weighted contribution
        # ----------------------------------------------------

        total_contribution = sum(
            row["weighted_contribution"]
            for row in temp_records
        )

        # ----------------------------------------------------
        # Contribution share
        #
        # Shares can be negative.
        # Signed shares sum to 100%.
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

            records.append(row)


result_df = pd.DataFrame(
    records
)


# ============================================================
# 7. Print contribution results
# ============================================================

print("\n============================================")
print("Contribution Shares")
print("============================================")

print(
    result_df[
        [
            "model_name",
            "prompt_strategy",
            "dimension",
            "score_diff",
            "weighted_contribution",
            "contribution_share"
        ]
    ]
    .round(3)
    .to_string(index=False)
)


# ============================================================
# 8. Save detailed contribution CSV
# ============================================================

csv_output = (
    data_path.parent
    / "dimension_contribution_pie_data.csv"
)

result_df.to_csv(
    csv_output,
    index=False
)

print(
    f"\nContribution data saved to:\n"
    f"{csv_output}"
)


# ============================================================
# 9. Pie layout
#
# Row 1:
# CoT vs Direct
#
# Row 2:
# Student–Tutor vs Direct
# ============================================================

PIE_GROUPS = [
    ("Llama3-8B", "CoT"),
    ("Qwen2.5-7B", "CoT"),
    ("DeepSeek-R1-7B", "CoT"),

    ("Llama3-8B", "Student–Tutor"),
    ("Qwen2.5-7B", "Student–Tutor"),
    ("DeepSeek-R1-7B", "Student–Tutor")
]


# ============================================================
# 10. Create 2 × 3 figure
# ============================================================

fig, axes = plt.subplots(
    nrows=2,
    ncols=3,
    figsize=(13, 7.5)
)


# ============================================================
# 11. Plot each pie
# ============================================================

for ax, (model, prompt) in zip(
    axes.flatten(),
    PIE_GROUPS
):

    temp_df = result_df[
        (result_df["model_name"] == model)
        &
        (result_df["prompt_strategy"] == prompt)
    ].copy()

    temp_df = (
        temp_df
        .set_index("dimension")
        .reindex(DIMENSION_ORDER)
        .reset_index()
    )

    signed_values = temp_df[
        "contribution_share"
    ].values

    # --------------------------------------------------------
    # Pie charts cannot display negative area.
    #
    # Therefore:
    # wedge size = absolute contribution share
    #
    # But labels display signed values.
    # --------------------------------------------------------

    pie_values = np.abs(
        signed_values
    )


    # --------------------------------------------------------
    # Custom labels
    # --------------------------------------------------------

    def make_autopct(values):

        counter = {
            "index": 0
        }

        def signed_autopct(_pct):

            i = counter["index"]

            value = values[i]

            counter["index"] += 1

            return f"{value:+.1f}%"

        return signed_autopct


    wedges, texts, autotexts = ax.pie(

        pie_values,
        radius=1.25,
        startangle=90,

        counterclock=False,

        autopct=make_autopct(
            signed_values
        ),

        pctdistance=0.70,

        wedgeprops={
            "edgecolor": "white",
            "linewidth": 0.8
        },

        textprops={
            "fontsize": 8
        }
    )


    # --------------------------------------------------------
    # Title
    # --------------------------------------------------------

    if prompt == "CoT":

        comparison_label = (
            "CoT vs Direct"
        )

    else:

        comparison_label = (
            "Student–Tutor vs Direct"
        )

    ax.set_title(
        f"{model}\n{comparison_label}",
        fontsize=10,
        pad=8
    )


# ============================================================
# 12. Shared legend
# ============================================================

legend_handles = (
    axes[0, 0]
    .patches[
        :len(DIMENSION_ORDER)
    ]
)

fig.legend(
    legend_handles,
    DIMENSION_ORDER,

    title="Evaluation Dimension",

    loc="upper center",

    bbox_to_anchor=(
        0.5,
        1.00
    ),

    ncol=5,

    frameon=False,

    fontsize=9,

    title_fontsize=10
)


# ============================================================
# 13. Explanatory note
# ============================================================

fig.text(
    0.5,
    0.015,
    (
        "Pie-sector sizes represent absolute contribution magnitude; "
        "percentage labels retain the original signed contribution."
    ),
    ha="center",
    fontsize=11
)


# ============================================================
# 14. Layout
# ============================================================

plt.tight_layout(
    rect=[
        0,
        0.04,
        1,
        0.93
    ]
)


# ============================================================
# 15. Save
# ============================================================

plt.savefig(
    output_path,
    dpi=300,
    bbox_inches="tight",
    pad_inches=0.08
)

plt.show()

plt.close(fig)

print(
    f"\nPie-chart figure saved to:\n"
    f"{output_path}"
)