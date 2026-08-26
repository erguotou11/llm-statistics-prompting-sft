import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

from scipy.stats import chi2_contingency, spearmanr


# ============================================================
# 0. Paths
# ============================================================

data_path = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis/turns_correlation.csv"
)

output_dir = data_path.parent


# ============================================================
# 1. Load data
# ============================================================

df = pd.read_csv(data_path)

print("\nColumns:")
print(df.columns.tolist())

print("\nShape:")
print(df.shape)

print("\nUnique values:")
for col in [
    "model_name",
    "hard_level",
    "question_type",
    "knowledge",
    "turns"
]:
    print(
        f"{col}: {df[col].nunique()}"
    )


# ============================================================
# 2. Numeric conversion
# ============================================================

df["turns"] = pd.to_numeric(
    df["turns"],
    errors="coerce"
)

df["cnt"] = pd.to_numeric(
    df["cnt"],
    errors="coerce"
)

df = df.dropna(
    subset=[
        "model_name",
        "hard_level",
        "question_type",
        "knowledge",
        "turns",
        "cnt"
    ]
)


# ============================================================
# 3. Bias-corrected Cramér's V
#
# Bergsma / Wicher correction:
#
# phi2 = chi2 / n
#
# phi2corr =
# max(0, phi2 - ((k-1)(r-1))/(n-1))
#
# rcorr = r - ((r-1)^2)/(n-1)
# kcorr = k - ((k-1)^2)/(n-1)
#
# V = sqrt(
#     phi2corr /
#     min(kcorr-1, rcorr-1)
# )
# ============================================================

def corrected_cramers_v(
    contingency_table
):

    table = np.asarray(
        contingency_table,
        dtype=float
    )

    chi2 = chi2_contingency(
        table,
        correction=False
    )[0]

    n = table.sum()

    if n <= 1:
        return np.nan

    phi2 = (
        chi2 / n
    )

    r, k = table.shape

    phi2_corr = max(
        0,
        phi2
        - (
            (k - 1)
            * (r - 1)
            / (n - 1)
        )
    )

    r_corr = (
        r
        - (
            (r - 1) ** 2
            / (n - 1)
        )
    )

    k_corr = (
        k
        - (
            (k - 1) ** 2
            / (n - 1)
        )
    )

    denominator = min(
        k_corr - 1,
        r_corr - 1
    )

    if denominator <= 0:
        return np.nan

    return np.sqrt(
        phi2_corr
        / denominator
    )


# ============================================================
# 4. Build contingency tables and calculate association
# ============================================================

VARIABLES = {
    "Student Model": "model_name",
    "Question Type": "question_type",
    "Difficulty Level": "hard_level",
    "Statistical Topic": "knowledge"
}

association_records = []


for display_name, column in VARIABLES.items():

    contingency = (
        df
        .pivot_table(
            index=column,
            columns="turns",
            values="cnt",
            aggfunc="sum",
            fill_value=0
        )
        .sort_index(axis=1)
    )

    print(
        "\n============================================"
    )
    print(
        f"{display_name} × Turns"
    )
    print(
        "============================================"
    )

    print(contingency)

    # --------------------------------------------------------
    # Bias-corrected Cramér's V
    # --------------------------------------------------------

    cramers_v = corrected_cramers_v(
        contingency.values
    )

    # --------------------------------------------------------
    # Chi-square test
    # --------------------------------------------------------

    chi2, p_value, dof, expected = (
        chi2_contingency(
            contingency.values,
            correction=False
        )
    )

    association_records.append({
        "variable": display_name,
        "cramers_v": cramers_v,
        "chi_square": chi2,
        "degrees_of_freedom": dof,
        "p_value": p_value
    })


association_df = pd.DataFrame(
    association_records
)

association_df = (
    association_df
    .sort_values(
        "cramers_v",
        ascending=True
    )
    .reset_index(
        drop=True
    )
)


# ============================================================
# 5. Print and save Cramér's V results
# ============================================================

print(
    "\n============================================"
)
print(
    "Association with Interaction Turns"
)
print(
    "============================================"
)

print(
    association_df
    .round(4)
    .to_string(index=False)
)


association_output = (
    output_dir
    / "turns_cramers_v_results.csv"
)

association_df.to_csv(
    association_output,
    index=False
)

print(
    f"\nAssociation results saved to:\n"
    f"{association_output}"
)


# ============================================================
# 6. Horizontal bar chart
# ============================================================

fig, ax = plt.subplots(
    figsize=(8.5, 4.8)
)

bars = ax.barh(
    association_df["variable"],
    association_df["cramers_v"]
)


# ============================================================
# 7. Value labels
# ============================================================

for bar, value in zip(
    bars,
    association_df["cramers_v"]
):

    ax.text(
        value + 0.008,
        bar.get_y()
        + bar.get_height() / 2,

        f"{value:.3f}",

        ha="left",
        va="center",

        fontsize=10
    )


# ============================================================
# 8. Formatting
# ============================================================

max_v = association_df[
    "cramers_v"
].max()

ax.set_xlim(
    0,
    max(
        0.5,
        max_v + 0.08
    )
)

# Remove axis-level labels/title
ax.set_xlabel("")
ax.set_ylabel("")
ax.set_title("")

# Figure-level title
fig.text(
    0.5,
    0.85,   # smaller -> move downward
    "Association Strength with Student–Tutor Interaction Turns",
    ha="center",
    va="center",
    fontsize=12
)

# Figure-level x-axis label
fig.text(
    0.5,
    0.08,   # larger -> move upward
    "Bias-Corrected Cramér's V",
    ha="center",
    va="center",
    fontsize=11
)

ax.grid(
    axis="x",
    linestyle="--",
    linewidth=0.6,
    alpha=0.25
)

ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

# Manual layout
# Do NOT use tight_layout() here
plt.subplots_adjust(
    left=0.18,
    right=0.96,
    bottom=0.16,
    top=0.84
)

# ============================================================
# 9. Save Cramér's V figure
# ============================================================

cramers_plot_path = (
    output_dir
    / "turns_cramers_v_association.png"
)

plt.savefig(
    cramers_plot_path,
    dpi=300,
    bbox_inches="tight",
    pad_inches=0.08
)

plt.show()
plt.close(fig)

print(
    f"\nCramér's V figure saved to:\n"
    f"{cramers_plot_path}"
)


# ============================================================
# 10. Spearman correlation:
# Difficulty Level × Turns
#
# Easy   = 1
# Medium = 2
# Hard   = 3
#
# Because the source file is aggregated,
# expand each row according to cnt.
# ============================================================

DIFFICULTY_MAP = {
    "Easy": 1,
    "Medium": 2,
    "Hard": 3
}

spearman_df = df[
    [
        "hard_level",
        "turns",
        "cnt"
    ]
].copy()

spearman_df[
    "difficulty_numeric"
] = spearman_df[
    "hard_level"
].map(
    DIFFICULTY_MAP
)

spearman_df = spearman_df.dropna(
    subset=[
        "difficulty_numeric",
        "turns",
        "cnt"
    ]
)


# ============================================================
# 11. Expand weighted counts
# ============================================================

expanded_difficulty = np.repeat(
    spearman_df[
        "difficulty_numeric"
    ].values,
    spearman_df[
        "cnt"
    ].astype(int).values
)

expanded_turns = np.repeat(
    spearman_df[
        "turns"
    ].values,
    spearman_df[
        "cnt"
    ].astype(int).values
)


# ============================================================
# 12. Spearman's rho
# ============================================================

rho, spearman_p = spearmanr(
    expanded_difficulty,
    expanded_turns
)

print(
    "\n============================================"
)
print(
    "Difficulty × Turns: Spearman correlation"
)
print(
    "============================================"
)

print(
    f"Spearman rho = {rho:.4f}"
)

print(
    f"p-value      = {spearman_p:.6g}"
)


# ============================================================
# 13. Save Spearman result
# ============================================================

spearman_output = (
    output_dir
    / "difficulty_turns_spearman.csv"
)

pd.DataFrame({
    "variable_1": [
        "Difficulty Level"
    ],
    "variable_2": [
        "Interaction Turns"
    ],
    "spearman_rho": [
        rho
    ],
    "p_value": [
        spearman_p
    ]
}).to_csv(
    spearman_output,
    index=False
)

print(
    f"\nSpearman result saved to:\n"
    f"{spearman_output}"
)