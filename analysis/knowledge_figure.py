import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 1. Paths
# ============================================================

data_dir = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis/"
)

input_file = data_dir / "knowledge_weighted_score.csv"
output_file = data_dir / "student_tutor_gain_by_knowledge.png"


# ============================================================
# 2. Load data
# ============================================================

df = pd.read_csv(input_file)

# Keep only the first four columns
df = df.iloc[:, :4].copy()

# Keep Direct and Student-Tutor only
df = df[
    df["prompt_strategy"].isin(["Direct", "Student-Tutor"])
].copy()


# ============================================================
# 3. Reshape data
# ============================================================

pivot_df = (
    df.pivot_table(
        index=["model_name", "knowledge"],
        columns="prompt_strategy",
        values="avg_weighted_score",
        aggfunc="mean"
    )
    .reset_index()
)

# Absolute difference
pivot_df["diff"] = (
    pivot_df["Student-Tutor"]
    - pivot_df["Direct"]
)

# Relative difference (%)
pivot_df["diff_rate"] = (
    pivot_df["diff"]
    / pivot_df["Direct"]
    * 100
)


# ============================================================
# 4. Ordering
# ============================================================

model_order = [
    "Llama3-8B",
    "Qwen2.5-7B",
    "DeepSeek-R1-7B"
]

topic_order = [
    "Descriptive Statistics",
    "Probability",
    "Distribution",
    "Sampling Distribution / CLT",
    "Confidence Interval",
    "Hypothesis Testing",
    "Correlation",
    "Regression",
    "ANOVA",
    "Classification"
]

pivot_df["model_name"] = pd.Categorical(
    pivot_df["model_name"],
    categories=model_order,
    ordered=True
)

pivot_df["knowledge"] = pd.Categorical(
    pivot_df["knowledge"],
    categories=topic_order,
    ordered=True
)

pivot_df = pivot_df.sort_values(
    ["model_name", "knowledge"]
)


# ============================================================
# 5. Plot
# ============================================================

fig, axes = plt.subplots(
    3,
    1,
    figsize=(13, 12),
    sharex=True
)

for ax, model in zip(axes, model_order):

    model_df = (
        pivot_df[pivot_df["model_name"] == model]
        .sort_values("knowledge")
    )

    x = np.arange(len(model_df))

    # --------------------------------------------------------
    # Absolute Diff: bars
    # --------------------------------------------------------

    bars = ax.bar(
        x,
        model_df["diff"],
        width=0.55,
        alpha=0.8,
        label="Diff"
    )

    ax.axhline(
        0,
        linewidth=0.8
    )

    ax.set_ylabel("Weighted Score Diff")
    ax.set_title(model)

    # Add vertical space
    ymin, ymax = ax.get_ylim()
    margin = (ymax - ymin) * 0.15

    ax.set_ylim(
        ymin - margin,
        ymax + margin
    )

    # --------------------------------------------------------
    # Diff labels
    # Positive -> above bar
    # Negative -> below bar
    # --------------------------------------------------------

    for bar, value in zip(
        bars,
        model_df["diff"]
    ):

        x_pos = (
            bar.get_x()
            + bar.get_width() / 2
        )

        if value >= 0:
            offset_y = 5
            va = "bottom"
        else:
            offset_y = -7
            va = "top"

        ax.annotate(
            f"{value:+.2f}",
            xy=(x_pos, value),
            xytext=(0, offset_y),
            textcoords="offset points",
            ha="center",
            va=va,
            fontsize=9
        )


    # --------------------------------------------------------
    # Diff Rate: line
    # --------------------------------------------------------

    ax2 = ax.twinx()

    ax2.plot(
        x,
        model_df["diff_rate"],
        marker="o",
        linewidth=1.8,
        label="Diff Rate (%)"
    )

    ax2.set_ylabel("Diff Rate (%)")

    # Add vertical space
    ymin2, ymax2 = ax2.get_ylim()
    margin2 = (ymax2 - ymin2) * 0.18

    ax2.set_ylim(
        ymin2 - margin2,
        ymax2 + margin2
    )


    # --------------------------------------------------------
    # Diff Rate labels
    #
    # Positive rate -> BELOW line point
    # Negative rate -> ABOVE line point
    #
    # This avoids overlap with negative Diff labels.
    # --------------------------------------------------------

    for xi, value in zip(
        x,
        model_df["diff_rate"]
    ):

        if value >= 0:
            offset_y = -13
            va = "top"
        else:
            offset_y = 10
            va = "bottom"

        ax2.annotate(
            f"{value:+.1f}%",
            xy=(xi, value),
            xytext=(0, offset_y),
            textcoords="offset points",
            ha="center",
            va=va,
            fontsize=9
        )


    # --------------------------------------------------------
    # Grid
    # --------------------------------------------------------

    ax.grid(
        axis="y",
        linestyle="--",
        alpha=0.3
    )


# ============================================================
# 6. X-axis labels
# ============================================================

axes[-1].set_xticks(
    np.arange(len(topic_order))
)

axes[-1].set_xticklabels(
    topic_order,
    rotation=35,
    ha="right"
)

axes[-1].set_xlabel(
    "Statistical Topic"
)


# ============================================================
# 7. Layout
# ============================================================

plt.tight_layout()


# ============================================================
# 8. Save figure
# ============================================================

plt.savefig(
    output_file,
    dpi=300,
    bbox_inches="tight"
)

plt.show()

print(
    f"Figure saved to: {output_file}"
)