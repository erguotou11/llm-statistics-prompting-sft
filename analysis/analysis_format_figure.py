import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


# ============================================================
# 0. Plot switches
# ============================================================

PLOT_WEIGHTED_SCORE = False
PLOT_PROMPTING_GAIN = True


# ============================================================
# 1. File paths
# ============================================================

data_path = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/analysis/overall_results.csv"
)

weighted_score_output = (
    data_path.parent / "prompting_strategy_weighted_score.png"
)

prompting_gain_output = (
    data_path.parent / "prompting_gain_over_direct.png"
)


# ============================================================
# 2. Load data
# ============================================================

df = pd.read_csv(data_path)

print("Columns:")
print(df.columns.tolist())

print("\nData:")
print(df)


# ============================================================
# 3. Standardise naming
# ============================================================

df["prompt_strategy"] = df["prompt_strategy"].replace({
    "Chain-of-Thought": "CoT",
    "Student-Tutor": "Student–Tutor",
    "Student--Tutor": "Student–Tutor"
})

model_order = [
    "Llama3-8B",
    "Qwen2.5-7B",
    "DeepSeek-R1-7B"
]

prompt_order = [
    "Direct",
    "CoT",
    "Student–Tutor"
]


# ============================================================
# 4. Pivot data
# ============================================================

plot_df = (
    df.pivot(
        index="model_name",
        columns="prompt_strategy",
        values="avg_weighted_score"
    )
    .reindex(
        index=model_order,
        columns=prompt_order
    )
)

print("\nPivot table:")
print(plot_df)


# ============================================================
# 5. Figure 1
#    Weighted scores across prompting strategies
# ============================================================

if PLOT_WEIGHTED_SCORE:

    fig, ax = plt.subplots(figsize=(9.5, 5.8))

    x = np.arange(len(model_order))
    bar_width = 0.24

    for i, prompt in enumerate(prompt_order):

        values = plot_df[prompt].values

        bars = ax.bar(
            x + (i - 1) * bar_width,
            values,
            width=bar_width,
            label=prompt
        )

        # Value labels
        for bar, value in zip(bars, values):
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                bar.get_height() + 0.03,
                f"{value:.2f}",
                ha="center",
                va="bottom",
                fontsize=10
            )

    # Axis formatting
    ax.set_xlabel(
        "Student Model",
        fontsize=12
    )

    ax.set_ylabel(
        "Mean Weighted Score (0–5)",
        fontsize=12
    )

    ax.set_xticks(x)
    ax.set_xticklabels(
        model_order,
        fontsize=11
    )

    ax.set_ylim(0, 5.2)

    ax.legend(
        title="Prompting Strategy",
        fontsize=10,
        title_fontsize=10
    )

    ax.grid(
        axis="y",
        linestyle="--",
        alpha=0.3
    )

    plt.tight_layout()

    plt.savefig(
        weighted_score_output,
        dpi=300,
        bbox_inches="tight"
    )

    plt.show()
    plt.close(fig)

    print(
        f"\nWeighted-score figure saved to:\n"
        f"{weighted_score_output}"
    )


# ============================================================
# 6. Figure 2
#    Prompting gains relative to Direct
# ============================================================

if PLOT_PROMPTING_GAIN:

    cot_gain = (
        plot_df["CoT"]
        - plot_df["Direct"]
    )

    student_tutor_gain = (
        plot_df["Student–Tutor"]
        - plot_df["Direct"]
    )

    print("\nPrompting gains relative to Direct:")

    gain_df = pd.DataFrame({
        "CoT vs Direct": cot_gain,
        "Student–Tutor vs Direct": student_tutor_gain
    })

    print(gain_df)

    fig, ax = plt.subplots(figsize=(8.5, 5))

    x = np.arange(len(model_order))

    # CoT gain
    ax.plot(
        x,
        cot_gain,
        marker="o",
        linewidth=1.8,
        markersize=7,
        label="CoT vs Direct"
    )

    # Student–Tutor gain
    ax.plot(
        x,
        student_tutor_gain,
        marker="s",
        linewidth=1.8,
        markersize=7,
        label="Student–Tutor vs Direct"
    )

    # CoT value labels
    for i, value in enumerate(cot_gain):
        ax.text(
            i,
            value + 0.018,
            f"+{value:.2f}",
            ha="center",
            va="bottom",
            fontsize=10
        )

    # Student–Tutor value labels
    for i, value in enumerate(student_tutor_gain):
        ax.text(
            i,
            value + 0.018,
            f"+{value:.2f}",
            ha="center",
            va="bottom",
            fontsize=10
        )

    # Axis formatting
    ax.set_xticks(x)

    ax.set_xticklabels(
        model_order,
        fontsize=10
    )

    ax.set_xlabel(
        "Student Model",
        fontsize=11
    )

    ax.set_ylabel(
        "Improvement in Mean Weighted Score",
        fontsize=11
    )

    ax.set_ylim(0, 0.65)

    ax.axhline(
        y=0,
        linewidth=0.8
    )

    ax.grid(
        axis="y",
        linestyle="--",
        linewidth=0.6,
        alpha=0.25
    )

    ax.legend(
        frameon=False,
        fontsize=9
    )

    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    plt.tight_layout()

    plt.savefig(
        prompting_gain_output,
        dpi=300,
        bbox_inches="tight"
    )

    plt.show()
    plt.close(fig)

    print(
        f"\nPrompting-gain figure saved to:\n"
        f"{prompting_gain_output}"
    )