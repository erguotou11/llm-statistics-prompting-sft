from pathlib import Path
import shutil


ADAPTER_DIR = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/"
    "train200_17-3/llama3_sft/adapters"
)

BEST_ADAPTER_DIR = Path(
    "/Users/fangjianchao/Desktop/Dissertation/Data/SFT/"
    "train200_17-3/llama3_sft/best_adapter"
)

BEST_ITERATION =170


# Create the best-adapter directory
BEST_ADAPTER_DIR.mkdir(
    parents=True,
    exist_ok=True
)

# Best checkpoint from validation loss
source_weights = (
    ADAPTER_DIR
    / f"{BEST_ITERATION:07d}_adapters.safetensors"
)

# MLX-LM expects this standard filename
target_weights = (
    BEST_ADAPTER_DIR
    / "adapters.safetensors"
)

source_config = (
    ADAPTER_DIR
    / "adapter_config.json"
)

target_config = (
    BEST_ADAPTER_DIR
    / "adapter_config.json"
)


if not source_weights.exists():
    raise FileNotFoundError(
        f"Checkpoint not found: {source_weights}"
    )

if not source_config.exists():
    raise FileNotFoundError(
        f"Adapter config not found: {source_config}"
    )


shutil.copy2(
    source_weights,
    target_weights
)

shutil.copy2(
    source_config,
    target_config
)


print("Best adapter created successfully.")
print(f"Iteration: {BEST_ITERATION}")
print(f"Source: {source_weights}")
print(f"Output: {BEST_ADAPTER_DIR}")