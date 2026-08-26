import os
import glob
import pandas as pd
from sqlalchemy import create_engine, text


# ============================================================
# 1. MySQL Configuration
# ============================================================

MYSQL_USER = "root"
MYSQL_PASSWORD = "12345678"
MYSQL_HOST = "localhost"
MYSQL_PORT = 3306
MYSQL_DATABASE = "paper"

TABLE_NAME = "eval_sft"

# Directory containing evaluation Excel files
EVALUATION_DIR = "/Users/fangjianchao/Desktop/Dissertation/Data/evaluation_sft"


# ============================================================
# 2. Create MySQL Connection
# ============================================================

MYSQL_URL = (
    f"mysql+pymysql://{MYSQL_USER}:{MYSQL_PASSWORD}"
    f"@{MYSQL_HOST}:{MYSQL_PORT}/{MYSQL_DATABASE}"
    f"?charset=utf8mb4"
)

engine = create_engine(
    MYSQL_URL,
    pool_pre_ping=True
)
#Adapted Openintro
#R Generated
# ============================================================
# 3. Create Evaluation Table
# ============================================================

CREATE_TABLE_SQL = f"""
CREATE TABLE IF NOT EXISTS {TABLE_NAME} (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    question_from VARCHAR(100) DEFAULT 'R Generated', 
    question_number INT,
    knowledge VARCHAR(100),
    question_type VARCHAR(100),
    hard_level VARCHAR(50),
    scenario VARCHAR(100),
    model_name VARCHAR(100),
    prompt_strategy VARCHAR(50),
    question_content varchar(1000),
    options  varchar(500),
    model_answer TEXT,
    turns INT,
    duration DECIMAL(12, 3),
    word_counts INT,
    generation_status VARCHAR(50),

    correct_score DECIMAL(6, 2),
    reasoning_score DECIMAL(6, 2),
    logical_score DECIMAL(6, 2),
    explanation_score DECIMAL(6, 2),
    statistics_interpretation_score DECIMAL(6, 2),
    weighted_score DECIMAL(6, 2),

    error_type VARCHAR(255),
    evaluation_reason TEXT,

    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4
COLLATE=utf8mb4_unicode_ci;
"""


def create_table():
    """Create the evaluation table if it does not already exist."""

    with engine.begin() as conn:
        conn.execute(text(CREATE_TABLE_SQL))

    print(f"Table '{TABLE_NAME}' is ready.")


# ============================================================
# 4. Columns to Import from Excel
# ============================================================

EXCEL_COLUMNS = [
    "question_number",
    "knowledge",
    "question_type",
    "hard_level",
    "scenario",
    "model_name",
    "prompt_strategy",
    "question_content",
    "options",
    "model_answer",
    "turns",
    "duration",
    "word_counts",
    "generation_status",
    "correct_score",
    "reasoning_score",
    "logical_score",
    "explanation_score",
    "statistics_interpretation_score",
    "weighted_score",
    "error_type",
    "evaluation_reason",
]


# ============================================================
# 5. Read a Single Evaluation File
# ============================================================

def read_evaluation_file(file_path):
    """Read and clean one evaluation Excel file."""

    print(f"\nReading: {os.path.basename(file_path)}")

    # Evaluation results are stored in the "Evaluation" sheet
    df = pd.read_excel(
        file_path,
        sheet_name="Evaluation"
    )

    # Remove leading and trailing spaces from column names
    df.columns = df.columns.astype(str).str.strip()

    # Check whether all required columns exist
    missing_columns = [
        col
        for col in EXCEL_COLUMNS
        if col not in df.columns
    ]

    if missing_columns:
        raise ValueError(
            f"\nFile: {file_path}\n"
            f"Missing columns: {missing_columns}"
        )

    # Keep only the columns required for MySQL
    df = df[EXCEL_COLUMNS].copy()

    # Set the source of all current questions
    df.insert(
        0,
        "question_from",
        "R Generated"
    )

    # Integer columns
    integer_columns = [
        "question_number",
        "turns",
        "word_counts",
    ]

    # Numeric score and duration columns
    numeric_columns = [
        "duration",
        "correct_score",
        "reasoning_score",
        "logical_score",
        "explanation_score",
        "statistics_interpretation_score",
        "weighted_score",
    ]

    # Convert integer-related columns to numeric values
    for col in integer_columns:
        df[col] = pd.to_numeric(
            df[col],
            errors="coerce"
        )

    # Convert score-related columns to numeric values
    for col in numeric_columns:
        df[col] = pd.to_numeric(
            df[col],
            errors="coerce"
        )

    # Convert NaN values to None so they are stored as NULL in MySQL
    df = df.astype(object).where(
        pd.notnull(df),
        None
    )

    return df


# ============================================================
# 6. Find All Excel Files
# ============================================================

def get_excel_files():
    """Return all Excel files in the evaluation directory."""

    xlsx_files = glob.glob(
        os.path.join(EVALUATION_DIR, "*.xlsx")
    )

    xls_files = glob.glob(
        os.path.join(EVALUATION_DIR, "*.xls")
    )

    files = sorted(
        xlsx_files + xls_files
    )

    # Ignore temporary Excel files
    files = [
        file
        for file in files
        if not os.path.basename(file).startswith("~$")
    ]

    return files


# ============================================================
# 7. Import All Evaluation Files
# ============================================================

def import_all_evaluations():
    """Import all evaluation Excel files into MySQL."""

    # Create the target table first
    create_table()

    files = get_excel_files()

    if not files:
        print(
            f"No Excel files found in:\n"
            f"{EVALUATION_DIR}"
        )
        return

    print(f"\nFound {len(files)} Excel files.")

    total_rows = 0
    success_files = 0
    failed_files = []

    for file_path in files:

        try:
            # Read and clean the Excel file
            df = read_evaluation_file(file_path)

            if df.empty:
                print("Skipped: Evaluation sheet is empty.")
                continue

            # Insert data into MySQL
            df.to_sql(
                name=TABLE_NAME,
                con=engine,
                if_exists="append",
                index=False,
                chunksize=500,
                method="multi"
            )

            row_count = len(df)

            total_rows += row_count
            success_files += 1

            print(
                f"Inserted {row_count} rows."
            )

        except Exception as e:

            failed_files.append(
                (
                    os.path.basename(file_path),
                    str(e)
                )
            )

            print(
                f"FAILED: {os.path.basename(file_path)}"
            )

            print(e)

    # Print import summary
    print("\n" + "=" * 60)
    print("Import finished")
    print("=" * 60)

    print(
        f"Successful files : {success_files}"
    )

    print(
        f"Inserted rows    : {total_rows}"
    )

    print(
        f"Failed files     : {len(failed_files)}"
    )

    # Print failed file details if any
    if failed_files:

        print("\nFailed file details:")

        for file_name, error in failed_files:

            print(
                f"\nFile: {file_name}"
            )

            print(
                f"Error: {error}"
            )


# ============================================================
# 8. Main Entry Point
# ============================================================

if __name__ == "__main__":
    import_all_evaluations()