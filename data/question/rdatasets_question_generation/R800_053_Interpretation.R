# ============================================================
# Output Interpretation Question Generator
#
# R800_053
# Dataset: faithful
# Domain: General Everyday
# Difficulty: Medium
# Question type: Single Choice
# Count: 5
#
# Outputs:
# 1. R800_053_Interpretation_v2.csv
# 2. R800_053_Interpretation_v2.json
# ============================================================

set.seed(20260711)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

library(jsonlite)

# ------------------------------------------------------------
# Formatting helpers
# ------------------------------------------------------------

fmt_num <- function(x, digits = 3) {
  formatC(
    x,
    format = "f",
    digits = digits
  )
}

fmt_p <- function(p) {
  if (p < 0.001) {
    "< 0.001"
  } else {
    paste0(
      "= ",
      formatC(
        p,
        format = "f",
        digits = 3
      )
    )
  }
}

format_options <- function(options) {
  paste(
    paste0(
      LETTERS[seq_along(options)],
      ". ",
      options
    ),
    collapse = "\n"
  )
}

# ------------------------------------------------------------
# Statistical summaries from faithful
# ------------------------------------------------------------

faithful_data <- faithful

eruptions_mean <- mean(
  faithful_data$eruptions
)

eruptions_median <- median(
  faithful_data$eruptions
)

waiting_mean <- mean(
  faithful_data$waiting
)

waiting_median <- median(
  faithful_data$waiting
)

eruptions_sd <- sd(
  faithful_data$eruptions
)

waiting_sd <- sd(
  faithful_data$waiting
)

cor_test_result <- cor.test(
  faithful_data$eruptions,
  faithful_data$waiting,
  method = "pearson"
)

cor_value <- unname(
  cor_test_result$estimate
)

cor_p <- cor_test_result$p.value

eruptions_shapiro <- shapiro.test(
  faithful_data$eruptions
)

waiting_shapiro <- shapiro.test(
  faithful_data$waiting
)

short_group <- faithful_data[
  faithful_data$waiting < 70,
]

long_group <- faithful_data[
  faithful_data$waiting >= 70,
]

short_mean <- mean(
  short_group$eruptions
)

long_mean <- mean(
  long_group$eruptions
)

mean_difference <- long_mean - short_mean

# ------------------------------------------------------------
# Scenario design
# ------------------------------------------------------------

everyday_scenarios <- c(

  paste(
    "Visitors planning a stop at the geyser want to know whether longer",
    "waiting periods are generally followed by longer eruptions. A summary",
    "of the faithful data reports a large positive Pearson correlation."
  ),

  paste(
    "On a public information board, the average eruption duration is shown",
    "next to the median. The two values are close enough to invite a simple",
    "interpretation, but not enough to describe the entire distribution."
  ),

  paste(
    "Instead of treating every waiting period as part of one group, a travel",
    "guide separates waits below 70 minutes from waits of 70 minutes or more.",
    "The average eruption durations differ between the two groups."
  ),

  paste(
    "A local newsletter claims that a small Shapiro-Wilk p-value means the",
    "probability of normality is almost zero. The wording must be checked",
    "before publication."
  ),

  paste(
    "For a final everyday summary, the dataset is described as showing a strong",
    "relationship between waiting time and eruption duration, along with visible",
    "clustering in both variables."
  )
)

language_styles <- c(
  "visitor-planning",
  "summary-statistics",
  "group-comparison",
  "misconception-check",
  "balanced-conclusion"
)

interpretation_tasks <- c(
  "interpret_correlation",
  "interpret_mean_median",
  "interpret_group_difference",
  "interpret_shapiro_p",
  "balanced_dataset_summary"
)

# ------------------------------------------------------------
# Build questions
# ------------------------------------------------------------

build_question <- function(i) {

  task_name <- interpretation_tasks[i]

  if (task_name == "interpret_correlation") {

    stem <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "The output gives r = ",
      fmt_num(cor_value),
      " with p ",
      fmt_p(cor_p),
      ". Which interpretation is most appropriate?"
    )

    options <- c(
      "Longer waits tend to be associated with longer eruptions, and the linear relationship is strong",
      "Every extra minute of waiting causes exactly the same increase in eruption duration",
      "The variables are unrelated because correlation is not equal to 1",
      "The p-value is the probability that no relationship exists"
    )

    correct <- 1

    explanation <- paste0(
      "The positive sign gives direction, the large absolute value gives strength,",
      " and the small p-value provides evidence against zero correlation."
    )

    response_value <- "eruptions"
    predictor_value <- "waiting"

  } else if (task_name == "interpret_mean_median") {

    stem <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "For eruptions, mean = ",
      fmt_num(eruptions_mean),
      " and median = ",
      fmt_num(eruptions_median),
      ". Which conclusion is best?"
    )

    options <- c(
      "The centre is similar by these two summaries, but normality or unimodality cannot be concluded from them alone",
      "The distribution must be perfectly normal because the values are close",
      "The mean and median prove there are no clusters",
      "The larger statistic is always the better measure"
    )

    correct <- 1

    explanation <- paste0(
      "Mean and median describe centre only. They do not reveal clustering,",
      " tails or the full distributional shape."
    )

    response_value <- "eruptions"
    predictor_value <- ""

  } else if (task_name == "interpret_group_difference") {

    stem <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "The mean eruption duration is ",
      fmt_num(short_mean),
      " minutes for waits below 70 minutes and ",
      fmt_num(long_mean),
      " minutes for waits of 70 minutes or more. The difference is ",
      fmt_num(mean_difference),
      " minutes. Which interpretation is most defensible?"
    )

    options <- c(
      "Longer-wait observations are associated with longer average eruptions in this dataset",
      "Waiting 70 minutes or more guarantees an eruption longer by exactly the reported difference",
      "The comparison proves waiting time causes eruption duration",
      "The two groups have identical eruption behaviour"
    )

    correct <- 1

    explanation <- paste0(
      "The result is an observed group difference. It does not justify a deterministic",
      " or causal statement."
    )

    response_value <- "eruptions"
    predictor_value <- "waiting"

  } else if (task_name == "interpret_shapiro_p") {

    stem <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "For waiting, the Shapiro-Wilk test gives W = ",
      fmt_num(unname(waiting_shapiro$statistic)),
      " and p ",
      fmt_p(waiting_shapiro$p.value),
      ". Which statement is correct?"
    )

    options <- c(
      "The data provide evidence against a single normal model at the 5% level",
      "The probability that the data are normal equals the p-value",
      "Every waiting time is an outlier",
      "A small p-value proves the sample mean is biased"
    )

    correct <- 1

    explanation <- paste0(
      "A small p-value supports rejecting the normality null hypothesis,",
      " but it is not the probability that normality is true."
    )

    response_value <- "waiting"
    predictor_value <- ""

  } else {

    stem <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "Which statement gives the most balanced practical conclusion?"
    )

    options <- c(
      "Waiting and eruption duration are strongly positively associated, but clustering means one simple normal model may not describe either variable well",
      "Because the correlation is strong, both variables must be normally distributed",
      "The dataset proves that waiting longer causes a longer eruption",
      "Visible clusters mean no statistical analysis is possible"
    )

    correct <- 1

    explanation <- paste0(
      "The relationship and distributional shape answer different questions.",
      " Strong association can coexist with non-normal or clustered variables."
    )

    response_value <- "eruptions"
    predictor_value <- "waiting"
  }

  question <- paste0(
    stem,
    "\n\n",
    format_options(options)
  )

  reference_answer <- paste0(
    LETTERS[correct],
    ". ",
    options[correct]
  )

  solution_steps <- paste0(
    "1. Identify the output being interpreted. ",
    "2. Separate direction, strength, statistical evidence and distributional shape. ",
    "3. Eliminate causal, deterministic or probability-of-hypothesis claims. ",
    "4. Select option ",
    LETTERS[correct],
    ". ",
    explanation
  )

  data.frame(
    id = sprintf(
      "R800_053_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_053",
    dataset_name = "faithful",
    statistical_concept = "Interpretation",
    task = "statistical_output_interpretation",
    template_id = paste0(
      "output_interpretation_single_choice_",
      task_name
    ),
    difficulty = "medium",
    scenario = "general_everyday",
    language_style = language_styles[i],
    question_type = "single_choice",
    predictor = predictor_value,
    response = response_value,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "single_choice",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate questions
# ------------------------------------------------------------

interpretation_questions <- do.call(
  rbind,
  lapply(
    seq_len(5),
    build_question
  )
)

# ------------------------------------------------------------
# Required field order
# ------------------------------------------------------------

required_columns <- c(
  "id",
  "source",
  "blueprint_id",
  "dataset_name",
  "statistical_concept",
  "task",
  "template_id",
  "difficulty",
  "scenario",
  "language_style",
  "question_type",
  "predictor",
  "response",
  "question",
  "reference_answer",
  "solution_steps",
  "answer_type",
  "version"
)

interpretation_questions <- interpretation_questions[
  ,
  required_columns
]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(
    names(interpretation_questions),
    required_columns
  )
)

stopifnot(
  nrow(interpretation_questions) == 5
)

stopifnot(
  length(
    unique(interpretation_questions$id)
  ) == 5
)

stopifnot(
  !anyDuplicated(
    interpretation_questions$question
  )
)

stopifnot(
  all(
    interpretation_questions$blueprint_id ==
      "R800_053"
  )
)

stopifnot(
  all(
    interpretation_questions$difficulty ==
      "medium"
  )
)

stopifnot(
  all(
    interpretation_questions$question_type ==
      "single_choice"
  )
)

stopifnot(
  all(
    nchar(
      interpretation_questions$question
    ) >= 100
  )
)

stopifnot(
  all(
    nchar(
      interpretation_questions$solution_steps
    ) >= 50
  )
)

# ------------------------------------------------------------
# Preview
# ------------------------------------------------------------

cat(
  "\nQuestion count:\n"
)

print(
  table(
    interpretation_questions$blueprint_id
  )
)

cat(
  "\nLanguage styles:\n"
)

print(
  table(
    interpretation_questions$language_style
  )
)

preview_columns <- c(
  "id",
  "dataset_name",
  "difficulty",
  "scenario",
  "language_style",
  "question_type",
  "predictor",
  "response",
  "template_id",
  "reference_answer"
)

print(
  interpretation_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

cat(
  "\n\n================ R800_053 example ================\n\n"
)

cat(
  interpretation_questions$question[1],
  "\n\nReference answer:\n",
  interpretation_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  interpretation_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_053_Interpretation_v2.csv"
json_file <- "R800_053_Interpretation_v2.json"

write.csv(
  interpretation_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write_json(
  interpretation_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat(
  "\n\nSuccessfully generated ",
  nrow(interpretation_questions),
  " interpretation questions.\n",
  sep = ""
)

cat(
  "CSV file: ",
  normalizePath(
    csv_file,
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)

cat(
  "JSON file: ",
  normalizePath(
    json_file,
    mustWork = FALSE
  ),
  "\n",
  sep = ""
)
