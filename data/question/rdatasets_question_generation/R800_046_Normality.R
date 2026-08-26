# ============================================================
# Normality Question Generator
#
# R800_046
# Dataset: faithful
# Domain: Healthcare
# Difficulty: Medium
# Question type: Single Choice
# Count: 10
#
# Outputs:
# 1. R800_046_Normality_v2.csv
# 2. R800_046_Normality_v2.json
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
  formatC(x, format = "f", digits = digits)
}

fmt_p <- function(p) {
  if (p < 0.001) {
    "< 0.001"
  } else {
    paste0("= ", formatC(p, format = "f", digits = 3))
  }
}

format_options <- function(options) {
  paste(
    paste0(LETTERS[seq_along(options)], ". ", options),
    collapse = "\n"
  )
}

# ------------------------------------------------------------
# Numerical summaries
# ------------------------------------------------------------

normality_summary <- function(x) {

  shapiro_result <- shapiro.test(x)

  ordered_x <- sort(x)
  theoretical_quantiles <- qnorm(ppoints(length(x)))

  list(
    n = length(x),
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    shapiro_w = unname(shapiro_result$statistic),
    shapiro_p = shapiro_result$p.value,
    qq_correlation = cor(ordered_x, theoretical_quantiles),
    min = min(x),
    max = max(x)
  )
}

eruptions_stats <- normality_summary(faithful$eruptions)
waiting_stats <- normality_summary(faithful$waiting)

short_waiting <- faithful$waiting[faithful$waiting < 70]
long_waiting <- faithful$waiting[faithful$waiting >= 70]

short_waiting_stats <- normality_summary(short_waiting)
long_waiting_stats <- normality_summary(long_waiting)

# ============================================================
# R800_046
# faithful + Healthcare + Medium + Single Choice
# ============================================================

healthcare_scenarios <- c(

  paste(
    "Before trainee analysts are allowed to inspect confidential patient data,",
    "they practise on faithful. Eruption duration is treated as a harmless stand-in",
    "for a continuous clinical measurement."
  ),

  paste(
    "A methods workshop presents waiting time as a proxy for appointment delay.",
    "The class must decide which numerical and graphical tools are appropriate",
    "for checking a normality assumption."
  ),

  paste(
    "During review of a mock laboratory report, the Shapiro-Wilk test for",
    "eruptions returns a very small p-value. One interpretation in the draft",
    "is statistically defensible; the others are not."
  ),

  paste(
    "Rather than relying on a single p-value, a clinical training exercise",
    "combines a Q-Q plot with the Shapiro-Wilk result for waiting time."
  ),

  paste(
    "A histogram of waiting time shows two visible clusters. The analyst is",
    "asked whether fitting one normal distribution to the full sample is sensible."
  ),

  paste(
    "After splitting waiting times into shorter and longer groups, the normality",
    "checks look different from the result based on the combined data."
  ),

  paste(
    "One trainee writes that a non-significant Shapiro-Wilk test proves the",
    "data are exactly normal. The statement appears in a quality-assurance note."
  ),

  paste(
    "A simulated clinical model requires approximately normal residuals,",
    "yet the analyst has tested the raw outcome instead of the model residuals."
  ),

  paste(
    "For a moderate-sized sample, the Q-Q points deviate mainly in the tails",
    "while the centre remains fairly straight. The task is to choose the most",
    "careful interpretation."
  ),

  paste(
    "To close the session, students compare eruptions with waiting and decide",
    "which evidence would justify rejecting a normal model at the 5% level."
  )
)

language_styles <- c(
  "training-context",
  "method-selection",
  "output-interpretation",
  "combined-evidence",
  "shape-recognition",
  "subgroup-analysis",
  "misconception-check",
  "model-assumption",
  "qq-interpretation",
  "comparative-decision"
)

normality_tasks <- c(
  "select_shapiro",
  "select_qq_and_shapiro",
  "interpret_small_p",
  "combine_qq_and_test",
  "recognise_bimodality",
  "subgroup_reasoning",
  "avoid_proof_claim",
  "test_residuals",
  "interpret_tail_deviation",
  "compare_variables"
)

build_normality_question <- function(i) {

  task_name <- normality_tasks[i]

  if (task_name == "select_shapiro") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which method is most appropriate for a formal test of normality?"
    )

    options <- c(
      "Shapiro-Wilk test",
      "Chi-squared test of independence",
      "Paired t-test",
      "One-way ANOVA"
    )

    correct <- 1

    explanation <- paste0(
      "The Shapiro-Wilk test is designed to assess whether a quantitative sample",
      " is compatible with a normal distribution."
    )

  } else if (task_name == "select_qq_and_shapiro") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which combination gives the most informative normality check?"
    )

    options <- c(
      "A Q-Q plot together with a Shapiro-Wilk test",
      "A pie chart together with a chi-squared test",
      "A bar chart together with a paired t-test",
      "A scatterplot together with a proportion test"
    )

    correct <- 1

    explanation <- paste0(
      "The Q-Q plot shows the shape of departures, while the Shapiro-Wilk test",
      " supplies a formal significance assessment."
    )

  } else if (task_name == "interpret_small_p") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "For eruptions, W = ",
      fmt_num(eruptions_stats$shapiro_w),
      " and p ",
      fmt_p(eruptions_stats$shapiro_p),
      ". Which conclusion is best?"
    )

    options <- c(
      "Reject the normality assumption at the 5% level",
      "The probability that the data are normal is below 0.001",
      "The sample mean must be incorrect",
      "Every observation is an outlier"
    )

    correct <- 1

    explanation <- paste0(
      "A small p-value is evidence against the null hypothesis of normality;",
      " it is not the probability that the data are normal."
    )

  } else if (task_name == "combine_qq_and_test") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Suppose the Q-Q plot shows systematic curvature and the Shapiro-Wilk",
      " p-value is below 0.05. Which interpretation is most appropriate?"
    )

    options <- c(
      "Both pieces of evidence suggest that a normal model is questionable",
      "The Q-Q plot should be ignored because only p-values matter",
      "The variable must be normal because the sample size is large",
      "Curvature proves the data were entered incorrectly"
    )

    correct <- 1

    explanation <- paste0(
      "Agreement between graphical and formal evidence strengthens the case",
      " that the normality assumption is not reasonable."
    )

  } else if (task_name == "recognise_bimodality") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which conclusion is most defensible?"
    )

    options <- c(
      "A single normal model may be inappropriate because the distribution appears bimodal",
      "Two clusters are exactly what a normal distribution should show",
      "Bimodality guarantees that the mean equals the median",
      "The histogram alone proves which subgroup caused the pattern"
    )

    correct <- 1

    explanation <- paste0(
      "A normal distribution is unimodal, so two clear clusters suggest that",
      " the full sample may combine distinct subgroups."
    )

  } else if (task_name == "subgroup_reasoning") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which explanation best accounts for this change?"
    )

    options <- c(
      "Mixing distinct subgroups can make the combined distribution look non-normal even when each subgroup is closer to normal",
      "Splitting the data automatically makes every subgroup normal",
      "The Shapiro-Wilk test cannot be used on subgroups",
      "A subgroup p-value must always equal the full-sample p-value"
    )

    correct <- 1

    explanation <- paste0(
      "A mixture of distributions can be non-normal even when its components",
      " are individually more regular."
    )

  } else if (task_name == "avoid_proof_claim") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which correction is statistically accurate?"
    )

    options <- c(
      "A non-significant result means there is insufficient evidence to reject normality, not that exact normality has been proved",
      "A non-significant result proves every value follows a perfect bell curve",
      "A p-value above 0.05 means the alternative hypothesis is impossible",
      "The Shapiro-Wilk test measures the sample mean"
    )

    correct <- 1

    explanation <- paste0(
      "Failure to reject a null hypothesis is not proof that the null model is exactly true."
    )

  } else if (task_name == "test_residuals") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which variable should be checked when the modelling assumption concerns",
      " normal regression errors?"
    )

    options <- c(
      "The fitted model residuals",
      "Only the row numbers",
      "The categorical predictor labels",
      "The sample size by itself"
    )

    correct <- 1

    explanation <- paste0(
      "In regression, the normality assumption applies to the conditional errors",
      " or residuals rather than necessarily to the raw response."
    )

  } else if (task_name == "interpret_tail_deviation") {

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which statement best reflects the Q-Q plot?"
    )

    options <- c(
      "The centre is approximately normal, but the tails show departures that may affect tail-sensitive analyses",
      "The data are perfectly normal because most points are near the line",
      "Any single deviation makes the entire dataset unusable",
      "Tail departures prove that the mean is zero"
    )

    correct <- 1

    explanation <- paste0(
      "Q-Q plots allow a nuanced interpretation: central fit may be reasonable",
      " even when the tails depart from normality."
    )

  } else {

    larger_p_variable <- if (
      eruptions_stats$shapiro_p >
        waiting_stats$shapiro_p
    ) {
      "eruptions"
    } else {
      "waiting"
    }

    stem <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The p-values are ",
      fmt_num(eruptions_stats$shapiro_p, 6),
      " for eruptions and ",
      fmt_num(waiting_stats$shapiro_p, 6),
      " for waiting. Which statement is correct?"
    )

    options <- c(
      paste0(
        "Both variables reject normality at 5%, and ",
        larger_p_variable,
        " has the larger p-value"
      ),
      "Neither variable rejects normality at 5%",
      "Only the variable with the larger p-value rejects normality",
      "The variable with the smaller p-value is closer to normal"
    )

    correct <- 1

    explanation <- paste0(
      "Each p-value is compared separately with 0.05; a larger p-value",
      " does not automatically imply a satisfactory normal model."
    )
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
    "1. Identify whether the question concerns test selection, graphical evidence,",
    " subgroup structure or interpretation of a p-value. ",
    "2. Recall that Shapiro-Wilk tests the null hypothesis of normality, while",
    " a Q-Q plot reveals the form of departures. ",
    "3. Eliminate options that treat non-significance as proof or confuse raw outcomes",
    " with model residuals. ",
    "4. Select option ",
    LETTERS[correct],
    ". ",
    explanation
  )

  data.frame(
    id = sprintf("R800_046_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_046",
    dataset_name = "faithful",
    statistical_concept = "Normality",
    task = "normality_method_selection",
    template_id = paste0("normality_single_choice_", task_name),
    difficulty = "medium",
    scenario = "healthcare",
    language_style = language_styles[i],
    question_type = "single_choice",
    predictor = "",
    response = ifelse(
      task_name %in% c(
        "select_shapiro",
        "interpret_small_p"
      ),
      "eruptions",
      ifelse(
        task_name %in% c(
          "select_qq_and_shapiro",
          "combine_qq_and_test",
          "recognise_bimodality",
          "subgroup_reasoning"
        ),
        "waiting",
        "eruptions, waiting"
      )
    ),
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

normality_questions <- do.call(
  rbind,
  lapply(
    seq_len(10),
    build_normality_question
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

normality_questions <- normality_questions[, required_columns]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(names(normality_questions), required_columns),
  nrow(normality_questions) == 10,
  length(unique(normality_questions$id)) == 10,
  !anyDuplicated(normality_questions$question),
  all(normality_questions$blueprint_id == "R800_046"),
  all(normality_questions$difficulty == "medium"),
  all(normality_questions$question_type == "single_choice"),
  all(nchar(normality_questions$question) >= 100),
  all(nchar(normality_questions$solution_steps) >= 60)
)

# ------------------------------------------------------------
# Preview
# ------------------------------------------------------------

cat("\nQuestion count:\n")
print(table(normality_questions$blueprint_id))

cat("\nLanguage styles:\n")
print(table(normality_questions$language_style))

preview_columns <- c(
  "id",
  "dataset_name",
  "difficulty",
  "scenario",
  "language_style",
  "question_type",
  "response",
  "template_id",
  "reference_answer"
)

print(
  normality_questions[, preview_columns],
  row.names = FALSE
)

cat("\n\n================ R800_046 example ================\n\n")

cat(
  normality_questions$question[1],
  "\n\nReference answer:\n",
  normality_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  normality_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_046_Normality_v2.csv"
json_file <- "R800_046_Normality_v2.json"

write.csv(
  normality_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write_json(
  normality_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat(
  "\n\nSuccessfully generated ",
  nrow(normality_questions),
  " normality questions.\n",
  sep = ""
)

cat(
  "CSV file: ",
  normalizePath(csv_file, mustWork = FALSE),
  "\n",
  sep = ""
)

cat(
  "JSON file: ",
  normalizePath(json_file, mustWork = FALSE),
  "\n",
  sep = ""
)
