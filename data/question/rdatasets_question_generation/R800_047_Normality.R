# ============================================================
# Normality Question Generator
#
# R800_047
# Dataset: faithful
# Domain: Manufacturing
# Difficulty: Hard
# Question type: Short Answer
# Count: 10
#
# Outputs:
# 1. R800_047_Normality_v2.csv
# 2. R800_047_Normality_v2.json
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

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

# ------------------------------------------------------------
# Normality summaries
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
    max = max(x),
    mean_median_gap = abs(mean(x) - median(x))
  )
}

eruptions_stats <- normality_summary(faithful$eruptions)
waiting_stats <- normality_summary(faithful$waiting)

short_waiting <- faithful$waiting[faithful$waiting < 70]
long_waiting <- faithful$waiting[faithful$waiting >= 70]

short_waiting_stats <- normality_summary(short_waiting)
long_waiting_stats <- normality_summary(long_waiting)

# ------------------------------------------------------------
# Scenario and task design
# ------------------------------------------------------------

manufacturing_scenarios <- c(

  paste(
    "Following an overnight production run, cycle times show two visible clusters",
    "rather than one smooth bell-shaped pattern. The quality report nevertheless",
    "proposes fitting a single normal distribution to all observations."
  ),

  paste(
    "While reviewing inspection delays from three shifts, the process engineer",
    "finds a Shapiro-Wilk p-value below 0.001. Management asks whether the result",
    "alone is enough to reject the process model."
  ),

  paste(
    "Instead of approving the dashboard immediately, the quality team compares",
    "the histogram, Q-Q plot and Shapiro-Wilk output for a waiting-time variable.",
    "The three pieces of evidence point in the same direction."
  ),

  paste(
    "Several weeks after a new machine was installed, the combined waiting-time",
    "distribution appears non-normal. When the data are split into shorter and",
    "longer operating regimes, each subgroup looks much more regular."
  ),

  paste(
    "During a supplier audit, a non-significant Shapiro-Wilk result is reported",
    "as proof that the measurement process is perfectly normal. The wording is",
    "about to be included in an ISO quality document."
  ),

  paste(
    "Faced with a large sample, the manufacturing analyst obtains a tiny p-value",
    "even though the Q-Q plot shows only mild tail departures. The practical",
    "consequences for downstream analysis are being debated."
  ),

  paste(
    "Because two production lines were merged into one database, the mean and",
    "median are fairly close, yet the histogram remains clearly bimodal.",
    "A manager argues that similar averages are enough to justify normality."
  ),

  paste(
    "When preparing a control-chart study, the analyst checks the raw outcome",
    "variable but not the residuals from the fitted process model. The assumption",
    "under review concerns model errors rather than the marginal distribution."
  ),

  paste(
    "Before recommending a Box-Cox transformation, the team notes that the",
    "departures from normality come mainly from mixing operational regimes.",
    "A transformation is being proposed without first separating those regimes."
  ),

  paste(
    "For the final process-capability review, eruptions and waiting are treated as",
    "harmless stand-ins for two manufacturing measurements. Both fail the",
    "Shapiro-Wilk test, but the decision on how to proceed must go beyond p-values."
  )
)

language_styles <- c(
  "mixture-diagnosis",
  "test-interpretation",
  "evidence-synthesis",
  "regime-separation",
  "audit-correction",
  "practical-significance",
  "summary-statistics-limit",
  "residual-assumption",
  "transformation-decision",
  "process-capability"
)

normality_tasks <- c(
  "single_normal_model",
  "small_p_value",
  "combine_evidence",
  "subgroup_structure",
  "non_significant_not_proof",
  "large_sample_sensitivity",
  "mean_median_not_enough",
  "check_residuals",
  "transform_or_stratify",
  "balanced_recommendation"
)

# ------------------------------------------------------------
# Build one question
# ------------------------------------------------------------

build_normality_question <- function(i) {

  task_name <- normality_tasks[i]

  if (task_name == "single_normal_model") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "Using faithful$waiting as the benchmark, explain whether one normal model",
      " is reasonable and justify your answer using distribution shape and process logic."
    )

    reference_answer <- paste0(
      "One normal model is not reasonable for the full waiting-time distribution because the data are clearly bimodal and the Shapiro-Wilk test strongly rejects normality. ",
      "The two clusters suggest that distinct operating regimes are being combined. A better approach is to identify and analyse the regimes separately before considering normal models within each subgroup."
    )

    solution_steps <- paste0(
      "1. Inspect the overall shape for multimodality. ",
      "2. Use the Shapiro-Wilk result as supporting evidence rather than the sole criterion. ",
      "3. Link the two clusters to possible production regimes. ",
      "4. Recommend stratified analysis instead of forcing one normal distribution."
    )

    response_value <- "waiting"

  } else if (task_name == "small_p_value") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "For waiting, W = ",
      fmt_num(waiting_stats$shapiro_w),
      " and p ",
      fmt_p(waiting_stats$shapiro_p),
      ". Write a careful interpretation suitable for a quality report."
    )

    reference_answer <- paste0(
      "The small p-value provides strong evidence against the null hypothesis that the waiting-time data come from a normal distribution. ",
      "It does not quantify how serious the departure is, nor does it identify the cause. The histogram, Q-Q plot and process context should be reviewed before deciding on transformation, subgroup analysis or a different method."
    )

    solution_steps <- paste0(
      "1. State the null hypothesis of normality. ",
      "2. Compare the p-value with 0.05. ",
      "3. Reject the null without calling the p-value the probability that normality is true. ",
      "4. Add the need for graphical and process-based diagnosis."
    )

    response_value <- "waiting"

  } else if (task_name == "combine_evidence") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "Explain how the three sources of evidence should be combined and why",
      " relying on only one of them would be weaker."
    )

    reference_answer <- paste0(
      "The histogram reveals overall shape and possible multimodality, the Q-Q plot shows where departures from normality occur, and the Shapiro-Wilk test gives formal evidence against the normal model. ",
      "When all three agree, the case against normality is stronger. Any one tool alone may miss important structure or overemphasise sample-size effects."
    )

    solution_steps <- paste0(
      "1. State the role of the histogram. ",
      "2. State the role of the Q-Q plot. ",
      "3. State the role of the Shapiro-Wilk test. ",
      "4. Explain why agreement across tools gives more defensible evidence."
    )

    response_value <- "waiting"

  } else if (task_name == "subgroup_structure") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "The full waiting sample has p ",
      fmt_p(waiting_stats$shapiro_p),
      ", the short-wait subgroup has p ",
      fmt_p(short_waiting_stats$shapiro_p),
      ", and the long-wait subgroup has p ",
      fmt_p(long_waiting_stats$shapiro_p),
      ". Explain the pattern."
    )

    reference_answer <- paste0(
      "The full sample mixes two operating regimes, which produces a non-normal combined distribution. ",
      "Within-regime subgroups may be much closer to normal because the mixing mechanism has been removed. This illustrates that non-normality can arise from unmodelled process structure rather than from irregular behaviour within each regime."
    )

    solution_steps <- paste0(
      "1. Compare full-sample and subgroup p-values. ",
      "2. Recognise the mixture-distribution explanation. ",
      "3. Distinguish between within-regime shape and combined shape. ",
      "4. Recommend modelling or stratifying by regime."
    )

    response_value <- "waiting"

  } else if (task_name == "non_significant_not_proof") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "Rewrite the conclusion so that it is statistically correct and suitable",
      " for an audit trail."
    )

    reference_answer <- paste0(
      "A non-significant Shapiro-Wilk result means there is insufficient evidence to reject normality at the chosen significance level. ",
      "It does not prove exact normality. The conclusion should also mention sample size, Q-Q plot evidence and whether the approximation is adequate for the intended quality-control method."
    )

    solution_steps <- paste0(
      "1. Replace 'proved normal' with 'insufficient evidence to reject normality.' ",
      "2. Note the dependence on sample size and significance level. ",
      "3. Add graphical evidence. ",
      "4. Tie the decision to the intended manufacturing analysis."
    )

    response_value <- "waiting"

  } else if (task_name == "large_sample_sensitivity") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "Explain why statistical significance and practical importance may differ",
      " in this situation, and state how the team should proceed."
    )

    reference_answer <- paste0(
      "With a large sample, the Shapiro-Wilk test can detect small departures that may have little practical effect on robust procedures. ",
      "The team should inspect the Q-Q plot, assess the size and location of the departures, and evaluate whether the planned method is sensitive to tail behaviour. The p-value should not be the only decision rule."
    )

    solution_steps <- paste0(
      "1. Recognise that test power increases with sample size. ",
      "2. Separate detectable departure from operationally important departure. ",
      "3. Inspect the Q-Q plot and tail behaviour. ",
      "4. Judge adequacy relative to the downstream method."
    )

    response_value <- "waiting"

  } else if (task_name == "mean_median_not_enough") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "Using eruptions as the benchmark, explain why similar mean and median values",
      " do not establish normality."
    )

    reference_answer <- paste0(
      "A normal distribution is characterised by its full shape, not only by the closeness of mean and median. ",
      "Mixtures, symmetric multimodal distributions and heavy-tailed distributions can have similar means and medians while remaining non-normal. The histogram, Q-Q plot and Shapiro-Wilk test are therefore still necessary."
    )

    solution_steps <- paste0(
      "1. State what mean and median measure. ",
      "2. Explain that normality concerns the whole distribution. ",
      "3. Give examples of non-normal shapes with similar centres. ",
      "4. Recommend graphical and formal checks."
    )

    response_value <- "eruptions"

  } else if (task_name == "check_residuals") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "Explain what should be checked instead and why."
    )

    reference_answer <- paste0(
      "The residuals from the fitted process model should be checked because the regression normality assumption concerns conditional errors. ",
      "The raw outcome may be non-normal due to changing means across predictors or regimes even when residuals are approximately normal. Residual Q-Q plots and residual diagnostics are therefore more relevant."
    )

    solution_steps <- paste0(
      "1. Identify the assumption as one about model errors. ",
      "2. Distinguish the marginal outcome distribution from the conditional residual distribution. ",
      "3. Recommend a residual Q-Q plot and residual-based test. ",
      "4. Interpret any departure in the context of model adequacy."
    )

    response_value <- "eruptions, waiting"

  } else if (task_name == "transform_or_stratify") {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "Should transformation be the first response? Justify a better sequence of analysis."
    )

    reference_answer <- paste0(
      "Transformation should not be the first response when the main issue is a mixture of process regimes. ",
      "The team should first identify and separate the regimes, verify data quality, and examine each subgroup. A transformation may then be considered within a subgroup if residual shape remains problematic and the transformed scale is meaningful."
    )

    solution_steps <- paste0(
      "1. Diagnose whether non-normality comes from skewness or mixing. ",
      "2. Separate operational regimes before transforming. ",
      "3. Recheck distributions or residuals within groups. ",
      "4. Use transformation only if it addresses a remaining, interpretable problem."
    )

    response_value <- "waiting"

  } else {

    question <- paste0(
      manufacturing_scenarios[i],
      "\n\n",
      "For eruptions, W = ",
      fmt_num(eruptions_stats$shapiro_w),
      " and p ",
      fmt_p(eruptions_stats$shapiro_p),
      "; for waiting, W = ",
      fmt_num(waiting_stats$shapiro_w),
      " and p ",
      fmt_p(waiting_stats$shapiro_p),
      ". Write a balanced recommendation for further analysis."
    )

    reference_answer <- paste0(
      "Both variables show strong evidence against a single normal model, so the report should not assume normality without qualification. ",
      "The next step is to inspect histograms and Q-Q plots, investigate clustering or process regimes, and decide whether stratification, transformation, robust methods or non-parametric procedures are appropriate. ",
      "The final choice should depend on the intended analysis and the practical consequences of the observed departures."
    )

    solution_steps <- paste0(
      "1. Interpret both Shapiro-Wilk results. ",
      "2. Avoid ranking variables solely by p-value. ",
      "3. Diagnose the form and source of non-normality. ",
      "4. Recommend a method matched to the process structure and analytical goal."
    )

    response_value <- "eruptions, waiting"
  }

  data.frame(
    id = sprintf("R800_047_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_047",
    dataset_name = "faithful",
    statistical_concept = "Normality",
    task = "normality_reasoning",
    template_id = paste0("normality_shapiro_qq_", task_name),
    difficulty = "hard",
    scenario = "manufacturing",
    language_style = language_styles[i],
    question_type = "short_answer",
    predictor = "",
    response = response_value,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_reasoning",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate questions
# ------------------------------------------------------------

normality_questions <- do.call(
  rbind,
  lapply(seq_len(10), build_normality_question)
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
  all(normality_questions$blueprint_id == "R800_047"),
  all(normality_questions$difficulty == "hard"),
  all(normality_questions$question_type == "short_answer"),
  all(nchar(normality_questions$question) >= 120),
  all(nchar(normality_questions$reference_answer) >= 120),
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
  "template_id"
)

print(
  normality_questions[, preview_columns],
  row.names = FALSE
)

cat("\n\n================ R800_047 example ================\n\n")

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

csv_file <- "R800_047_Normality_v2.csv"
json_file <- "R800_047_Normality_v2.json"

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
