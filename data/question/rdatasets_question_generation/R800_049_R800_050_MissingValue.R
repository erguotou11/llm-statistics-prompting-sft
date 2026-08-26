# ============================================================
# Missing Value Question Generator
#
# R800_049
# Dataset: airquality
# Domain: Healthcare
# Difficulty: Medium
# Question type: Interpretation
# Count: 10
#
# R800_050
# Dataset: airquality
# Domain: Finance
# Difficulty: Hard
# Question type: Short Answer
# Count: 5
#
# Outputs:
# 1. R800_049_R800_050_MissingValue_v2.csv
# 2. R800_049_R800_050_MissingValue_v2.json
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

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

# ------------------------------------------------------------
# Core summaries from airquality
# ------------------------------------------------------------

aq <- airquality

ozone_na <- sum(is.na(aq$Ozone))
solar_na <- sum(is.na(aq$Solar.R))

ozone_missing_rate <- mean(is.na(aq$Ozone))
solar_missing_rate <- mean(is.na(aq$Solar.R))

complete_rows_all <- sum(
  complete.cases(
    aq[, c("Ozone", "Solar.R", "Wind", "Temp", "Month")]
  )
)

ozone_mean <- mean(aq$Ozone, na.rm = TRUE)
ozone_median <- median(aq$Ozone, na.rm = TRUE)
solar_mean <- mean(aq$Solar.R, na.rm = TRUE)
solar_median <- median(aq$Solar.R, na.rm = TRUE)

complete_case_data <- aq[
  complete.cases(
    aq[, c("Ozone", "Solar.R", "Wind", "Temp", "Month")]
  ),
]

complete_case_ozone_mean <- mean(
  complete_case_data$Ozone
)

mean_imputed_ozone <- aq$Ozone
mean_imputed_ozone[is.na(mean_imputed_ozone)] <- ozone_mean

median_imputed_ozone <- aq$Ozone
median_imputed_ozone[is.na(median_imputed_ozone)] <- ozone_median

mean_imputed_ozone_mean <- mean(mean_imputed_ozone)
median_imputed_ozone_mean <- mean(median_imputed_ozone)

mean_imputed_ozone_sd <- sd(mean_imputed_ozone)
median_imputed_ozone_sd <- sd(median_imputed_ozone)
observed_ozone_sd <- sd(aq$Ozone, na.rm = TRUE)

month_missing_table <- aggregate(
  cbind(
    ozone_missing = is.na(Ozone),
    solar_missing = is.na(Solar.R)
  ) ~ Month,
  data = aq,
  FUN = sum
)

month_ozone_missing_rates <- aggregate(
  is.na(Ozone) ~ Month,
  data = aq,
  FUN = mean
)

names(month_ozone_missing_rates)[2] <- "missing_rate"

highest_missing_month <- month_ozone_missing_rates$Month[
  which.max(month_ozone_missing_rates$missing_rate)
]

highest_missing_rate <- max(
  month_ozone_missing_rates$missing_rate
)

# ------------------------------------------------------------
# Scenario banks
# ------------------------------------------------------------

healthcare_scenarios <- c(

  paste(
    "Before hospital trainees work with genuine respiratory records, airquality",
    "is used as a harmless proxy dataset. Ozone stands in for a biomarker whose",
    "measurements are absent on some days."
  ),

  paste(
    "During review of a mock exposure study, the analyst reports that 37 Ozone",
    "values are missing and proposes deleting every affected row without further",
    "investigation."
  ),

  paste(
    "A clinical data workshop compares mean and median imputation for a skewed",
    "measurement. The purpose is to decide which summary is less vulnerable to",
    "extreme observed values."
  ),

  paste(
    "Rather than looking only at the overall missing percentage, the quality team",
    "checks whether missing Ozone records are concentrated in particular months."
  ),

  paste(
    "A trainee notices that the average Ozone value barely changes after mean",
    "imputation and concludes that the procedure cannot affect any analysis."
  ),

  paste(
    "Once rows missing either Ozone or Solar.R are removed, the sample size falls",
    "from 153 to a much smaller number. The report must explain what that means."
  ),

  paste(
    "In a simulated patient-monitoring exercise, Solar.R stands in for a second",
    "clinical measurement. Its missingness rate is compared with that of Ozone."
  ),

  paste(
    "After median imputation, the standard deviation of Ozone changes. The team",
    "must interpret whether simple single-value filling can distort variability."
  ),

  paste(
    "A methods note says that complete-case analysis is unbiased whenever missing",
    "values are deleted. That statement is being checked before it is taught to",
    "new analysts."
  ),

  paste(
    "For the closing healthcare exercise, several preprocessing options are",
    "considered: deletion, mean imputation, median imputation and model-based",
    "imputation. The choice must reflect both missingness and analysis goals."
  )
)

healthcare_styles <- c(
  "proxy-clinical",
  "deletion-review",
  "robust-summary",
  "pattern-check",
  "imputation-effect",
  "sample-size-loss",
  "missingness-comparison",
  "variance-distortion",
  "bias-caution",
  "method-selection"
)

healthcare_tasks <- c(
  "interpret_missing_count",
  "evaluate_listwise_deletion",
  "mean_vs_median_imputation",
  "interpret_month_pattern",
  "mean_preservation_not_safety",
  "complete_case_consequence",
  "compare_missing_rates",
  "interpret_sd_change",
  "missing_mechanism_bias",
  "balanced_method_choice"
)

finance_scenarios <- c(

  paste(
    "Following a disruption in environmental reporting, an investment team finds",
    "that several pollution observations are missing from the dataset used in an",
    "ESG risk model."
  ),

  paste(
    "While preparing a lender exposure score, analysts discover that complete-case",
    "deletion removes many rows and may alter the composition of the sample."
  ),

  paste(
    "Instead of treating every absent value as random, the model-risk committee",
    "asks whether missingness could be linked to month, weather conditions or",
    "sensor performance."
  ),

  paste(
    "A valuation model uses mean-imputed Ozone values because the method is easy",
    "to audit. The risk team now questions whether the resulting confidence in",
    "the model is overstated."
  ),

  paste(
    "For the final governance memo, the committee must recommend a missing-data",
    "strategy that balances transparency, statistical validity and operational",
    "cost."
  )
)

finance_styles <- c(
  "esg-risk",
  "sample-composition",
  "missingness-mechanism",
  "model-risk",
  "governance-recommendation"
)

finance_tasks <- c(
  "esg_missingness_risk",
  "deletion_and_selection_bias",
  "mechanism_reasoning",
  "imputation_uncertainty",
  "governance_strategy"
)

# ------------------------------------------------------------
# Build R800_049 questions
# ------------------------------------------------------------

build_healthcare_question <- function(i) {

  task_name <- healthcare_tasks[i]

  if (task_name == "interpret_missing_count") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The dataset contains ",
      ozone_na,
      " missing Ozone values out of ",
      nrow(aq),
      " records, a missing rate of ",
      fmt_pct(ozone_missing_rate),
      ". Interpret this result in context."
    )

    reference_answer <- paste0(
      "About ",
      fmt_pct(ozone_missing_rate),
      " of the proxy biomarker values are unavailable. This is large enough to",
      " affect sample size and possibly bias estimates, so the missing-data pattern",
      " should be investigated before deletion or imputation."
    )

    solution_steps <- paste0(
      "1. Convert the count into a percentage. ",
      "2. Interpret the percentage as loss of measurement availability. ",
      "3. Note possible effects on precision and bias. ",
      "4. Recommend checking the missingness mechanism."
    )

  } else if (task_name == "evaluate_listwise_deletion") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Explain why automatic row deletion may be risky even though it produces",
      " a dataset with no missing values."
    )

    reference_answer <- paste0(
      "Deleting all incomplete rows reduces the usable sample and may change which",
      " observations remain. If missingness is related to exposure level, month or",
      " another measured factor, complete-case estimates can be biased. Deletion is",
      " most defensible when missingness is plausibly unrelated to the analysis variables."
    )

    solution_steps <- paste0(
      "1. Identify the sample-size loss. ",
      "2. Explain how deletion can alter sample composition. ",
      "3. Link bias to the missingness mechanism. ",
      "4. State the conditions under which complete-case analysis is more defensible."
    )

  } else if (task_name == "mean_vs_median_imputation") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The observed Ozone mean is ",
      fmt_num(ozone_mean),
      " and the median is ",
      fmt_num(ozone_median),
      ". Which imputation is more resistant to extreme values, and why?"
    )

    reference_answer <- paste0(
      "Median imputation is more resistant to extreme Ozone values because the",
      " median depends on order rather than magnitude. Mean imputation is more",
      " sensitive to unusually high observations and may be less representative",
      " when the observed distribution is skewed."
    )

    solution_steps <- paste0(
      "1. Compare the roles of mean and median. ",
      "2. Recall that the median is robust to extremes. ",
      "3. Link the choice to possible skewness in Ozone. ",
      "4. Note that both methods still understate imputation uncertainty."
    )

  } else if (task_name == "interpret_month_pattern") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Month ",
      highest_missing_month,
      " has the highest Ozone missing rate at ",
      fmt_pct(highest_missing_rate),
      ". What does this pattern suggest?"
    )

    reference_answer <- paste0(
      "The concentration of missing values in one month suggests that missingness",
      " may be related to season, operational conditions or sensor behaviour rather",
      " than being completely random. Month should therefore be considered in the",
      " missing-data model or imputation process."
    )

    solution_steps <- paste0(
      "1. Identify the month with the largest rate. ",
      "2. Recognise clustering rather than uniform missingness. ",
      "3. Explain why this weakens a completely-random assumption. ",
      "4. Recommend including Month in further analysis."
    )

  } else if (task_name == "mean_preservation_not_safety") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Observed-case mean Ozone is ",
      fmt_num(ozone_mean),
      ", and the mean after mean imputation is ",
      fmt_num(mean_imputed_ozone_mean),
      ". Why does this similarity not prove the method is harmless?"
    )

    reference_answer <- paste0(
      "Mean imputation is designed to preserve the mean, so the similarity is expected.",
      " It can still reduce variance, weaken correlations and make standard errors too",
      " small because every missing case receives the same value. Preserving one summary",
      " does not preserve the full data structure."
    )

    solution_steps <- paste0(
      "1. Note that mean imputation inserts the observed mean. ",
      "2. Explain why the overall mean is therefore preserved. ",
      "3. Identify distortion of variance and relationships. ",
      "4. Reject the claim that unchanged mean implies unchanged analysis."
    )

  } else if (task_name == "complete_case_consequence") {

    rows_lost <- nrow(aq) - complete_rows_all

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Only ",
      complete_rows_all,
      " complete records remain, so ",
      rows_lost,
      " rows are lost. Interpret the statistical consequence."
    )

    reference_answer <- paste0(
      "Complete-case analysis reduces the effective sample from ",
      nrow(aq),
      " to ",
      complete_rows_all,
      ", which lowers precision and may change the sample composition. If the",
      " missing rows differ systematically from the retained rows, the estimates",
      " may also be biased."
    )

    solution_steps <- paste0(
      "1. Calculate the number of rows lost. ",
      "2. Link fewer rows to larger uncertainty. ",
      "3. Explain possible selection bias. ",
      "4. Recommend comparing retained and excluded records."
    )

  } else if (task_name == "compare_missing_rates") {

    higher_variable <- if (
      ozone_missing_rate > solar_missing_rate
    ) {
      "Ozone"
    } else {
      "Solar.R"
    }

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Ozone is missing in ",
      fmt_pct(ozone_missing_rate),
      " of rows, while Solar.R is missing in ",
      fmt_pct(solar_missing_rate),
      ". Interpret the comparison."
    )

    reference_answer <- paste0(
      higher_variable,
      " has the higher missingness rate. This means analyses involving ",
      higher_variable,
      " face greater data loss or imputation burden, although the missingness",
      " mechanism matters more than the rate alone."
    )

    solution_steps <- paste0(
      "1. Compare the two percentages. ",
      "2. Identify the variable with more missing data. ",
      "3. Explain the implications for usable sample size. ",
      "4. Add that rate alone does not determine bias."
    )

  } else if (task_name == "interpret_sd_change") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Observed Ozone SD is ",
      fmt_num(observed_ozone_sd),
      ", while median-imputed SD is ",
      fmt_num(median_imputed_ozone_sd),
      ". What does this change indicate?"
    )

    reference_answer <- paste0(
      "Replacing all missing values with one median adds repeated central values,",
      " which can compress the distribution and reduce variability. The imputed",
      " standard deviation therefore understates uncertainty that would exist if",
      " the missing Ozone values had been observed."
    )

    solution_steps <- paste0(
      "1. Compare the two standard deviations. ",
      "2. Recognise the effect of inserting the same value repeatedly. ",
      "3. Explain the reduction in spread. ",
      "4. Link this to understated uncertainty."
    )

  } else if (task_name == "missing_mechanism_bias") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Explain why the validity of complete-case analysis depends on why values",
      " are missing."
    )

    reference_answer <- paste0(
      "If values are missing completely at random, deleting incomplete rows may",
      " mainly reduce precision. If missingness depends on observed or unobserved",
      " values, the retained sample may no longer represent the full population.",
      " In that case, deletion can bias means, associations and model coefficients."
    )

    solution_steps <- paste0(
      "1. Distinguish random from systematic missingness. ",
      "2. Explain how systematic missingness changes the retained sample. ",
      "3. Connect this to biased estimates. ",
      "4. Recommend diagnostics and sensitivity analysis."
    )

  } else {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Which approach is most defensible, and what evidence should guide the choice?"
    )

    reference_answer <- paste0(
      "No single method is automatically best. The choice should depend on the",
      " proportion and pattern of missingness, variable distribution, intended",
      " analysis and whether auxiliary variables such as Month help explain missingness.",
      " Model-based or multiple imputation is generally preferable when uncertainty",
      " and relationships among variables must be preserved."
    )

    solution_steps <- paste0(
      "1. Assess missingness rate and pattern. ",
      "2. Consider distribution and analysis goals. ",
      "3. Compare deletion, simple imputation and model-based methods. ",
      "4. Recommend a method that reflects imputation uncertainty."
    )
  }

  data.frame(
    id = sprintf("R800_049_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_049",
    dataset_name = "airquality",
    statistical_concept = "Missing Value",
    task = "missing_value_interpretation",
    template_id = paste0("missing_value_", task_name),
    difficulty = "medium",
    scenario = "healthcare",
    language_style = healthcare_styles[i],
    question_type = "interpretation",
    predictor = "Ozone, Solar.R, Wind, Temp, Month",
    response = "missing value handling",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "written_interpretation",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Build R800_050 questions
# ------------------------------------------------------------

build_finance_question <- function(i) {

  task_name <- finance_tasks[i]

  if (task_name == "esg_missingness_risk") {

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Ozone is missing in ",
      fmt_pct(ozone_missing_rate),
      " of records and Solar.R in ",
      fmt_pct(solar_missing_rate),
      ". Explain how this could affect an ESG risk score and propose checks."
    )

    reference_answer <- paste0(
      "Missing pollution values can distort exposure estimates, reduce sample size",
      " and weaken comparability across time. If missingness is concentrated in",
      " particular months or conditions, the ESG score may systematically understate",
      " or overstate risk. The team should profile missingness by Month and weather",
      " variables, compare retained with missing rows, and run sensitivity analyses."
    )

    solution_steps <- paste0(
      "1. Quantify missingness. ",
      "2. Link missing data to possible score distortion. ",
      "3. Consider systematic seasonal or operational patterns. ",
      "4. Recommend diagnostics and sensitivity analysis."
    )

  } else if (task_name == "deletion_and_selection_bias") {

    rows_lost <- nrow(aq) - complete_rows_all

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Complete-case analysis keeps ",
      complete_rows_all,
      " of ",
      nrow(aq),
      " rows. Assess the risk of using that reduced sample in a lending model."
    )

    reference_answer <- paste0(
      "Removing ",
      rows_lost,
      " rows reduces statistical precision and may introduce selection bias.",
      " If incomplete records are associated with certain months, temperatures or",
      " pollution levels, the model will be trained on a non-representative subset.",
      " The lender should compare included and excluded observations and test the",
      " stability of coefficients under alternative missing-data treatments."
    )

    solution_steps <- paste0(
      "1. Calculate the share of rows retained and lost. ",
      "2. Explain precision loss. ",
      "3. Explain selection bias under systematic missingness. ",
      "4. Recommend coefficient stability checks."
    )

  } else if (task_name == "mechanism_reasoning") {

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Explain how MCAR, MAR and MNAR assumptions would lead to different",
      " modelling decisions."
    )

    reference_answer <- paste0(
      "Under MCAR, complete-case analysis may remain unbiased but inefficient.",
      " Under MAR, imputation should condition on observed variables such as Month,",
      " Temp, Wind and Solar.R. Under MNAR, missingness depends on unobserved values",
      " themselves, so standard imputation may remain biased and sensitivity analysis",
      " or explicit missingness models are needed."
    )

    solution_steps <- paste0(
      "1. Define MCAR. ",
      "2. Define MAR and identify useful observed predictors. ",
      "3. Define MNAR. ",
      "4. Match each mechanism to an appropriate analysis strategy."
    )

  } else if (task_name == "imputation_uncertainty") {

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Observed Ozone SD is ",
      fmt_num(observed_ozone_sd),
      " and mean-imputed SD is ",
      fmt_num(mean_imputed_ozone_sd),
      ". Explain why single mean imputation can make a finance model look",
      " more certain than it really is."
    )

    reference_answer <- paste0(
      "Mean imputation inserts the same central value for every missing record,",
      " which reduces variability and can weaken correlations with other predictors.",
      " Standard errors and risk estimates may therefore appear too stable. Multiple",
      " imputation is preferable because it propagates uncertainty across several",
      " plausible completed datasets."
    )

    solution_steps <- paste0(
      "1. Compare observed and imputed variability. ",
      "2. Explain variance compression. ",
      "3. Link reduced variance to understated model risk. ",
      "4. Recommend multiple imputation or sensitivity analysis."
    )

  } else {

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Write a governance recommendation covering method choice, documentation,",
      " validation and sensitivity testing."
    )

    reference_answer <- paste0(
      "The committee should begin with a documented missingness audit by variable",
      " and Month, test whether missingness relates to observed conditions, and avoid",
      " automatic deletion or single-value imputation as the default. Multiple",
      " imputation or a transparent model-based method should be validated against",
      " complete-case and simple-imputation alternatives. Governance records should",
      " include assumptions, diagnostics, performance changes and sensitivity results."
    )

    solution_steps <- paste0(
      "1. Require a missingness audit. ",
      "2. State preferred and fallback methods. ",
      "3. Define validation comparisons. ",
      "4. Require documentation and sensitivity analysis."
    )
  }

  data.frame(
    id = sprintf("R800_050_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_050",
    dataset_name = "airquality",
    statistical_concept = "Missing Value",
    task = "missing_value_reasoning",
    template_id = paste0("missing_value_", task_name),
    difficulty = "hard",
    scenario = "finance",
    language_style = finance_styles[i],
    question_type = "short_answer",
    predictor = "Ozone, Solar.R, Wind, Temp, Month",
    response = "missing value handling",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_reasoning",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate all questions
# ------------------------------------------------------------

healthcare_questions <- do.call(
  rbind,
  lapply(seq_len(10), build_healthcare_question)
)

finance_questions <- do.call(
  rbind,
  lapply(seq_len(5), build_finance_question)
)

missing_value_questions <- rbind(
  healthcare_questions,
  finance_questions
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

missing_value_questions <- missing_value_questions[, required_columns]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(names(missing_value_questions), required_columns),
  nrow(missing_value_questions) == 15,
  length(unique(missing_value_questions$id)) == 15,
  !anyDuplicated(missing_value_questions$question),
  sum(missing_value_questions$blueprint_id == "R800_049") == 10,
  sum(missing_value_questions$blueprint_id == "R800_050") == 5,
  all(healthcare_questions$difficulty == "medium"),
  all(healthcare_questions$question_type == "interpretation"),
  all(finance_questions$difficulty == "hard"),
  all(finance_questions$question_type == "short_answer"),
  all(nchar(missing_value_questions$question) >= 100),
  all(nchar(missing_value_questions$reference_answer) >= 100),
  all(nchar(missing_value_questions$solution_steps) >= 50)
)

# ------------------------------------------------------------
# Preview
# ------------------------------------------------------------

cat("\nQuestion count by blueprint:\n")
print(table(missing_value_questions$blueprint_id))

cat("\nQuestion count by difficulty:\n")
print(table(missing_value_questions$difficulty))

cat("\nQuestion count by question type:\n")
print(table(missing_value_questions$question_type))

preview_columns <- c(
  "id",
  "dataset_name",
  "difficulty",
  "scenario",
  "language_style",
  "question_type",
  "template_id"
)

print(
  missing_value_questions[, preview_columns],
  row.names = FALSE
)

cat("\n\n================ R800_049 example ================\n\n")
cat(
  healthcare_questions$question[1],
  "\n\nReference answer:\n",
  healthcare_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  healthcare_questions$solution_steps[1],
  "\n"
)

cat("\n\n================ R800_050 example ================\n\n")
cat(
  finance_questions$question[1],
  "\n\nReference answer:\n",
  finance_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  finance_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_049_R800_050_MissingValue_v2.csv"
json_file <- "R800_049_R800_050_MissingValue_v2.json"

write.csv(
  missing_value_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write_json(
  missing_value_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat(
  "\n\nSuccessfully generated ",
  nrow(missing_value_questions),
  " missing-value questions.\n",
  sep = ""
)

cat(
  "R800_049 healthcare interpretation questions: ",
  nrow(healthcare_questions),
  "\n",
  sep = ""
)

cat(
  "R800_050 finance short-answer questions: ",
  nrow(finance_questions),
  "\n",
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
