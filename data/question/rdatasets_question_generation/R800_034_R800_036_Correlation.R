# ============================================================
# Correlation Question Generator
#
# R800_034
# Dataset: mtcars
# Domain: Education
# Difficulty: Medium
# Question type: Single Choice
# Count: 10
#
# R800_036
# Dataset: mtcars
# Domain: Sports Analytics
# Difficulty: Medium
# Question type: Multiple Choice
# Count: 10
#
# Outputs:
# 1. R800_034_R800_036_Correlation.csv
# 2. R800_034_R800_036_Correlation.json
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

format_ci <- function(lower, upper, digits = 3) {
  paste0(
    "[",
    fmt_num(lower, digits),
    ", ",
    fmt_num(upper, digits),
    "]"
  )
}

direction_label <- function(r) {
  if (r > 0) {
    "positive"
  } else if (r < 0) {
    "negative"
  } else {
    "no linear"
  }
}

strength_label <- function(r) {

  a <- abs(r)

  if (a < 0.20) {
    "very weak"
  } else if (a < 0.40) {
    "weak"
  } else if (a < 0.60) {
    "moderate"
  } else if (a < 0.80) {
    "strong"
  } else {
    "very strong"
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

format_answer_letters <- function(indices) {
  paste(
    LETTERS[indices],
    collapse = ", "
  )
}

# ------------------------------------------------------------
# Correlation calculations
# ------------------------------------------------------------

calculate_correlation <- function(data, x, y) {

  keep <- complete.cases(
    data[[x]],
    data[[y]]
  )

  x_values <- data[[x]][keep]
  y_values <- data[[y]][keep]

  test_result <- cor.test(
    x_values,
    y_values,
    method = "pearson"
  )

  r_value <- unname(
    test_result$estimate
  )

  n_value <- length(
    x_values
  )

  df_value <- n_value - 2

  list(
    x = x,
    y = y,
    n = n_value,
    df = df_value,
    r = r_value,
    r_squared = r_value^2,
    t_value = unname(test_result$statistic),
    p_value = test_result$p.value,
    ci_lower = unname(test_result$conf.int[1]),
    ci_upper = unname(test_result$conf.int[2]),
    direction = direction_label(r_value),
    strength = strength_label(r_value)
  )
}

correlation_lookup <- list(
  mpg_wt = calculate_correlation(mtcars, "mpg", "wt"),
  mpg_hp = calculate_correlation(mtcars, "mpg", "hp"),
  mpg_disp = calculate_correlation(mtcars, "mpg", "disp"),
  wt_hp = calculate_correlation(mtcars, "wt", "hp"),
  wt_disp = calculate_correlation(mtcars, "wt", "disp"),
  hp_disp = calculate_correlation(mtcars, "hp", "disp")
)

# ============================================================
# R800_034
# Education + Medium + Single Choice
# ============================================================

education_scenarios <- c(

  paste(
    "A statistics class is reviewing a scatterplot of mpg against wt.",
    "The points form a clear downward pattern, and students must choose",
    "the statement that best matches the numerical result."
  ),

  paste(
    "During a lesson on research methods, the instructor presents",
    "a correlation between mpg and hp and asks students to distinguish",
    "direction from strength."
  ),

  paste(
    "An assessment-design team is checking whether learners understand",
    "when Pearson correlation is appropriate. The practice variables are",
    "wt and disp from mtcars."
  ),

  paste(
    "A tutor gives the class an output showing a very small p-value for",
    "the correlation between mpg and disp. One of four interpretations",
    "must be selected."
  ),

  paste(
    "For a classroom exercise, students compare hp with disp and are asked",
    "what a large positive coefficient means in practical terms."
  ),

  paste(
    "A revision worksheet reports r = -0.868 for mpg and wt.",
    "The next item tests whether students can interpret the sign without",
    "turning the relationship into a causal statement."
  ),

  paste(
    "A seminar on statistical reporting uses the 95% confidence interval",
    "for the mpg-disp correlation. The task is to identify the most accurate",
    "interpretation of the interval."
  ),

  paste(
    "A lecturer asks which R command correctly performs a Pearson correlation",
    "test between mpg and hp in mtcars."
  ),

  paste(
    "A mock exam asks students to decide what r-squared means after",
    "calculating the correlation between wt and disp."
  ),

  paste(
    "At the end of a correlation lesson, students compare cor(mpg, wt)",
    "with cor(mpg, hp) and must identify which association is stronger",
    "in absolute terms."
  )
)

education_styles <- c(
  "scatterplot-led",
  "conceptual",
  "method-selection",
  "output-interpretation",
  "contextual",
  "causal-caution",
  "confidence-interval",
  "software-selection",
  "effect-size",
  "comparison"
)

education_tasks <- c(
  "interpret_downward_pattern",
  "direction_vs_strength",
  "select_pearson",
  "interpret_small_p",
  "interpret_positive_r",
  "avoid_causal_claim",
  "interpret_ci",
  "select_r_command",
  "interpret_r_squared",
  "compare_strength"
)

education_pairs <- c(
  "mpg_wt",
  "mpg_hp",
  "wt_disp",
  "mpg_disp",
  "hp_disp",
  "mpg_wt",
  "mpg_disp",
  "mpg_hp",
  "wt_disp",
  "mpg_wt"
)

build_education_question <- function(i) {

  stats <- correlation_lookup[[education_pairs[i]]]
  task_name <- education_tasks[i]
  x <- stats$x
  y <- stats$y

  if (task_name == "interpret_downward_pattern") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "The Pearson correlation is r = ",
      fmt_num(stats$r),
      ". Which interpretation is best?"
    )

    options <- c(
      "Vehicles with higher weight tend to have lower mpg, and the linear association is very strong",
      "Vehicles with higher weight always have exactly the same mpg",
      "The relationship is positive because both variables are numerical",
      "The coefficient proves that weight is the only cause of fuel economy"
    )

    correct <- 1

    explanation <- paste0(
      "The negative sign indicates opposite movement, and |r| = ",
      fmt_num(abs(stats$r)),
      " indicates a very strong association."
    )

  } else if (task_name == "direction_vs_strength") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "For mpg and hp, r = ",
      fmt_num(stats$r),
      ". Which statement correctly separates direction from strength?"
    )

    options <- c(
      "The relationship is negative in direction and very strong in magnitude",
      "The relationship is positive in direction and weak in magnitude",
      "The relationship has no direction because r is not equal to 1",
      "The relationship is strong only because the p-value is small"
    )

    correct <- 1

    explanation <- paste0(
      "The sign of r gives direction, while |r| = ",
      fmt_num(abs(stats$r)),
      " gives strength."
    )

  } else if (task_name == "select_pearson") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which method is most appropriate for measuring the linear association",
      " between these two quantitative variables?"
    )

    options <- c(
      "Pearson correlation",
      "Chi-squared test of independence",
      "Paired-samples t-test",
      "One-sample proportion test"
    )

    correct <- 1

    explanation <- paste0(
      "Pearson correlation is suitable because wt and disp are both quantitative",
      " and the question concerns linear association."
    )

  } else if (task_name == "interpret_small_p") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "The output gives r = ",
      fmt_num(stats$r),
      " and p ",
      fmt_p(stats$p_value),
      ". Which conclusion is justified?"
    )

    options <- c(
      "There is strong evidence that the population linear correlation is not zero",
      "The null hypothesis has probability less than 0.001 of being true",
      "Every vehicle follows the same exact mpg-disp relationship",
      "The correlation must be clinically or practically important because p is small"
    )

    correct <- 1

    explanation <- paste0(
      "The small p-value provides evidence against H0: rho = 0,",
      " but it is not the probability that H0 is true."
    )

  } else if (task_name == "interpret_positive_r") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "The coefficient is r = ",
      fmt_num(stats$r),
      ". What does this positive value indicate?"
    )

    options <- c(
      "Cars with larger displacement tend to have higher horsepower",
      "Cars with larger displacement tend to have lower horsepower",
      "Horsepower and displacement are unrelated because r is below 1",
      "Displacement causes horsepower to increase by exactly r units"
    )

    correct <- 1

    explanation <- paste0(
      "A positive coefficient means the variables tend to increase together."
    )

  } else if (task_name == "avoid_causal_claim") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which sentence is the most statistically careful?"
    )

    options <- c(
      "Higher weight is associated with lower mpg, but the correlation alone does not prove causation",
      "Higher weight always causes lower mpg for every vehicle",
      "The negative correlation proves that no other factor matters",
      "Because r is negative, the variables are measured incorrectly"
    )

    correct <- 1

    explanation <- paste0(
      "Correlation supports an association statement, not an automatic causal conclusion."
    )

  } else if (task_name == "interpret_ci") {

    ci_text <- format_ci(
      stats$ci_lower,
      stats$ci_upper
    )

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "The 95% confidence interval is ",
      ci_text,
      ". Which interpretation is best?"
    )

    options <- c(
      "The plausible population correlations are all negative, since the interval excludes zero",
      "There is a 95% probability that every vehicle has a negative correlation",
      "The interval proves the sample correlation is exactly equal to its midpoint",
      "The interval shows that mpg and disp are unrelated because both limits are below zero"
    )

    correct <- 1

    explanation <- paste0(
      "Because the entire interval is negative, it supports a negative population correlation."
    )

  } else if (task_name == "select_r_command") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which R command is correct?"
    )

    options <- c(
      "cor.test(mtcars$mpg, mtcars$hp, method = \"pearson\")",
      "aov(mpg ~ hp, data = mtcars)",
      "chisq.test(mtcars$mpg, mtcars$hp)",
      "t.test(mtcars$mpg == mtcars$hp)"
    )

    correct <- 1

    explanation <- paste0(
      "cor.test() performs the Pearson correlation test for two quantitative variables."
    )

  } else if (task_name == "interpret_r_squared") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "For wt and disp, r-squared = ",
      fmt_num(stats$r_squared),
      ". What does this value represent?"
    )

    options <- c(
      paste0(
        "About ",
        fmt_num(100 * stats$r_squared, 1),
        "% shared linear variation between wt and disp"
      ),
      "The probability that wt causes disp",
      "The percentage of observations that were entered correctly",
      "The number of variables included in the analysis"
    )

    correct <- 1

    explanation <- paste0(
      "r-squared is the squared correlation and summarises shared linear variation."
    )

  } else {

    a <- correlation_lookup[["mpg_wt"]]
    b <- correlation_lookup[["mpg_hp"]]

    stronger <- if (
      abs(a$r) > abs(b$r)
    ) {
      "wt"
    } else {
      "hp"
    }

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "cor(mpg, wt) = ",
      fmt_num(a$r),
      " and cor(mpg, hp) = ",
      fmt_num(b$r),
      ". Which variable has the stronger linear association with mpg?"
    )

    options <- c(
      stronger,
      ifelse(stronger == "wt", "hp", "wt"),
      "Both are equally strong because both are negative",
      "Neither can be compared because the signs are the same"
    )

    correct <- 1

    explanation <- paste0(
      "Strength is compared using absolute values. The larger |r| belongs to ",
      stronger,
      "."
    )

    x <- "wt, hp"
    y <- "mpg"
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
    "1. Identify the statistical concept being tested. ",
    "2. Use the sign of r for direction, |r| for strength, and the p-value or confidence interval for inferential evidence. ",
    "3. Eliminate claims that confuse association with causation or p-values with probabilities of hypotheses. ",
    "4. Select option ",
    LETTERS[correct],
    ". ",
    explanation
  )

  data.frame(
    id = sprintf("R800_034_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_034",
    dataset_name = "mtcars",
    statistical_concept = "Correlation",
    task = "correlation_method_selection",
    template_id = paste0(
      "correlation_single_choice_",
      task_name
    ),
    difficulty = "medium",
    scenario = "education",
    language_style = education_styles[i],
    question_type = "single_choice",
    predictor = x,
    response = y,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "single_choice",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_036
# Sports Analytics + Medium + Multiple Choice
# ============================================================

sports_scenarios <- c(

  paste(
    "A performance department uses mtcars as a numerical stand-in before",
    "working with restricted athlete data. mpg is treated as an efficiency score",
    "and wt as a body-mass-style measure."
  ),

  paste(
    "During a scouting workshop, hp is used as a proxy for explosive power",
    "and mpg as a proxy for movement efficiency. Several claims about their",
    "correlation are proposed."
  ),

  paste(
    "A sports science class treats disp as a proxy for muscle volume and wt",
    "as a body-size measure. The task is to identify which interpretations",
    "of their correlation are statistically valid."
  ),

  paste(
    "A coaching analyst compares hp with disp as stand-ins for two performance",
    "metrics. The output shows a strong positive correlation."
  ),

  paste(
    "A conditioning report presents r, p and r-squared for mpg and wt.",
    "The review panel must decide which statements belong in an accurate summary."
  ),

  paste(
    "A data team is deciding whether wt and hp should both enter a player-rating model.",
    "Their pairwise correlation is examined as an initial screen for redundant information."
  ),

  paste(
    "A training-load study finds a strong negative sample correlation between",
    "a proxy efficiency measure and a proxy power measure. The analysts must separate",
    "valid association claims from causal overreach."
  ),

  paste(
    "A sports analytics module compares two correlations with mpg:",
    "one involving wt and one involving hp. Students must select every defensible conclusion."
  ),

  paste(
    "An analyst sees one unusual observation in the mpg-disp scatterplot and argues",
    "that the entire correlation result should be discarded."
  ),

  paste(
    "A final performance report must interpret a confidence interval for the",
    "mpg-hp correlation and explain what additional evidence would be needed",
    "before using the result for prediction."
  )
)

sports_styles <- c(
  "performance-analytics",
  "scouting",
  "sports-science",
  "coaching",
  "reporting",
  "model-building",
  "causal-review",
  "comparison",
  "outlier-reasoning",
  "prediction-readiness"
)

sports_tasks <- c(
  "valid_efficiency_mass_claims",
  "power_efficiency_claims",
  "body_size_volume_claims",
  "positive_association_claims",
  "reporting_claims",
  "multicollinearity_claims",
  "association_vs_causation",
  "compare_correlations",
  "outlier_claims",
  "confidence_and_prediction"
)

sports_pairs <- c(
  "mpg_wt",
  "mpg_hp",
  "wt_disp",
  "hp_disp",
  "mpg_wt",
  "wt_hp",
  "mpg_hp",
  "mpg_wt",
  "mpg_disp",
  "mpg_hp"
)

build_sports_question <- function(i) {

  stats <- correlation_lookup[[sports_pairs[i]]]
  task_name <- sports_tasks[i]
  x <- stats$x
  y <- stats$y

  if (task_name == "valid_efficiency_mass_claims") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For mpg and wt, r = ",
      fmt_num(stats$r),
      " and p ",
      fmt_p(stats$p_value),
      ". Which statements are supported? Select all that apply."
    )

    options <- c(
      "Higher values of wt tend to be associated with lower mpg",
      "The relationship is very strong in absolute terms",
      "The small p-value provides evidence against zero population correlation",
      "The correlation proves that changing wt alone will cause a fixed change in mpg",
      "Individual observations may still depart from the overall pattern"
    )

    correct <- c(
      1,
      2,
      3,
      5
    )

  } else if (task_name == "power_efficiency_claims") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For mpg and hp, r = ",
      fmt_num(stats$r),
      ". Which statements are valid? Select all that apply."
    )

    options <- c(
      "The association is negative",
      "The association is very strong",
      "Higher hp values tend to accompany lower mpg values",
      "Every observation must lie exactly on one straight line",
      "The result alone does not establish a causal mechanism"
    )

    correct <- c(
      1,
      2,
      3,
      5
    )

  } else if (task_name == "body_size_volume_claims") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For wt and disp, r = ",
      fmt_num(stats$r),
      " and r-squared = ",
      fmt_num(stats$r_squared),
      ". Which interpretations are correct? Select all that apply."
    )

    options <- c(
      "The variables show a very strong positive linear association",
      paste0(
        "About ",
        fmt_num(100 * stats$r_squared, 1),
        "% of their variation is shared in a linear sense"
      ),
      "The result proves that one variable causes the other",
      "The coefficient is close to, but not exactly, 1",
      "A positive r means the variables tend to rise together"
    )

    correct <- c(
      1,
      2,
      4,
      5
    )

  } else if (task_name == "positive_association_claims") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For hp and disp, r = ",
      fmt_num(stats$r),
      ". Which statements are defensible? Select all that apply."
    )

    options <- c(
      "Larger disp values tend to be associated with larger hp values",
      "The relationship is positive and strong",
      "The variables are identical because r is high",
      "Correlation measures linear association rather than exact equivalence",
      "A scatterplot would be useful for checking unusual observations"
    )

    correct <- c(
      1,
      2,
      4,
      5
    )

  } else if (task_name == "reporting_claims") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "The output gives r = ",
      fmt_num(stats$r),
      ", p ",
      fmt_p(stats$p_value),
      " and r-squared = ",
      fmt_num(stats$r_squared),
      ". Which items belong in a strong report? Select all that apply."
    )

    options <- c(
      "The direction and strength of the relationship",
      "The p-value as evidence about zero correlation",
      "The squared correlation as shared linear variation",
      "A statement that the relationship is causal because p is small",
      "A limitation about individual prediction or generalisability"
    )

    correct <- c(
      1,
      2,
      3,
      5
    )

  } else if (task_name == "multicollinearity_claims") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For wt and hp, r = ",
      fmt_num(stats$r),
      ". Which conclusions are reasonable? Select all that apply."
    )

    options <- c(
      "The predictors contain overlapping linear information",
      "The pair deserves further multicollinearity assessment",
      "The correlation alone proves the regression model is unusable",
      "VIF or other model diagnostics would provide additional evidence",
      "Strong correlation can make separate coefficient estimates less stable"
    )

    correct <- c(
      1,
      2,
      4,
      5
    )

  } else if (task_name == "association_vs_causation") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "The correlation is r = ",
      fmt_num(stats$r),
      ". Which statements correctly separate association from causation? Select all that apply."
    )

    options <- c(
      "The variables have a strong negative association in the sample",
      "A causal claim would require stronger design evidence",
      "A small p-value alone proves causation",
      "Confounding variables may contribute to the observed pattern",
      "Correlation does not show which variable, if either, drives the other"
    )

    correct <- c(
      1,
      2,
      4,
      5
    )

  } else if (task_name == "compare_correlations") {

    a <- correlation_lookup[["mpg_wt"]]
    b <- correlation_lookup[["mpg_hp"]]
    stronger <- if (
      abs(a$r) > abs(b$r)
    ) {
      "wt"
    } else {
      "hp"
    }

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "cor(mpg, wt) = ",
      fmt_num(a$r),
      " and cor(mpg, hp) = ",
      fmt_num(b$r),
      ". Which statements are correct? Select all that apply."
    )

    options <- c(
      paste0(
        stronger,
        " has the stronger bivariate linear association with mpg"
      ),
      "Both relationships are negative",
      "The stronger correlation automatically guarantees better out-of-sample prediction",
      "Absolute values should be compared when judging strength",
      "Neither correlation alone proves causation"
    )

    correct <- c(
      1,
      2,
      4,
      5
    )

    x <- "wt, hp"
    y <- "mpg"

  } else if (task_name == "outlier_claims") {

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "The sample correlation is r = ",
      fmt_num(stats$r),
      ". Which statements are reasonable? Select all that apply."
    )

    options <- c(
      "One unusual point does not automatically invalidate the overall correlation",
      "The observation should be checked for influence or data quality",
      "Correlation can remain strong even when some observations depart from the trend",
      "The outlier must be deleted without investigation",
      "A scatterplot helps assess whether the coefficient is being driven by a few points"
    )

    correct <- c(
      1,
      2,
      3,
      5
    )

  } else {

    ci_text <- format_ci(
      stats$ci_lower,
      stats$ci_upper
    )

    stem <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For mpg and hp, r = ",
      fmt_num(stats$r),
      " with a 95% confidence interval of ",
      ci_text,
      ". Which statements are supported? Select all that apply."
    )

    options <- c(
      "The plausible population correlations are negative",
      "The interval excludes zero",
      "The result supports a non-zero negative linear association",
      "The confidence interval guarantees accurate prediction for every new case",
      "Prediction quality should be checked with a fitted model and validation data"
    )

    correct <- c(
      1,
      2,
      3,
      5
    )
  }

  question <- paste0(
    stem,
    "\n\n",
    format_options(options)
  )

  reference_answer <- paste0(
    format_answer_letters(correct),
    ". Correct statements: ",
    paste(
      options[correct],
      collapse = " | "
    )
  )

  incorrect <- setdiff(
    seq_along(options),
    correct
  )

  solution_steps <- paste0(
    "1. Evaluate each statement separately. ",
    "2. Use the sign of r for direction, |r| for strength, p or the confidence interval for inferential evidence, and r-squared for shared linear variation. ",
    "3. Reject statements that confuse correlation with causation, exact equivalence or guaranteed prediction. ",
    "4. Select ",
    format_answer_letters(correct),
    ". Options ",
    format_answer_letters(incorrect),
    " are unsupported or overstated."
  )

  data.frame(
    id = sprintf("R800_036_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_036",
    dataset_name = "mtcars",
    statistical_concept = "Correlation",
    task = "correlation_method_selection",
    template_id = paste0(
      "correlation_multiple_choice_",
      task_name
    ),
    difficulty = "medium",
    scenario = "sports_analytics",
    language_style = sports_styles[i],
    question_type = "multiple_choice",
    predictor = x,
    response = y,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "multiple_choice",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate questions
# ------------------------------------------------------------

education_questions <- do.call(
  rbind,
  lapply(
    seq_len(10),
    build_education_question
  )
)

sports_questions <- do.call(
  rbind,
  lapply(
    seq_len(10),
    build_sports_question
  )
)

correlation_questions <- rbind(
  education_questions,
  sports_questions
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

correlation_questions <- correlation_questions[
  ,
  required_columns
]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(
    names(correlation_questions),
    required_columns
  )
)

stopifnot(
  nrow(correlation_questions) == 20
)

stopifnot(
  length(
    unique(correlation_questions$id)
  ) == 20
)

stopifnot(
  !anyDuplicated(
    correlation_questions$question
  )
)

stopifnot(
  sum(
    correlation_questions$blueprint_id ==
      "R800_034"
  ) == 10
)

stopifnot(
  sum(
    correlation_questions$blueprint_id ==
      "R800_036"
  ) == 10
)

stopifnot(
  all(
    education_questions$difficulty ==
      "medium"
  )
)

stopifnot(
  all(
    education_questions$question_type ==
      "single_choice"
  )
)

stopifnot(
  all(
    sports_questions$difficulty ==
      "medium"
  )
)

stopifnot(
  all(
    sports_questions$question_type ==
      "multiple_choice"
  )
)

stopifnot(
  all(
    nchar(
      correlation_questions$question
    ) >= 100
  )
)

stopifnot(
  all(
    nchar(
      correlation_questions$solution_steps
    ) >= 40
  )
)

multiple_answer_counts <- sapply(
  strsplit(
    sports_questions$reference_answer,
    "\\. Correct statements:"
  ),
  function(x) {
    length(
      strsplit(
        x[1],
        ", "
      )[[1]]
    )
  }
)

stopifnot(
  all(
    multiple_answer_counts >= 2
  )
)

# ------------------------------------------------------------
# Preview
# ------------------------------------------------------------

cat(
  "\nQuestion count by blueprint:\n"
)

print(
  table(
    correlation_questions$blueprint_id
  )
)

cat(
  "\nQuestion count by question type:\n"
)

print(
  table(
    correlation_questions$question_type
  )
)

cat(
  "\nQuestion count by difficulty:\n"
)

print(
  table(
    correlation_questions$difficulty
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
  correlation_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

cat(
  "\n\n================ R800_034 example ================\n\n"
)

cat(
  education_questions$question[1],
  "\n\nReference answer:\n",
  education_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  education_questions$solution_steps[1],
  "\n"
)

cat(
  "\n\n================ R800_036 example ================\n\n"
)

cat(
  sports_questions$question[1],
  "\n\nReference answer:\n",
  sports_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  sports_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_034_R800_036_Correlation.csv"
json_file <- "R800_034_R800_036_Correlation.json"

write.csv(
  correlation_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write_json(
  correlation_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat(
  "\n\nSuccessfully generated ",
  nrow(correlation_questions),
  " correlation questions.\n",
  sep = ""
)

cat(
  "R800_034 education single-choice questions: ",
  nrow(education_questions),
  "\n",
  sep = ""
)

cat(
  "R800_036 sports multiple-choice questions: ",
  nrow(sports_questions),
  "\n",
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
