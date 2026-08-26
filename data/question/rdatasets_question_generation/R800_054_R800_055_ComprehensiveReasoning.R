# ============================================================
# Comprehensive Reasoning Question Generator
#
# R800_054
# Dataset: mtcars
# Domain: Finance
# Difficulty: Hard
# Question type: Short Answer
# Count: 5
#
# R800_055
# Dataset: ToothGrowth
# Domain: Healthcare
# Difficulty: Hard
# Question type: Short Answer
# Count: 5
#
# Outputs:
# 1. R800_054_R800_055_ComprehensiveReasoning_v2_1.csv
# 2. R800_054_R800_055_ComprehensiveReasoning_v2_1.json
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
  if (length(x) == 0 || is.na(x[1]) || !is.finite(x[1])) {
    return("NA")
  }

  formatC(
    as.numeric(x[1]),
    format = "f",
    digits = digits
  )
}

fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p[1]) || !is.finite(p[1])) {
    return("NA")
  }

  p <- as.numeric(p[1])

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

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
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

# ------------------------------------------------------------
# mtcars models and summaries
# ------------------------------------------------------------

mtcars_model <- lm(
  mpg ~ wt + hp + am,
  data = mtcars
)

mtcars_interaction_model <- lm(
  mpg ~ wt + hp + am + wt:am,
  data = mtcars
)

mtcars_summary <- summary(mtcars_model)
mtcars_interaction_summary <- summary(mtcars_interaction_model)

mt_coef <- coef(mtcars_model)
mt_int_coef <- coef(mtcars_interaction_model)

mt_r2 <- mtcars_summary$r.squared
mt_adj_r2 <- mtcars_summary$adj.r.squared

mt_residual_shapiro <- shapiro.test(
  residuals(mtcars_model)
)

mt_cor_wt_hp <- cor(
  mtcars$wt,
  mtcars$hp
)

mt_vif_like_wt <- 1 / (
  1 - summary(
    lm(
      wt ~ hp + am,
      data = mtcars
    )
  )$r.squared
)

mt_vif_like_hp <- 1 / (
  1 - summary(
    lm(
      hp ~ wt + am,
      data = mtcars
    )
  )$r.squared
)

mt_manual_profile <- data.frame(
  wt = 3.0,
  hp = 150,
  am = 1
)

mt_auto_profile <- data.frame(
  wt = 3.0,
  hp = 150,
  am = 0
)

mt_manual_prediction <- as.data.frame(
  predict(
    mtcars_model,
    newdata = mt_manual_profile,
    interval = "confidence"
  )
)

mt_auto_prediction <- as.data.frame(
  predict(
    mtcars_model,
    newdata = mt_auto_profile,
    interval = "confidence"
  )
)

mt_manual_minus_auto <- mt_manual_prediction$fit -
  mt_auto_prediction$fit

mt_reduced_model <- lm(
  mpg ~ wt + hp,
  data = mtcars
)

mt_model_comparison <- anova(
  mt_reduced_model,
  mtcars_model
)

mt_am_partial_p <- mt_model_comparison$`Pr(>F)`[2]

stopifnot(
  all(
    c("wt", "hp", "am") %in%
      names(mt_coef)
  ),
  is.finite(mt_r2),
  is.finite(mt_adj_r2),
  is.finite(mt_residual_shapiro$p.value),
  is.finite(mt_cor_wt_hp),
  is.finite(mt_vif_like_wt),
  is.finite(mt_vif_like_hp),
  is.finite(mt_am_partial_p)
)

# ------------------------------------------------------------
# ToothGrowth models and summaries
# ------------------------------------------------------------

ToothGrowth$dose_factor <- factor(
  ToothGrowth$dose
)

tooth_anova_model <- aov(
  len ~ supp * dose_factor,
  data = ToothGrowth
)

tooth_anova_summary <- summary(
  tooth_anova_model
)

tooth_anova_table <- tooth_anova_summary[[1]]

clean_anova_names <- function(x) {
  trimws(as.character(x))
}

get_anova_p <- function(anova_table, term) {

  row_names <- clean_anova_names(
    rownames(anova_table)
  )

  column_names <- clean_anova_names(
    colnames(anova_table)
  )

  row_index <- match(
    term,
    row_names
  )

  p_column_index <- match(
    "Pr(>F)",
    column_names
  )

  if (
    is.na(row_index) ||
      is.na(p_column_index)
  ) {
    stop(
      paste0(
        "Could not extract ANOVA p-value for term '",
        term,
        "'. Available rows: ",
        paste(row_names, collapse = ", "),
        "; available columns: ",
        paste(column_names, collapse = ", "),
        "."
      )
    )
  }

  p_value <- as.numeric(
    anova_table[
      row_index,
      p_column_index
    ]
  )

  if (
    length(p_value) == 0 ||
      is.na(p_value) ||
      !is.finite(p_value)
  ) {
    stop(
      paste0(
        "ANOVA p-value for term '",
        term,
        "' is missing or non-finite."
      )
    )
  }

  p_value
}

tooth_supp_p <- get_anova_p(
  tooth_anova_table,
  "supp"
)

tooth_dose_p <- get_anova_p(
  tooth_anova_table,
  "dose_factor"
)

tooth_interaction_p <- get_anova_p(
  tooth_anova_table,
  "supp:dose_factor"
)

tooth_lm_model <- lm(
  len ~ supp * dose,
  data = ToothGrowth
)

tooth_lm_summary <- summary(
  tooth_lm_model
)

tooth_group_means <- aggregate(
  len ~ supp + dose,
  data = ToothGrowth,
  FUN = mean
)

tooth_group_sds <- aggregate(
  len ~ supp + dose,
  data = ToothGrowth,
  FUN = sd
)

tooth_group_ns <- aggregate(
  len ~ supp + dose,
  data = ToothGrowth,
  FUN = length
)

names(tooth_group_sds)[3] <- "sd"
names(tooth_group_ns)[3] <- "n"

tooth_group_summary <- merge(
  tooth_group_means,
  tooth_group_sds,
  by = c("supp", "dose")
)

tooth_group_summary <- merge(
  tooth_group_summary,
  tooth_group_ns,
  by = c("supp", "dose")
)

tooth_vc_05 <- subset(
  ToothGrowth,
  supp == "VC" & dose == 0.5
)$len

tooth_oj_05 <- subset(
  ToothGrowth,
  supp == "OJ" & dose == 0.5
)$len

tooth_test_05 <- t.test(
  tooth_oj_05,
  tooth_vc_05,
  var.equal = FALSE
)

tooth_vc_20 <- subset(
  ToothGrowth,
  supp == "VC" & dose == 2.0
)$len

tooth_oj_20 <- subset(
  ToothGrowth,
  supp == "OJ" & dose == 2.0
)$len

tooth_test_20 <- t.test(
  tooth_oj_20,
  tooth_vc_20,
  var.equal = FALSE
)

tooth_05_difference <- mean(tooth_oj_05) -
  mean(tooth_vc_05)

tooth_20_difference <- mean(tooth_oj_20) -
  mean(tooth_vc_20)

tooth_residual_shapiro <- shapiro.test(
  residuals(tooth_lm_model)
)

tooth_homogeneity_proxy <- max(
  tooth_group_summary$sd
) / min(
  tooth_group_summary$sd
)

tooth_pred_grid <- expand.grid(
  supp = levels(ToothGrowth$supp),
  dose = c(0.5, 1.0, 2.0)
)

tooth_pred_grid$predicted_len <- as.numeric(
  predict(
    tooth_lm_model,
    newdata = tooth_pred_grid
  )
)

# ------------------------------------------------------------
# Scenario banks
# ------------------------------------------------------------

finance_scenarios <- c(

  paste(
    "Ahead of an automotive-sector investment meeting, the research team must",
    "decide whether weight, horsepower and transmission type jointly provide a",
    "credible explanation of fuel efficiency. The committee does not want a list",
    "of coefficients; it wants a defensible analytical argument."
  ),

  paste(
    "One valuation memo claims that manual transmission creates a fuel-economy",
    "premium after controlling for vehicle weight and horsepower. Before that",
    "statement reaches clients, the model evidence and causal wording need to be",
    "examined together."
  ),

  paste(
    "Rather than relying on R-squared alone, the model-risk group checks residual",
    "normality, predictor dependence and the stability of the fitted coefficients.",
    "The task is to judge whether the model is suitable for scenario analysis."
  ),

  paste(
    "Once a wt-by-am interaction is introduced, the effect of weight is no longer",
    "assumed to be identical for automatic and manual vehicles. The investment",
    "team asks whether the added complexity is substantively useful."
  ),

  paste(
    "For the final portfolio note, a predicted mpg value for a specific vehicle",
    "must be combined with uncertainty, model limitations and a recommendation",
    "about how much confidence to place in the result."
  )
)

finance_styles <- c(
  "investment-review",
  "causal-claim-audit",
  "model-risk",
  "interaction-evaluation",
  "portfolio-recommendation"
)

finance_tasks <- c(
  "integrated_model_assessment",
  "manual_transmission_claim",
  "diagnostic_reasoning",
  "interaction_reasoning",
  "prediction_governance"
)

healthcare_scenarios <- c(

  paste(
    "During review of a nutritional intervention study, tooth length is compared",
    "across supplement types and dose levels. The clinical team needs one coherent",
    "interpretation that connects group means, hypothesis tests and the interaction",
    "between supplement and dose."
  ),

  paste(
    "At the lowest dose, orange juice appears to outperform vitamin C by a wide",
    "margin; at the highest dose, the difference is much smaller. The research",
    "summary must explain why one overall supplement effect would be misleading."
  ),

  paste(
    "Rather than stopping at an ANOVA p-value, the evidence review considers",
    "effect size, uncertainty, residual assumptions and the practical importance",
    "of the observed differences."
  ),

  paste(
    "A clinician asks whether increasing dose has the same expected benefit under",
    "both supplements. The fitted interaction model provides an answer, but only",
    "if the coefficients and predicted means are interpreted together."
  ),

  paste(
    "For a final healthcare recommendation, the analyst must weigh efficacy,",
    "dose-response shape, uncertainty and study design before proposing which",
    "supplement-dose combination deserves follow-up."
  )
)

healthcare_styles <- c(
  "clinical-evidence-synthesis",
  "dose-specific-contrast",
  "assumption-and-effect",
  "interaction-analysis",
  "treatment-recommendation"
)

healthcare_tasks <- c(
  "integrated_anova_reasoning",
  "dose_specific_effect",
  "assumptions_and_practicality",
  "interaction_and_prediction",
  "balanced_clinical_recommendation"
)

# ------------------------------------------------------------
# Build R800_054 questions
# ------------------------------------------------------------

build_finance_question <- function(i) {

  task_name <- finance_tasks[i]

  if (task_name == "integrated_model_assessment") {

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "The model mpg ~ wt + hp + am has R-squared = ",
      fmt_num(mt_r2),
      " and adjusted R-squared = ",
      fmt_num(mt_adj_r2),
      ". The wt coefficient is ",
      fmt_num(mt_coef["wt"]),
      ", the hp coefficient is ",
      fmt_num(mt_coef["hp"]),
      ", and the am coefficient is ",
      fmt_num(mt_coef["am"]),
      ". Write an integrated assessment of the model."
    )

    reference_answer <- paste0(
      "The model explains a large share of the sample variation in mpg and indicates",
      " that greater weight and horsepower are associated with lower fuel economy,",
      " while manual transmission has a positive adjusted association. These effects",
      " are conditional on the other predictors. The model is useful for structured",
      " scenario analysis, but the observational design, small sample and possible",
      " predictor dependence limit causal and out-of-sample claims."
    )

    solution_steps <- paste0(
      "1. Interpret overall fit using R-squared and adjusted R-squared. ",
      "2. Interpret wt, hp and am conditionally. ",
      "3. Separate association from causation. ",
      "4. Add sample-size and external-validity limitations. ",
      "5. Conclude whether the model is suitable for scenario analysis rather than definitive valuation."
    )

  } else if (task_name == "manual_transmission_claim") {

    am_p <- mtcars_summary$coefficients["am", "Pr(>|t|)"]

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "For wt = 3.0 and hp = 150, predicted mpg is ",
      fmt_num(mt_manual_prediction$fit),
      " for a manual vehicle and ",
      fmt_num(mt_auto_prediction$fit),
      " for an automatic vehicle. The adjusted difference is ",
      fmt_num(mt_manual_minus_auto),
      " mpg, and the am coefficient has p ",
      fmt_p(am_p),
      ". Evaluate the claim."
    )

    reference_answer <- paste0(
      "The fitted model predicts a manual-versus-automatic difference of ",
      fmt_num(mt_manual_minus_auto),
      " mpg at the stated weight and horsepower. The coefficient and p-value support",
      " an adjusted association, but they do not prove that changing transmission",
      " alone causes the difference. Transmission choice may be linked to design,",
      " gearing, market segment and other omitted factors."
    )

    solution_steps <- paste0(
      "1. Compare the matched predictions. ",
      "2. Interpret the am coefficient as an adjusted mean difference. ",
      "3. Use the p-value as evidence against zero association. ",
      "4. Identify possible confounding. ",
      "5. Replace causal wording with conditional association language."
    )

  } else if (task_name == "diagnostic_reasoning") {

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Residual Shapiro-Wilk p = ",
      fmt_num(mt_residual_shapiro$p.value, 6),
      ", cor(wt, hp) = ",
      fmt_num(mt_cor_wt_hp),
      ", VIF-like values are ",
      fmt_num(mt_vif_like_wt),
      " for wt and ",
      fmt_num(mt_vif_like_hp),
      " for hp. Explain what these diagnostics imply."
    )

    reference_answer <- paste0(
      "The residual normality result does not provide strong evidence against normal",
      " errors at the 5% level, although graphical checks are still needed. Weight and",
      " horsepower are positively correlated, so their separate coefficients may be",
      " less stable than the overall prediction. The VIF-like values do not necessarily",
      " indicate extreme multicollinearity, but the dependence should still be reported",
      " and coefficient sensitivity examined."
    )

    solution_steps <- paste0(
      "1. Interpret the residual normality p-value cautiously. ",
      "2. Interpret the wt-hp correlation as predictor dependence. ",
      "3. Use the VIF-like values to assess severity. ",
      "4. Distinguish coefficient instability from predictive usefulness. ",
      "5. Recommend residual plots and sensitivity checks."
    )

  } else if (task_name == "interaction_reasoning") {

    interaction_candidates <- grep(
      "wt:am|am:wt",
      names(mt_int_coef),
      value = TRUE
    )

    if (length(interaction_candidates) == 0) {
      stop("No wt-by-am interaction coefficient was found.")
    }

    interaction_name <- interaction_candidates[1]
    interaction_est <- unname(
      mt_int_coef[interaction_name]
    )

    interaction_p <- as.numeric(
      mtcars_interaction_summary$coefficients[
        interaction_name,
        "Pr(>|t|)"
      ]
    )

    if (
      is.na(interaction_est) ||
        is.na(interaction_p)
    ) {
      stop("The wt-by-am interaction estimate or p-value is missing.")
    }

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "The interaction coefficient ",
      interaction_name,
      " is ",
      fmt_num(interaction_est),
      " with p ",
      fmt_p(interaction_p),
      ". Explain the change in model interpretation and whether the interaction",
      " should automatically be retained."
    )

    reference_answer <- paste0(
      "The interaction allows the weight slope to differ by transmission type.",
      " The wt coefficient now describes automatic vehicles, while the manual slope",
      " equals the wt coefficient plus ",
      fmt_num(interaction_est),
      ". The interaction should not be retained automatically: its uncertainty,",
      " interpretability, sample size and improvement in prediction or fit should all",
      " be considered."
    )

    solution_steps <- paste0(
      "1. Identify the reference transmission. ",
      "2. Interpret the automatic-group weight slope. ",
      "3. Add the interaction term for the manual-group slope. ",
      "4. Use the p-value and model purpose to assess retention. ",
      "5. Balance complexity against substantive value."
    )

  } else {

    pred_fit <- mt_manual_prediction$fit
    pred_lwr <- mt_manual_prediction$lwr
    pred_upr <- mt_manual_prediction$upr

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "For a manual vehicle with wt = 3.0 and hp = 150, predicted mean mpg is ",
      fmt_num(pred_fit),
      " with 95% confidence interval ",
      format_ci(pred_lwr, pred_upr),
      ". Write a recommendation on how this result should be used in an investment decision."
    )

    reference_answer <- paste0(
      "The estimate provides a reasonable model-based scenario for vehicles with",
      " similar characteristics, and the confidence interval quantifies uncertainty",
      " in the mean response. It should not be treated as a guaranteed outcome for one",
      " vehicle or as a complete valuation signal. Investment use should combine the",
      " prediction with model validation, newer vehicle data, operating-cost assumptions",
      " and sensitivity analysis."
    )

    solution_steps <- paste0(
      "1. Interpret the point prediction. ",
      "2. Distinguish a confidence interval for the mean from individual uncertainty. ",
      "3. Identify observational and external-validity limits. ",
      "4. Connect statistical output to financial decision criteria. ",
      "5. Recommend validation and sensitivity analysis."
    )
  }

  data.frame(
    id = sprintf("R800_054_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_054",
    dataset_name = "mtcars",
    statistical_concept = "Comprehensive Reasoning",
    task = "multi_step_statistical_reasoning",
    template_id = paste0("multi_step_reasoning_", task_name),
    difficulty = "hard",
    scenario = "finance",
    language_style = finance_styles[i],
    question_type = "short_answer",
    predictor = "wt, hp, am",
    response = "mpg",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_reasoning",
    version = "v2.1",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Build R800_055 questions
# ------------------------------------------------------------

build_healthcare_question <- function(i) {

  task_name <- healthcare_tasks[i]

  if (task_name == "integrated_anova_reasoning") {

    supp_p <- tooth_supp_p
    dose_p <- tooth_dose_p
    interaction_p <- tooth_interaction_p

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The ANOVA gives p ",
      fmt_p(supp_p),
      " for supplement, p ",
      fmt_p(dose_p),
      " for dose, and p ",
      fmt_p(interaction_p),
      " for the supplement-by-dose interaction. Write an integrated interpretation."
    )

    reference_answer <- paste0(
      "Dose has a strong association with tooth growth, and the supplement effect",
      " cannot be summarised by one constant difference if the interaction is important.",
      " The interaction means that the relative performance of OJ and VC changes across",
      " dose levels. Therefore, dose-specific contrasts and predicted means are more",
      " informative than reporting only overall main effects."
    )

    solution_steps <- paste0(
      "1. Interpret the dose main effect. ",
      "2. Interpret the supplement main effect conditionally. ",
      "3. Evaluate the interaction. ",
      "4. Explain why interaction changes the meaning of main effects. ",
      "5. Recommend dose-specific comparisons."
    )

  } else if (task_name == "dose_specific_effect") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "At dose 0.5, OJ minus VC = ",
      fmt_num(tooth_05_difference),
      " with 95% CI ",
      format_ci(
        tooth_test_05$conf.int[1],
        tooth_test_05$conf.int[2]
      ),
      " and p ",
      fmt_p(tooth_test_05$p.value),
      ". At dose 2.0, the difference is ",
      fmt_num(tooth_20_difference),
      " with p ",
      fmt_p(tooth_test_20$p.value),
      ". Explain the pattern."
    )

    reference_answer <- paste0(
      "OJ shows a substantial advantage over VC at the low dose, with the confidence",
      " interval and p-value supporting a real dose-specific difference. At the high dose,",
      " the supplement difference is much smaller and may not be statistically compelling.",
      " This changing contrast is consistent with a supplement-by-dose interaction and",
      " argues against reporting one universal supplement effect."
    )

    solution_steps <- paste0(
      "1. Interpret the low-dose contrast and interval. ",
      "2. Interpret the high-dose contrast and p-value. ",
      "3. Compare the two effect sizes. ",
      "4. Link the changing difference to interaction. ",
      "5. Avoid averaging away dose-specific behaviour."
    )

  } else if (task_name == "assumptions_and_practicality") {

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Residual Shapiro-Wilk p = ",
      fmt_num(tooth_residual_shapiro$p.value, 6),
      ", and the ratio of the largest to smallest group SD is ",
      fmt_num(tooth_homogeneity_proxy),
      ". Explain what these diagnostics do and do not establish."
    )

    reference_answer <- paste0(
      "The residual normality test does not by itself show a serious violation if",
      " the p-value is not small, but a Q-Q plot should still be examined. The SD ratio",
      " gives a rough indication of variance heterogeneity; it is not a formal test.",
      " Even if assumptions are approximately satisfied, practical importance still",
      " depends on the size of the dose and supplement effects, not only on p-values."
    )

    solution_steps <- paste0(
      "1. Interpret the residual normality result. ",
      "2. Interpret the SD ratio as a rough variance check. ",
      "3. State the limitations of both diagnostics. ",
      "4. Separate statistical significance from clinical relevance. ",
      "5. Recommend graphical checks and robust alternatives if needed."
    )

  } else if (task_name == "interaction_and_prediction") {

    pred_text <- paste(
      paste0(
        tooth_pred_grid$supp,
        " at dose ",
        tooth_pred_grid$dose,
        ": ",
        fmt_num(tooth_pred_grid$predicted_len)
      ),
      collapse = "; "
    )

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The fitted predictions are ",
      pred_text,
      ". Explain how these values answer the clinician's question about whether",
      " dose has the same effect under both supplements."
    )

    reference_answer <- paste0(
      "The predicted means should be compared within each supplement across dose and",
      " between supplements at the same dose. If the dose-related increase differs",
      " between OJ and VC, the model supports an interaction rather than one common",
      " dose-response slope. The predictions are model-based averages and should not",
      " be treated as guaranteed outcomes for individual subjects."
    )

    solution_steps <- paste0(
      "1. Compare predicted means across dose within OJ. ",
      "2. Repeat within VC. ",
      "3. Compare OJ and VC at matched doses. ",
      "4. Identify whether the dose-response pattern differs by supplement. ",
      "5. Add an individual-outcome limitation."
    )

  } else {

    best_row <- tooth_group_summary[
      which.max(tooth_group_summary$len),
      ,
      drop = FALSE
    ]

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The largest observed group mean is ",
      fmt_num(best_row$len),
      " for supplement ",
      as.character(best_row$supp),
      " at dose ",
      fmt_num(best_row$dose, 1),
      ". Write a balanced recommendation for follow-up research."
    )

    reference_answer <- paste0(
      "The ",
      as.character(best_row$supp),
      " supplement at dose ",
      fmt_num(best_row$dose, 1),
      " has the highest observed mean tooth length in this dataset, making it a",
      " reasonable candidate for follow-up. However, the recommendation should also",
      " consider uncertainty, interaction patterns, possible plateauing, safety, dose",
      " burden and the experimental context. Replication and clinically relevant effect",
      " thresholds are needed before treatment guidance."
    )

    solution_steps <- paste0(
      "1. Identify the highest group mean. ",
      "2. Compare it with nearby dose-supplement combinations. ",
      "3. Consider interaction and diminishing returns. ",
      "4. Add uncertainty, safety and design limitations. ",
      "5. Recommend replication rather than immediate clinical adoption."
    )
  }

  data.frame(
    id = sprintf("R800_055_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_055",
    dataset_name = "ToothGrowth",
    statistical_concept = "Comprehensive Reasoning",
    task = "multi_step_statistical_reasoning",
    template_id = paste0("multi_step_reasoning_", task_name),
    difficulty = "hard",
    scenario = "healthcare",
    language_style = healthcare_styles[i],
    question_type = "short_answer",
    predictor = "supp, dose",
    response = "len",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_reasoning",
    version = "v2.1",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate all questions
# ------------------------------------------------------------

finance_questions <- do.call(
  rbind,
  lapply(seq_len(5), build_finance_question)
)

healthcare_questions <- do.call(
  rbind,
  lapply(seq_len(5), build_healthcare_question)
)

reasoning_questions <- rbind(
  finance_questions,
  healthcare_questions
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

reasoning_questions <- reasoning_questions[, required_columns]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(names(reasoning_questions), required_columns),
  nrow(reasoning_questions) == 10,
  length(unique(reasoning_questions$id)) == 10,
  !anyDuplicated(reasoning_questions$question),
  sum(reasoning_questions$blueprint_id == "R800_054") == 5,
  sum(reasoning_questions$blueprint_id == "R800_055") == 5,
  all(finance_questions$difficulty == "hard"),
  all(finance_questions$question_type == "short_answer"),
  all(healthcare_questions$difficulty == "hard"),
  all(healthcare_questions$question_type == "short_answer"),
  all(nchar(reasoning_questions$question) >= 140),
  all(nchar(reasoning_questions$reference_answer) >= 140),
  all(nchar(reasoning_questions$solution_steps) >= 80)
)

# ------------------------------------------------------------
# Preview
# ------------------------------------------------------------

cat("\nToothGrowth ANOVA table used by the generator:\n")
print(tooth_anova_table)

cat("\nQuestion count by blueprint:\n")
print(table(reasoning_questions$blueprint_id))

cat("\nQuestion count by dataset:\n")
print(table(reasoning_questions$dataset_name))

cat("\nLanguage styles:\n")
print(table(reasoning_questions$language_style))

preview_columns <- c(
  "id",
  "dataset_name",
  "difficulty",
  "scenario",
  "language_style",
  "question_type",
  "predictor",
  "response",
  "template_id"
)

print(
  reasoning_questions[, preview_columns],
  row.names = FALSE
)

cat("\n\n================ R800_054 example ================\n\n")

cat(
  finance_questions$question[1],
  "\n\nReference answer:\n",
  finance_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  finance_questions$solution_steps[1],
  "\n"
)

cat("\n\n================ R800_055 example ================\n\n")

cat(
  healthcare_questions$question[1],
  "\n\nReference answer:\n",
  healthcare_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  healthcare_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_054_R800_055_ComprehensiveReasoning_v2_1.csv"
json_file <- "R800_054_R800_055_ComprehensiveReasoning_v2_1.json"

write.csv(
  reasoning_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write_json(
  reasoning_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat(
  "\n\nSuccessfully generated ",
  nrow(reasoning_questions),
  " comprehensive-reasoning questions.\n",
  sep = ""
)

cat(
  "R800_054 finance questions: ",
  nrow(finance_questions),
  "\n",
  sep = ""
)

cat(
  "R800_055 healthcare questions: ",
  nrow(healthcare_questions),
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
