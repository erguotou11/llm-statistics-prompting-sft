# ============================================================
# Output Interpretation Question Generator
#
# R800_051
# Dataset: CO2
# Domain: Environmental Science
# Difficulty: Hard
# Question type: Interpretation
# Count: 5
#
# R800_052
# Dataset: mtcars
# Domain: Finance
# Difficulty: Hard
# Question type: Short Answer
# Count: 5
#
# Outputs:
# 1. R800_051_R800_052_Interpretation_v2.csv
# 2. R800_051_R800_052_Interpretation_v2.json
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
# Models
# ------------------------------------------------------------

co2_additive_model <- lm(
  uptake ~ conc + Type + Treatment,
  data = CO2
)

co2_interaction_model <- lm(
  uptake ~ conc * Type + conc * Treatment,
  data = CO2
)

mtcars_model <- lm(
  mpg ~ wt + hp + am,
  data = mtcars
)

mtcars_interaction_model <- lm(
  mpg ~ wt + hp + am + wt:am,
  data = mtcars
)

# ------------------------------------------------------------
# Model summaries
# ------------------------------------------------------------

co2_additive_summary <- summary(co2_additive_model)
co2_interaction_summary <- summary(co2_interaction_model)

mtcars_summary <- summary(mtcars_model)
mtcars_interaction_summary <- summary(mtcars_interaction_model)

co2_coef <- coef(co2_additive_model)
co2_int_coef <- coef(co2_interaction_model)

mt_coef <- coef(mtcars_model)
mt_int_coef <- coef(mtcars_interaction_model)

co2_r2 <- co2_additive_summary$r.squared
co2_adj_r2 <- co2_additive_summary$adj.r.squared

mt_r2 <- mtcars_summary$r.squared
mt_adj_r2 <- mtcars_summary$adj.r.squared

# ------------------------------------------------------------
# Selected predictions and contrasts
# ------------------------------------------------------------

co2_newdata <- data.frame(
  conc = c(500, 500, 1000, 1000),
  Type = factor(
    c("Quebec", "Mississippi", "Quebec", "Mississippi"),
    levels = levels(CO2$Type)
  ),
  Treatment = factor(
    c("nonchilled", "nonchilled", "chilled", "chilled"),
    levels = levels(CO2$Treatment)
  )
)

co2_predictions <- as.numeric(
  predict(
    co2_additive_model,
    newdata = co2_newdata
  )
)

co2_ci <- as.data.frame(
  predict(
    co2_additive_model,
    newdata = co2_newdata[1, , drop = FALSE],
    interval = "confidence"
  )
)

mtcars_newdata <- data.frame(
  wt = c(3.0, 3.0, 3.5, 3.5),
  hp = c(150, 150, 180, 180),
  am = c(0, 1, 0, 1)
)

mtcars_predictions <- as.numeric(
  predict(
    mtcars_model,
    newdata = mtcars_newdata
  )
)

mtcars_ci <- as.data.frame(
  predict(
    mtcars_model,
    newdata = mtcars_newdata[2, , drop = FALSE],
    interval = "confidence"
  )
)

# ------------------------------------------------------------
# R800_051 scenarios and tasks
# ------------------------------------------------------------

environment_scenarios <- c(

  paste(
    "By the end of a controlled growth-chamber study, the project team has",
    "estimated uptake as a function of carbon-dioxide concentration, plant Type",
    "and chilling Treatment. The numerical output now has to be translated into",
    "a conclusion that an ecologist can actually use."
  ),

  paste(
    "At first glance, the coefficient for TypeMississippi appears to give a",
    "simple regional difference. Once the model structure is examined more closely,",
    "however, the meaning of that coefficient depends on the reference categories",
    "and on what is being held constant."
  ),

  paste(
    "When the concentration term is allowed to interact with plant Type and",
    "Treatment, one common slope is no longer assumed for every group. The report",
    "must explain what that change means scientifically."
  ),

  paste(
    "A policy briefing asks for the expected uptake of Quebec and Mississippi",
    "plants under the same concentration and treatment conditions. The point",
    "predictions are available, but the wording must avoid turning an adjusted",
    "difference into a causal certainty."
  ),

  paste(
    "For a final environmental summary, the analyst must combine model fit,",
    "coefficient direction, uncertainty, interaction structure and study design",
    "into one balanced interpretation rather than listing output mechanically."
  )
)

environment_styles <- c(
  "ecological-summary",
  "reference-category",
  "interaction-meaning",
  "adjusted-comparison",
  "integrated-conclusion"
)

environment_tasks <- c(
  "interpret_concentration_effect",
  "interpret_type_coefficient",
  "interpret_interaction_model",
  "interpret_adjusted_predictions",
  "balanced_environmental_summary"
)

# ------------------------------------------------------------
# R800_052 scenarios and tasks
# ------------------------------------------------------------

finance_scenarios <- c(

  paste(
    "Inside an automotive valuation model, mpg is treated as a proxy for",
    "operating efficiency, while wt, hp and transmission type enter as predictors.",
    "The investment note must explain what the fitted coefficients mean economically."
  ),

  paste(
    "A portfolio review compares automatic and manual vehicles after controlling",
    "for weight and horsepower. The fitted am coefficient is positive, but the",
    "committee wants to know whether that result can be described as a causal",
    "premium in fuel efficiency."
  ),

  paste(
    "Rather than quoting R-squared as proof that the model is 'accurate', the",
    "risk team asks what the statistic does and does not establish for valuation",
    "and forecasting."
  ),

  paste(
    "Once a wt-by-am interaction is introduced, the effect of weight is allowed",
    "to differ between automatic and manual vehicles. The finance memo must explain",
    "how that changes interpretation of the main effects."
  ),

  paste(
    "For the closing investment recommendation, one short answer must connect",
    "predicted mpg, coefficient uncertainty, possible multicollinearity, observational",
    "data and out-of-sample risk."
  )
)

finance_styles <- c(
  "valuation-interpretation",
  "controlled-comparison",
  "model-fit-caution",
  "interaction-analysis",
  "investment-governance"
)

finance_tasks <- c(
  "interpret_weight_and_hp",
  "interpret_am_coefficient",
  "interpret_r_squared",
  "interpret_wt_am_interaction",
  "balanced_finance_summary"
)

# ------------------------------------------------------------
# Build R800_051
# ------------------------------------------------------------

build_environment_question <- function(i) {

  task_name <- environment_tasks[i]

  if (task_name == "interpret_concentration_effect") {

    conc_est <- co2_coef["conc"]
    conc_p <- co2_additive_summary$coefficients["conc", "Pr(>|t|)"]

    question <- paste0(
      environment_scenarios[i],
      "\n\n",
      "In the additive model, the coefficient of conc is ",
      fmt_num(conc_est),
      " with p ",
      fmt_p(conc_p),
      ". Interpret this result in context and state the key limitation of the additive specification."
    )

    reference_answer <- paste0(
      "Holding Type and Treatment constant, a one-unit increase in carbon-dioxide concentration is associated with an average increase of ",
      fmt_num(conc_est),
      " units in predicted uptake. The small p-value provides evidence that the common slope differs from zero. ",
      "However, the additive model assumes the same concentration slope for all Types and Treatments, which may be unrealistic if biological response curves differ across groups."
    )

    solution_steps <- paste0(
      "1. Read the sign and magnitude of the conc coefficient. ",
      "2. Interpret it conditionally on Type and Treatment. ",
      "3. Use the p-value as evidence against a zero slope. ",
      "4. Add the equal-slope limitation of the additive model."
    )

  } else if (task_name == "interpret_type_coefficient") {

    type_name <- grep(
      "^Type",
      names(co2_coef),
      value = TRUE
    )[1]

    type_est <- co2_coef[type_name]
    type_p <- co2_additive_summary$coefficients[type_name, "Pr(>|t|)"]

    question <- paste0(
      environment_scenarios[i],
      "\n\n",
      "The coefficient ",
      type_name,
      " is ",
      fmt_num(type_est),
      " with p ",
      fmt_p(type_p),
      ". Explain exactly what this coefficient compares."
    )

    reference_answer <- paste0(
      "The coefficient compares Mississippi with the reference Type, Quebec, while holding concentration and Treatment constant. ",
      "A value of ",
      fmt_num(type_est),
      " means that Mississippi plants are predicted to differ by that amount in uptake relative to otherwise comparable Quebec plants. ",
      "The interpretation is conditional on the model and reference categories; it is not an unconditional raw mean difference."
    )

    solution_steps <- paste0(
      "1. Identify Quebec as the reference Type. ",
      "2. Hold conc and Treatment constant. ",
      "3. Interpret the sign and magnitude of the coefficient. ",
      "4. Distinguish an adjusted coefficient from an unadjusted group mean difference."
    )

  } else if (task_name == "interpret_interaction_model") {

    int_terms <- grep(
      ":",
      names(co2_int_coef),
      value = TRUE
    )

    question <- paste0(
      environment_scenarios[i],
      "\n\n",
      "The interaction model contains the terms ",
      paste(int_terms, collapse = ", "),
      ". What does their inclusion mean for the interpretation of concentration?"
    )

    reference_answer <- paste0(
      "The interaction terms allow the slope of uptake against concentration to vary across plant Types and Treatments. ",
      "The main conc coefficient now represents the concentration slope for the reference Type and reference Treatment only. ",
      "For another group, the relevant slope is obtained by adding the corresponding interaction adjustment."
    )

    solution_steps <- paste0(
      "1. Identify the reference categories. ",
      "2. Interpret the main conc coefficient as a reference-group slope. ",
      "3. Explain how interaction coefficients modify that slope. ",
      "4. Emphasise that one universal concentration effect is no longer assumed."
    )

  } else if (task_name == "interpret_adjusted_predictions") {

    diff_500 <- co2_predictions[2] - co2_predictions[1]
    diff_1000 <- co2_predictions[4] - co2_predictions[3]

    question <- paste0(
      environment_scenarios[i],
      "\n\n",
      "At conc = 500 under nonchilled conditions, predicted uptake is ",
      fmt_num(co2_predictions[1]),
      " for Quebec and ",
      fmt_num(co2_predictions[2]),
      " for Mississippi. Under chilled conditions at conc = 1000, the predictions are ",
      fmt_num(co2_predictions[3]),
      " and ",
      fmt_num(co2_predictions[4]),
      ". Interpret the adjusted contrasts."
    )

    reference_answer <- paste0(
      "At conc = 500 under nonchilled conditions, Mississippi is predicted to differ from Quebec by ",
      fmt_num(diff_500),
      " uptake units. At conc = 1000 under chilled conditions, the adjusted difference is ",
      fmt_num(diff_1000),
      ". ",
      "These are model-based comparisons at matched conditions. They describe adjusted association and should not be interpreted as definitive causal effects without considering the experimental design."
    )

    solution_steps <- paste0(
      "1. Compare predictions within the same concentration and treatment. ",
      "2. Calculate Mississippi minus Quebec for each condition. ",
      "3. Interpret the contrasts as adjusted differences. ",
      "4. Add a causal-design limitation."
    )

  } else {

    conc_est <- co2_coef["conc"]
    fit_ci <- format_ci(
      co2_ci$lwr,
      co2_ci$upr
    )

    question <- paste0(
      environment_scenarios[i],
      "\n\n",
      "The additive model has R-squared = ",
      fmt_num(co2_r2),
      ", adjusted R-squared = ",
      fmt_num(co2_adj_r2),
      ", a conc coefficient of ",
      fmt_num(conc_est),
      ", and a predicted mean uptake of ",
      fmt_num(co2_ci$fit),
      " for Quebec, nonchilled plants at conc = 500 with 95% CI ",
      fit_ci,
      ". Write a balanced interpretation."
    )

    reference_answer <- paste0(
      "The model explains a substantial proportion of the observed variation in uptake and estimates a positive adjusted association between concentration and uptake. ",
      "For Quebec, nonchilled plants at conc = 500, the predicted mean uptake is ",
      fmt_num(co2_ci$fit),
      " with the stated confidence interval. ",
      "The interpretation remains conditional on the linear additive form, and possible interactions, repeated measurements or nonlinear biological response should be checked before broad generalisation."
    )

    solution_steps <- paste0(
      "1. Interpret R-squared as explained sample variation, not proof of correctness. ",
      "2. Interpret the conc coefficient conditionally. ",
      "3. Report the fitted mean and confidence interval. ",
      "4. Add model-form and design limitations."
    )
  }

  data.frame(
    id = sprintf("R800_051_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_051",
    dataset_name = "CO2",
    statistical_concept = "Interpretation",
    task = "statistical_output_interpretation",
    template_id = paste0("output_interpretation_", task_name),
    difficulty = "hard",
    scenario = "environmental_science",
    language_style = environment_styles[i],
    question_type = "interpretation",
    predictor = "conc, Type, Treatment",
    response = "uptake",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "written_interpretation",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Build R800_052
# ------------------------------------------------------------

build_finance_question <- function(i) {

  task_name <- finance_tasks[i]

  if (task_name == "interpret_weight_and_hp") {

    wt_est <- mt_coef["wt"]
    hp_est <- mt_coef["hp"]

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "The fitted coefficients are wt = ",
      fmt_num(wt_est),
      " and hp = ",
      fmt_num(hp_est),
      ". Explain both coefficients and state why neither should be called a universal causal effect."
    )

    reference_answer <- paste0(
      "Holding horsepower and transmission type constant, a one-unit increase in wt is associated with an average change of ",
      fmt_num(wt_est),
      " mpg. Holding weight and transmission constant, one additional horsepower is associated with an average change of ",
      fmt_num(hp_est),
      " mpg. ",
      "Both are conditional associations from observational data, so omitted variables and model assumptions prevent a universal causal interpretation."
    )

    solution_steps <- paste0(
      "1. Interpret wt while holding hp and am constant. ",
      "2. Interpret hp while holding wt and am constant. ",
      "3. Translate signs into economic meaning for fuel efficiency. ",
      "4. Add observational and omitted-variable limitations."
    )

  } else if (task_name == "interpret_am_coefficient") {

    am_est <- mt_coef["am"]
    am_p <- mtcars_summary$coefficients["am", "Pr(>|t|)"]

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "The am coefficient is ",
      fmt_num(am_est),
      " with p ",
      fmt_p(am_p),
      ". Interpret the estimate and explain why 'manual transmission causes this mpg increase' is too strong."
    )

    reference_answer <- paste0(
      "After controlling for wt and hp, manual-transmission vehicles are predicted to have ",
      fmt_num(am_est),
      " more mpg than comparable automatic vehicles on average. ",
      "The p-value describes evidence against a zero adjusted association. Because transmission was not randomly assigned and other vehicle characteristics may differ, the coefficient should not be treated as a causal effect."
    )

    solution_steps <- paste0(
      "1. Identify automatic transmission as the reference group. ",
      "2. Interpret the am coefficient conditionally on wt and hp. ",
      "3. Use the p-value correctly. ",
      "4. Explain why observational confounding blocks a causal claim."
    )

  } else if (task_name == "interpret_r_squared") {

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "The model has R-squared = ",
      fmt_num(mt_r2),
      " and adjusted R-squared = ",
      fmt_num(mt_adj_r2),
      ". Explain what these values mean and why they do not prove strong out-of-sample forecasting."
    )

    reference_answer <- paste0(
      "R-squared indicates that the model explains about ",
      fmt_pct(mt_r2),
      " of the sample variation in mpg, while adjusted R-squared accounts for the number of predictors. ",
      "Neither statistic measures future forecast error directly. Strong in-sample fit can coexist with overfitting, unstable coefficients or poor performance in a different vehicle population."
    )

    solution_steps <- paste0(
      "1. Interpret R-squared as in-sample explained variation. ",
      "2. Explain the adjustment for model size. ",
      "3. Distinguish fit from predictive validation. ",
      "4. Recommend cross-validation or an external test set."
    )

  } else if (task_name == "interpret_wt_am_interaction") {

    interaction_name <- grep(
      "wt:am|am:wt",
      names(mt_int_coef),
      value = TRUE
    )[1]

    interaction_est <- mt_int_coef[interaction_name]

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "The interaction coefficient ",
      interaction_name,
      " is ",
      fmt_num(interaction_est),
      ". Explain how this changes the interpretation of wt and am."
    )

    reference_answer <- paste0(
      "With the interaction included, the wt coefficient represents the weight slope for the reference transmission group, automatic vehicles. ",
      "For manual vehicles, the weight slope equals the wt coefficient plus ",
      fmt_num(interaction_est),
      ". ",
      "The am main effect now represents the manual-versus-automatic difference when wt equals zero, so practical comparisons should be made at meaningful weight values."
    )

    solution_steps <- paste0(
      "1. Identify automatic as the reference transmission. ",
      "2. Interpret wt as the automatic-group slope. ",
      "3. Add the interaction term to obtain the manual-group slope. ",
      "4. Explain why the am main effect at wt = 0 may have limited practical meaning."
    )

  } else {

    pred_manual <- mtcars_predictions[2]
    ci_text <- format_ci(
      mtcars_ci$lwr,
      mtcars_ci$upr
    )

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "For a manual vehicle with wt = 3.0 and hp = 150, predicted mpg is ",
      fmt_num(pred_manual),
      " with 95% confidence interval ",
      ci_text,
      ". Using the coefficient estimates and model diagnostics, write a balanced investment conclusion."
    )

    reference_answer <- paste0(
      "The model predicts mean fuel efficiency of ",
      fmt_num(pred_manual),
      " mpg for a manual vehicle with the stated specifications, subject to the reported uncertainty. ",
      "Weight and horsepower are associated with lower mpg, while manual transmission has an adjusted positive association. ",
      "The result should be treated as a scenario estimate rather than a valuation certainty because the sample is small, the data are observational, predictors may be correlated and external market conditions are not represented."
    )

    solution_steps <- paste0(
      "1. Report the fitted value and confidence interval. ",
      "2. Summarise the directions of wt, hp and am. ",
      "3. Distinguish conditional association from causation. ",
      "4. Add sample-size, multicollinearity and out-of-sample limitations."
    )
  }

  data.frame(
    id = sprintf("R800_052_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_052",
    dataset_name = "mtcars",
    statistical_concept = "Interpretation",
    task = "statistical_output_reasoning",
    template_id = paste0("output_interpretation_", task_name),
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
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate all questions
# ------------------------------------------------------------

environment_questions <- do.call(
  rbind,
  lapply(seq_len(5), build_environment_question)
)

finance_questions <- do.call(
  rbind,
  lapply(seq_len(5), build_finance_question)
)

interpretation_questions <- rbind(
  environment_questions,
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

interpretation_questions <- interpretation_questions[, required_columns]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(names(interpretation_questions), required_columns),
  nrow(interpretation_questions) == 10,
  length(unique(interpretation_questions$id)) == 10,
  !anyDuplicated(interpretation_questions$question),
  sum(interpretation_questions$blueprint_id == "R800_051") == 5,
  sum(interpretation_questions$blueprint_id == "R800_052") == 5,
  all(environment_questions$difficulty == "hard"),
  all(environment_questions$question_type == "interpretation"),
  all(finance_questions$difficulty == "hard"),
  all(finance_questions$question_type == "short_answer"),
  all(nchar(interpretation_questions$question) >= 120),
  all(nchar(interpretation_questions$reference_answer) >= 120),
  all(nchar(interpretation_questions$solution_steps) >= 60)
)

# ------------------------------------------------------------
# Preview
# ------------------------------------------------------------

cat("\nQuestion count by blueprint:\n")
print(table(interpretation_questions$blueprint_id))

cat("\nQuestion count by question type:\n")
print(table(interpretation_questions$question_type))

cat("\nQuestion count by dataset:\n")
print(table(interpretation_questions$dataset_name))

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
  interpretation_questions[, preview_columns],
  row.names = FALSE
)

cat("\n\n================ R800_051 example ================\n\n")
cat(
  environment_questions$question[1],
  "\n\nReference answer:\n",
  environment_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  environment_questions$solution_steps[1],
  "\n"
)

cat("\n\n================ R800_052 example ================\n\n")
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

csv_file <- "R800_051_R800_052_Interpretation_v2.csv"
json_file <- "R800_051_R800_052_Interpretation_v2.json"

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
  "R800_051 environmental interpretation questions: ",
  nrow(environment_questions),
  "\n",
  sep = ""
)

cat(
  "R800_052 finance short-answer questions: ",
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
