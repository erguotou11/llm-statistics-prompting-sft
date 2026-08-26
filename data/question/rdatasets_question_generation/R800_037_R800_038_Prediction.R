# ============================================================
# Prediction Question Generator
#
# R800_037
# Dataset: mtcars
# Domain: Finance
# Difficulty: Medium
# Question type: Calculation
# Count: 20
#
# R800_038
# Dataset: CO2
# Domain: Environmental Science
# Difficulty: Medium
# Question type: Calculation
# Count: 15
#
# Outputs:
# 1. R800_037_R800_038_Prediction_v2.csv
# 2. R800_037_R800_038_Prediction_v2.json
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

format_interval <- function(lower, upper, digits = 3) {
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

mtcars_model <- lm(
  mpg ~ wt + hp,
  data = mtcars
)

co2_model <- lm(
  uptake ~ conc + Type + Treatment,
  data = CO2
)

# ------------------------------------------------------------
# Prediction helpers
# ------------------------------------------------------------

predict_point <- function(model, newdata) {
  as.numeric(
    predict(
      model,
      newdata = newdata
    )
  )
}

predict_confidence <- function(model, newdata, level = 0.95) {
  as.data.frame(
    predict(
      model,
      newdata = newdata,
      interval = "confidence",
      level = level
    )
  )
}

predict_observation <- function(model, newdata, level = 0.95) {
  as.data.frame(
    predict(
      model,
      newdata = newdata,
      interval = "prediction",
      level = level
    )
  )
}

# ------------------------------------------------------------
# Coefficients and model summaries
# ------------------------------------------------------------

mt_coef <- coef(mtcars_model)
co2_coef <- coef(co2_model)

mt_r2 <- summary(mtcars_model)$r.squared
co2_r2 <- summary(co2_model)$r.squared

# ============================================================
# R800_037
# mtcars + Finance + Medium + Calculation
# ============================================================

finance_scenarios <- c(

  paste(
    "A vehicle-finance desk is reviewing the fuel-cost assumptions used",
    "in a lease-pricing model. For one candidate car, the available specifications",
    "are a weight of 3.0 thousand pounds and 110 horsepower."
  ),

  paste(
    "The credit team has shortlisted two cars with identical horsepower",
    "but noticeably different weights. Before estimating running-cost exposure,",
    "it wants predicted mpg for both vehicles."
  ),

  paste(
    "In a residual-value report, one model is described by",
    "mpg = b0 + b1 × wt + b2 × hp. A proposed vehicle weighs",
    "2.8 thousand pounds and produces 150 horsepower."
  ),

  paste(
    "A fleet-cost scenario assumes a vehicle loses 0.4 thousand pounds",
    "while horsepower remains unchanged. The finance model is used to estimate",
    "how much predicted mpg would move."
  ),

  paste(
    "Two versions of the same model are under consideration:",
    "one has 100 horsepower and the other 160 horsepower, while both weigh",
    "3.2 thousand pounds. The expected mpg gap is needed."
  ),

  paste(
    "A lender's affordability calculator already contains the fitted",
    "mpg ~ wt + hp model. For a 3.5-thousand-pound, 180-horsepower car,",
    "the point prediction must be reconstructed from the coefficients."
  ),

  paste(
    "The underwriting memo asks for a 95% confidence interval for the",
    "mean fuel economy of vehicles with wt = 2.6 and hp = 95."
  ),

  paste(
    "For a single future vehicle with wt = 2.6 and hp = 95,",
    "the risk team needs a 95% prediction interval rather than a confidence interval."
  ),

  paste(
    "A portfolio analyst compares a light-performance car",
    "(wt = 2.2, hp = 170) with a heavier lower-power model",
    "(wt = 3.8, hp = 120)."
  ),

  paste(
    "The model-development notes report the fitted coefficients but not",
    "the predicted mpg for a vehicle at wt = 4.0 and hp = 175.",
    "That value must be calculated directly."
  ),

  paste(
    "A pricing committee asks how much predicted mpg changes when",
    "horsepower rises by 40 units at a fixed vehicle weight."
  ),

  paste(
    "A scenario analysis holds horsepower at 130 and changes weight",
    "from 2.5 to 3.5 thousand pounds. The resulting movement in predicted",
    "fuel economy is required."
  ),

  paste(
    "The finance team wants the marginal prediction effect of reducing",
    "weight by 0.6 thousand pounds while leaving horsepower unchanged."
  ),

  paste(
    "A valuation spreadsheet contains two cases:",
    "Case A has wt = 3.0 and hp = 120; Case B has wt = 3.0 and hp = 200.",
    "The model-implied mpg difference is missing."
  ),

  paste(
    "To check whether a manual worksheet matches R, the analyst computes",
    "the predicted mpg for wt = 2.9 and hp = 140 both from the regression",
    "equation and from predict()."
  ),

  paste(
    "A sensitivity table is being built around a baseline car",
    "with wt = 3.1 and hp = 140. One row asks for the predicted mpg",
    "after weight increases by 0.3 and horsepower decreases by 20."
  ),

  paste(
    "The lease-risk model is evaluated at the sample-average values",
    "of wt and hp. The corresponding fitted mpg is needed for the report."
  ),

  paste(
    "A new product proposal has wt = 1.9 and hp = 90.",
    "Because this lies near the lighter end of the observed range,",
    "the team wants both the point prediction and a brief numerical interpretation."
  ),

  paste(
    "A stress test compares a heavy high-power vehicle",
    "(wt = 5.0, hp = 250) with a lighter moderate-power vehicle",
    "(wt = 2.4, hp = 110)."
  ),

  paste(
    "For the final investment note, the analyst must report the predicted mpg",
    "for wt = 3.3 and hp = 160, together with the 95% confidence interval",
    "for the mean response."
  )
)

finance_styles <- c(
  "lease-pricing",
  "comparative",
  "equation-based",
  "sensitivity",
  "scenario-comparison",
  "manual-computation",
  "confidence-interval",
  "prediction-interval",
  "portfolio-comparison",
  "coefficient-reconstruction",
  "marginal-effect",
  "weight-sensitivity",
  "cost-saving",
  "power-comparison",
  "verification",
  "two-variable-change",
  "benchmark",
  "new-product",
  "stress-test",
  "reporting"
)

finance_tasks <- c(
  "point_prediction",
  "compare_weight",
  "manual_prediction",
  "weight_change",
  "horsepower_change",
  "manual_prediction",
  "confidence_interval",
  "prediction_interval",
  "compare_two_profiles",
  "manual_prediction",
  "horsepower_change",
  "weight_change",
  "weight_reduction",
  "horsepower_change",
  "verify_prediction",
  "combined_change",
  "mean_profile",
  "point_prediction",
  "compare_two_profiles",
  "confidence_interval"
)

finance_newdata <- list(
  data.frame(wt = 3.0, hp = 110),
  data.frame(wt = c(2.5, 3.5), hp = c(120, 120)),
  data.frame(wt = 2.8, hp = 150),
  data.frame(wt = c(3.4, 3.0), hp = c(140, 140)),
  data.frame(wt = c(3.2, 3.2), hp = c(100, 160)),
  data.frame(wt = 3.5, hp = 180),
  data.frame(wt = 2.6, hp = 95),
  data.frame(wt = 2.6, hp = 95),
  data.frame(wt = c(2.2, 3.8), hp = c(170, 120)),
  data.frame(wt = 4.0, hp = 175),
  data.frame(wt = c(3.0, 3.0), hp = c(120, 160)),
  data.frame(wt = c(2.5, 3.5), hp = c(130, 130)),
  data.frame(wt = c(3.4, 2.8), hp = c(150, 150)),
  data.frame(wt = c(3.0, 3.0), hp = c(120, 200)),
  data.frame(wt = 2.9, hp = 140),
  data.frame(wt = c(3.1, 3.4), hp = c(140, 120)),
  data.frame(wt = mean(mtcars$wt), hp = mean(mtcars$hp)),
  data.frame(wt = 1.9, hp = 90),
  data.frame(wt = c(5.0, 2.4), hp = c(250, 110)),
  data.frame(wt = 3.3, hp = 160)
)

build_finance_question <- function(i) {

  task_name <- finance_tasks[i]
  nd <- finance_newdata[[i]]

  if (task_name == "point_prediction") {

    pred <- predict_point(
      mtcars_model,
      nd
    )

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Using the fitted model mpg ~ wt + hp, calculate the predicted mpg."
    )

    reference_answer <- paste0(
      "Predicted mpg = ",
      fmt_num(pred),
      "."
    )

    solution_steps <- paste0(
      "1. Fit lm(mpg ~ wt + hp, data = mtcars). ",
      "2. Substitute wt = ",
      fmt_num(nd$wt),
      " and hp = ",
      fmt_num(nd$hp),
      " into the fitted equation. ",
      "3. The predicted mpg is ",
      fmt_num(pred),
      "."
    )

    predictor_value <- "wt, hp"
    answer_type <- "numeric"

  } else if (task_name == "manual_prediction") {

    pred <- predict_point(
      mtcars_model,
      nd
    )

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Use the fitted coefficients to calculate the prediction manually.",
      " Report the result to three decimal places."
    )

    reference_answer <- paste0(
      "Predicted mpg = ",
      fmt_num(pred),
      "."
    )

    solution_steps <- paste0(
      "1. The fitted equation is mpg = ",
      fmt_num(mt_coef[1]),
      " + (",
      fmt_num(mt_coef["wt"]),
      " × wt) + (",
      fmt_num(mt_coef["hp"]),
      " × hp). ",
      "2. Substitute wt = ",
      fmt_num(nd$wt),
      " and hp = ",
      fmt_num(nd$hp),
      ". ",
      "3. Predicted mpg = ",
      fmt_num(pred),
      "."
    )

    predictor_value <- "wt, hp"
    answer_type <- "numeric"

  } else if (
    task_name %in% c(
      "compare_weight",
      "weight_change",
      "weight_reduction",
      "horsepower_change",
      "combined_change",
      "compare_two_profiles"
    )
  ) {

    preds <- as.numeric(
      predict(
        mtcars_model,
        newdata = nd
      )
    )

    difference <- preds[2] - preds[1]

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Calculate the predicted mpg for both cases and report Case 2 minus Case 1."
    )

    reference_answer <- paste0(
      "Case 1 = ",
      fmt_num(preds[1]),
      " mpg; Case 2 = ",
      fmt_num(preds[2]),
      " mpg; difference = ",
      fmt_num(difference),
      " mpg."
    )

    solution_steps <- paste0(
      "1. Predict the first case to obtain ",
      fmt_num(preds[1]),
      " mpg. ",
      "2. Predict the second case to obtain ",
      fmt_num(preds[2]),
      " mpg. ",
      "3. Difference = ",
      fmt_num(preds[2]),
      " - ",
      fmt_num(preds[1]),
      " = ",
      fmt_num(difference),
      " mpg."
    )

    predictor_value <- "wt, hp"
    answer_type <- "numeric_and_comparison"

  } else if (task_name == "confidence_interval") {

    result <- predict_confidence(
      mtcars_model,
      nd
    )

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Use predict(..., interval = \"confidence\") and report the fitted value",
      " and the 95% confidence interval for the mean mpg."
    )

    reference_answer <- paste0(
      "Predicted mean mpg = ",
      fmt_num(result$fit),
      "; 95% CI = ",
      format_interval(
        result$lwr,
        result$upr
      ),
      "."
    )

    solution_steps <- paste0(
      "1. Create newdata with wt = ",
      fmt_num(nd$wt),
      " and hp = ",
      fmt_num(nd$hp),
      ". ",
      "2. Run predict(mtcars_model, newdata, interval = \"confidence\"). ",
      "3. The fitted value is ",
      fmt_num(result$fit),
      " and the interval is ",
      format_interval(result$lwr, result$upr),
      "."
    )

    predictor_value <- "wt, hp"
    answer_type <- "numeric_interval"

  } else if (task_name == "prediction_interval") {

    result <- predict_observation(
      mtcars_model,
      nd
    )

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Use predict(..., interval = \"prediction\") to obtain the point prediction",
      " and the 95% prediction interval for one future vehicle."
    )

    reference_answer <- paste0(
      "Predicted mpg = ",
      fmt_num(result$fit),
      "; 95% prediction interval = ",
      format_interval(
        result$lwr,
        result$upr
      ),
      "."
    )

    solution_steps <- paste0(
      "1. Use the specified wt and hp values in newdata. ",
      "2. Run predict(mtcars_model, newdata, interval = \"prediction\"). ",
      "3. The fitted value is ",
      fmt_num(result$fit),
      " and the prediction interval is ",
      format_interval(result$lwr, result$upr),
      "."
    )

    predictor_value <- "wt, hp"
    answer_type <- "numeric_interval"

  } else if (task_name == "verify_prediction") {

    manual_pred <- unname(
      mt_coef[1] +
        mt_coef["wt"] * nd$wt +
        mt_coef["hp"] * nd$hp
    )

    r_pred <- predict_point(
      mtcars_model,
      nd
    )

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Calculate the value both ways and confirm whether the two predictions agree."
    )

    reference_answer <- paste0(
      "Manual prediction = ",
      fmt_num(manual_pred),
      "; predict() result = ",
      fmt_num(r_pred),
      "; difference = ",
      fmt_num(r_pred - manual_pred),
      "."
    )

    solution_steps <- paste0(
      "1. Substitute the values into the fitted regression equation. ",
      "2. The manual result is ",
      fmt_num(manual_pred),
      ". ",
      "3. predict() returns ",
      fmt_num(r_pred),
      ". ",
      "4. Their difference is ",
      fmt_num(r_pred - manual_pred),
      ", so they agree apart from rounding."
    )

    predictor_value <- "wt, hp"
    answer_type <- "numeric_and_verification"

  } else if (task_name == "mean_profile") {

    pred <- predict_point(
      mtcars_model,
      nd
    )

    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Calculate mean(wt) and mean(hp), then use those values to obtain the fitted mpg."
    )

    reference_answer <- paste0(
      "mean(wt) = ",
      fmt_num(nd$wt),
      "; mean(hp) = ",
      fmt_num(nd$hp),
      "; predicted mpg = ",
      fmt_num(pred),
      "."
    )

    solution_steps <- paste0(
      "1. mean(mtcars$wt) = ",
      fmt_num(nd$wt),
      ". ",
      "2. mean(mtcars$hp) = ",
      fmt_num(nd$hp),
      ". ",
      "3. Substituting those values gives predicted mpg = ",
      fmt_num(pred),
      "."
    )

    predictor_value <- "wt, hp"
    answer_type <- "numeric"

  } else {

    stop(
      paste(
        "Unhandled finance task:",
        task_name
      )
    )
  }

  data.frame(
    id = sprintf(
      "R800_037_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_037",
    dataset_name = "mtcars",
    statistical_concept = "Prediction",
    task = "linear_model_prediction",
    template_id = paste0(
      "prediction_from_lm_",
      task_name
    ),
    difficulty = "medium",
    scenario = "finance",
    language_style = finance_styles[i],
    question_type = "calculation",
    predictor = predictor_value,
    response = "mpg",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_038
# CO2 + Environmental Science + Medium + Calculation
# ============================================================

co2_scenarios <- c(

  paste(
    "A chamber study is estimating carbon-dioxide uptake for a Quebec plant",
    "that was not chilled before measurement. The chamber concentration is",
    "set to 500 microlitres per litre."
  ),

  paste(
    "Two plants are exposed to the same concentration of 675.",
    "Both are from Quebec, but one received the chilled treatment",
    "and the other remained nonchilled."
  ),

  paste(
    "An environmental monitoring note asks for the expected uptake",
    "of a chilled Mississippi plant when conc = 350."
  ),

  paste(
    "To isolate the effect of concentration in the additive model,",
    "a Quebec nonchilled plant is evaluated first at conc = 300",
    "and then at conc = 700."
  ),

  paste(
    "At conc = 500, the research team compares Quebec with Mississippi",
    "while holding Treatment fixed at nonchilled."
  ),

  paste(
    "A treatment comparison is required for Mississippi plants",
    "at a common concentration of 500."
  ),

  paste(
    "The fitted model has been stored, but the predicted uptake for",
    "a Quebec chilled plant at conc = 900 is missing from the report."
  ),

  paste(
    "A field ecologist wants a 95% confidence interval for the mean uptake",
    "of Mississippi nonchilled plants at conc = 600."
  ),

  paste(
    "For one future Quebec chilled plant measured at conc = 600,",
    "the required uncertainty statement is a prediction interval."
  ),

  paste(
    "Two environmental scenarios are being compared:",
    "Scenario A is Quebec, nonchilled, conc = 400;",
    "Scenario B is Mississippi, chilled, conc = 800."
  ),

  paste(
    "The chamber concentration rises by 250 units while Type and Treatment",
    "remain fixed. The model-implied change in uptake must be calculated."
  ),

  paste(
    "At conc = 700 and under the chilled treatment, the expected uptake",
    "difference between Quebec and Mississippi is needed."
  ),

  paste(
    "The model coefficients are shown in an appendix.",
    "Using them directly, calculate uptake for a Mississippi nonchilled",
    "plant at conc = 1000."
  ),

  paste(
    "A validation check compares a manually calculated prediction",
    "with R's predict() result for Quebec, nonchilled, conc = 750."
  ),

  paste(
    "For the concluding table, report the predicted mean uptake and",
    "95% confidence interval for a Mississippi chilled plant at conc = 1000."
  )
)

co2_styles <- c(
  "chamber-study",
  "treatment-comparison",
  "monitoring-note",
  "concentration-sensitivity",
  "regional-comparison",
  "treatment-effect",
  "report-completion",
  "confidence-interval",
  "prediction-interval",
  "scenario-analysis",
  "marginal-effect",
  "regional-contrast",
  "coefficient-based",
  "verification",
  "final-report"
)

co2_tasks <- c(
  "point_prediction",
  "compare_treatment",
  "point_prediction",
  "concentration_change",
  "compare_type",
  "compare_treatment",
  "manual_prediction",
  "confidence_interval",
  "prediction_interval",
  "compare_profiles",
  "concentration_change",
  "compare_type",
  "manual_prediction",
  "verify_prediction",
  "confidence_interval"
)

co2_newdata <- list(
  data.frame(
    conc = 500,
    Type = factor("Quebec", levels = levels(CO2$Type)),
    Treatment = factor("nonchilled", levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = c(675, 675),
    Type = factor(c("Quebec", "Quebec"), levels = levels(CO2$Type)),
    Treatment = factor(c("nonchilled", "chilled"), levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = 350,
    Type = factor("Mississippi", levels = levels(CO2$Type)),
    Treatment = factor("chilled", levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = c(300, 700),
    Type = factor(c("Quebec", "Quebec"), levels = levels(CO2$Type)),
    Treatment = factor(c("nonchilled", "nonchilled"), levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = c(500, 500),
    Type = factor(c("Quebec", "Mississippi"), levels = levels(CO2$Type)),
    Treatment = factor(c("nonchilled", "nonchilled"), levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = c(500, 500),
    Type = factor(c("Mississippi", "Mississippi"), levels = levels(CO2$Type)),
    Treatment = factor(c("nonchilled", "chilled"), levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = 900,
    Type = factor("Quebec", levels = levels(CO2$Type)),
    Treatment = factor("chilled", levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = 600,
    Type = factor("Mississippi", levels = levels(CO2$Type)),
    Treatment = factor("nonchilled", levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = 600,
    Type = factor("Quebec", levels = levels(CO2$Type)),
    Treatment = factor("chilled", levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = c(400, 800),
    Type = factor(c("Quebec", "Mississippi"), levels = levels(CO2$Type)),
    Treatment = factor(c("nonchilled", "chilled"), levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = c(450, 700),
    Type = factor(c("Quebec", "Quebec"), levels = levels(CO2$Type)),
    Treatment = factor(c("nonchilled", "nonchilled"), levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = c(700, 700),
    Type = factor(c("Quebec", "Mississippi"), levels = levels(CO2$Type)),
    Treatment = factor(c("chilled", "chilled"), levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = 1000,
    Type = factor("Mississippi", levels = levels(CO2$Type)),
    Treatment = factor("nonchilled", levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = 750,
    Type = factor("Quebec", levels = levels(CO2$Type)),
    Treatment = factor("nonchilled", levels = levels(CO2$Treatment))
  ),
  data.frame(
    conc = 1000,
    Type = factor("Mississippi", levels = levels(CO2$Type)),
    Treatment = factor("chilled", levels = levels(CO2$Treatment))
  )
)

build_co2_question <- function(i) {

  task_name <- co2_tasks[i]
  nd <- co2_newdata[[i]]

  if (task_name == "point_prediction") {

    pred <- predict_point(
      co2_model,
      nd
    )

    question <- paste0(
      co2_scenarios[i],
      "\n\n",
      "Using the fitted model uptake ~ conc + Type + Treatment, calculate the predicted uptake."
    )

    reference_answer <- paste0(
      "Predicted uptake = ",
      fmt_num(pred),
      "."
    )

    solution_steps <- paste0(
      "1. Fit lm(uptake ~ conc + Type + Treatment, data = CO2). ",
      "2. Insert conc = ",
      fmt_num(nd$conc),
      ", Type = ",
      as.character(nd$Type),
      ", and Treatment = ",
      as.character(nd$Treatment),
      ". ",
      "3. The predicted uptake is ",
      fmt_num(pred),
      "."
    )

    predictor_value <- "conc, Type, Treatment"
    answer_type <- "numeric"

  } else if (
    task_name %in% c(
      "compare_treatment",
      "concentration_change",
      "compare_type",
      "compare_profiles"
    )
  ) {

    preds <- as.numeric(
      predict(
        co2_model,
        newdata = nd
      )
    )

    difference <- preds[2] - preds[1]

    question <- paste0(
      co2_scenarios[i],
      "\n\n",
      "Calculate the predicted uptake for both cases and report Case 2 minus Case 1."
    )

    reference_answer <- paste0(
      "Case 1 = ",
      fmt_num(preds[1]),
      "; Case 2 = ",
      fmt_num(preds[2]),
      "; difference = ",
      fmt_num(difference),
      "."
    )

    solution_steps <- paste0(
      "1. Predict Case 1 to obtain ",
      fmt_num(preds[1]),
      ". ",
      "2. Predict Case 2 to obtain ",
      fmt_num(preds[2]),
      ". ",
      "3. Difference = ",
      fmt_num(preds[2]),
      " - ",
      fmt_num(preds[1]),
      " = ",
      fmt_num(difference),
      "."
    )

    predictor_value <- "conc, Type, Treatment"
    answer_type <- "numeric_and_comparison"

  } else if (task_name == "manual_prediction") {

    pred <- predict_point(
      co2_model,
      nd
    )

    type_term <- if (
      as.character(nd$Type) == levels(CO2$Type)[1]
    ) {
      0
    } else {
      co2_coef[
        paste0(
          "Type",
          as.character(nd$Type)
        )
      ]
    }

    treatment_term <- if (
      as.character(nd$Treatment) == levels(CO2$Treatment)[1]
    ) {
      0
    } else {
      co2_coef[
        paste0(
          "Treatment",
          as.character(nd$Treatment)
        )
      ]
    }

    question <- paste0(
      co2_scenarios[i],
      "\n\n",
      "Use the fitted coefficients directly rather than relying only on predict()."
    )

    reference_answer <- paste0(
      "Predicted uptake = ",
      fmt_num(pred),
      "."
    )

    solution_steps <- paste0(
      "1. Begin with the intercept ",
      fmt_num(co2_coef[1]),
      ". ",
      "2. Add ",
      fmt_num(co2_coef["conc"]),
      " × ",
      fmt_num(nd$conc),
      " for concentration. ",
      "3. Add the Type adjustment ",
      fmt_num(type_term),
      " and Treatment adjustment ",
      fmt_num(treatment_term),
      ". ",
      "4. The resulting prediction is ",
      fmt_num(pred),
      "."
    )

    predictor_value <- "conc, Type, Treatment"
    answer_type <- "numeric"

  } else if (task_name == "confidence_interval") {

    result <- predict_confidence(
      co2_model,
      nd
    )

    question <- paste0(
      co2_scenarios[i],
      "\n\n",
      "Use predict(..., interval = \"confidence\") and report the fitted value",
      " with its 95% confidence interval for mean uptake."
    )

    reference_answer <- paste0(
      "Predicted mean uptake = ",
      fmt_num(result$fit),
      "; 95% CI = ",
      format_interval(
        result$lwr,
        result$upr
      ),
      "."
    )

    solution_steps <- paste0(
      "1. Construct newdata using the stated concentration, Type and Treatment. ",
      "2. Run predict(co2_model, newdata, interval = \"confidence\"). ",
      "3. The fitted value is ",
      fmt_num(result$fit),
      " and the interval is ",
      format_interval(result$lwr, result$upr),
      "."
    )

    predictor_value <- "conc, Type, Treatment"
    answer_type <- "numeric_interval"

  } else if (task_name == "prediction_interval") {

    result <- predict_observation(
      co2_model,
      nd
    )

    question <- paste0(
      co2_scenarios[i],
      "\n\n",
      "Use predict(..., interval = \"prediction\") and report the prediction",
      " with its 95% interval for one future observation."
    )

    reference_answer <- paste0(
      "Predicted uptake = ",
      fmt_num(result$fit),
      "; 95% prediction interval = ",
      format_interval(
        result$lwr,
        result$upr
      ),
      "."
    )

    solution_steps <- paste0(
      "1. Create the required newdata row. ",
      "2. Run predict(co2_model, newdata, interval = \"prediction\"). ",
      "3. The point prediction is ",
      fmt_num(result$fit),
      " and the interval is ",
      format_interval(result$lwr, result$upr),
      "."
    )

    predictor_value <- "conc, Type, Treatment"
    answer_type <- "numeric_interval"

  } else if (task_name == "verify_prediction") {

    manual_pred <- unname(
      co2_coef[1] +
        co2_coef["conc"] * nd$conc
    )

    if (
      as.character(nd$Type) != levels(CO2$Type)[1]
    ) {
      manual_pred <- manual_pred +
        co2_coef[
          paste0(
            "Type",
            as.character(nd$Type)
          )
        ]
    }

    if (
      as.character(nd$Treatment) != levels(CO2$Treatment)[1]
    ) {
      manual_pred <- manual_pred +
        co2_coef[
          paste0(
            "Treatment",
            as.character(nd$Treatment)
          )
        ]
    }

    r_pred <- predict_point(
      co2_model,
      nd
    )

    question <- paste0(
      co2_scenarios[i],
      "\n\n",
      "Calculate the value manually and compare it with predict()."
    )

    reference_answer <- paste0(
      "Manual prediction = ",
      fmt_num(manual_pred),
      "; predict() result = ",
      fmt_num(r_pred),
      "; difference = ",
      fmt_num(r_pred - manual_pred),
      "."
    )

    solution_steps <- paste0(
      "1. Evaluate the fitted equation using the stated factor levels. ",
      "2. The manual result is ",
      fmt_num(manual_pred),
      ". ",
      "3. predict() returns ",
      fmt_num(r_pred),
      ". ",
      "4. Their difference is ",
      fmt_num(r_pred - manual_pred),
      ", confirming agreement apart from rounding."
    )

    predictor_value <- "conc, Type, Treatment"
    answer_type <- "numeric_and_verification"

  } else {

    stop(
      paste(
        "Unhandled CO2 task:",
        task_name
      )
    )
  }

  data.frame(
    id = sprintf(
      "R800_038_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_038",
    dataset_name = "CO2",
    statistical_concept = "Prediction",
    task = "linear_model_prediction",
    template_id = paste0(
      "prediction_from_lm_",
      task_name
    ),
    difficulty = "medium",
    scenario = "environmental_science",
    language_style = co2_styles[i],
    question_type = "calculation",
    predictor = predictor_value,
    response = "uptake",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate questions
# ------------------------------------------------------------

finance_questions <- do.call(
  rbind,
  lapply(
    seq_len(20),
    build_finance_question
  )
)

co2_questions <- do.call(
  rbind,
  lapply(
    seq_len(15),
    build_co2_question
  )
)

prediction_questions <- rbind(
  finance_questions,
  co2_questions
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

prediction_questions <- prediction_questions[
  ,
  required_columns
]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(
    names(prediction_questions),
    required_columns
  )
)

stopifnot(
  nrow(prediction_questions) == 35
)

stopifnot(
  length(
    unique(prediction_questions$id)
  ) == 35
)

stopifnot(
  !anyDuplicated(
    prediction_questions$question
  )
)

stopifnot(
  sum(
    prediction_questions$blueprint_id ==
      "R800_037"
  ) == 20
)

stopifnot(
  sum(
    prediction_questions$blueprint_id ==
      "R800_038"
  ) == 15
)

stopifnot(
  all(
    prediction_questions$difficulty ==
      "medium"
  )
)

stopifnot(
  all(
    prediction_questions$question_type ==
      "calculation"
  )
)

stopifnot(
  all(
    nchar(
      prediction_questions$question
    ) >= 100
  )
)

stopifnot(
  all(
    nchar(
      prediction_questions$solution_steps
    ) >= 40
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
    prediction_questions$blueprint_id
  )
)

cat(
  "\nQuestion count by dataset:\n"
)

print(
  table(
    prediction_questions$dataset_name
  )
)

cat(
  "\nQuestion count by answer type:\n"
)

print(
  table(
    prediction_questions$answer_type
  )
)

preview_columns <- c(
  "id",
  "dataset_name",
  "difficulty",
  "scenario",
  "language_style",
  "predictor",
  "response",
  "template_id",
  "reference_answer"
)

print(
  prediction_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

cat(
  "\n\n================ R800_037 example ================\n\n"
)

cat(
  finance_questions$question[1],
  "\n\nReference answer:\n",
  finance_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  finance_questions$solution_steps[1],
  "\n"
)

cat(
  "\n\n================ R800_038 example ================\n\n"
)

cat(
  co2_questions$question[1],
  "\n\nReference answer:\n",
  co2_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  co2_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_037_R800_038_Prediction_v2.csv"
json_file <- "R800_037_R800_038_Prediction_v2.json"

write.csv(
  prediction_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write_json(
  prediction_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat(
  "\n\nSuccessfully generated ",
  nrow(prediction_questions),
  " prediction questions.\n",
  sep = ""
)

cat(
  "R800_037 finance questions: ",
  nrow(finance_questions),
  "\n",
  sep = ""
)

cat(
  "R800_038 environmental science questions: ",
  nrow(co2_questions),
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
