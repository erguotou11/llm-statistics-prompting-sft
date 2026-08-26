# ============================================================
# Prediction Question Generator
#
# R800_039
# Dataset: ChickWeight
# Domain: Agriculture
# Difficulty: Hard
# Question type: Interpretation
# Count: 10
#
# R800_040
# Dataset: mtcars
# Domain: Transportation
# Difficulty: Medium
# Question type: Short Answer
# Count: 15
#
# Outputs:
# 1. R800_039_R800_040_Prediction_v2.csv
# 2. R800_039_R800_040_Prediction_v2.json
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

format_interval <- function(lower, upper, digits = 3) {
  paste0(
    "[",
    fmt_num(lower, digits),
    ", ",
    fmt_num(upper, digits),
    "]"
  )
}

make_factor <- function(value, original_factor) {
  factor(
    value,
    levels = levels(original_factor),
    ordered = is.ordered(original_factor)
  )
}

# ------------------------------------------------------------
# Models
# ------------------------------------------------------------

# Population-average growth model with a Time-by-Diet interaction.
chick_population_model <- lm(
  weight ~ Time * Diet,
  data = ChickWeight
)

# Chick-adjusted fixed-effects model. This can predict only for chicks
# whose factor levels already occur in the fitted dataset.
chick_fixed_model <- lm(
  weight ~ Time * Diet + Chick,
  data = ChickWeight
)

transport_model <- lm(
  mpg ~ wt + hp,
  data = mtcars
)

chick_population_coef <- coef(
  chick_population_model
)

transport_coef <- coef(
  transport_model
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

# ============================================================
# R800_039
# ChickWeight + Agriculture + Hard + Interpretation
# ============================================================

agriculture_scenarios <- c(

  paste(
    "By day 14, the feeding trial has produced visibly different growth",
    "patterns across the four diets. Rather than comparing raw averages alone,",
    "the farm's research report uses weight ~ Time * Diet to generate predicted",
    "weights at a common age."
  ),

  paste(
    "Once the Time-by-Diet interaction is included, a single 'diet effect'",
    "no longer has the same meaning at every age. The fitted output is therefore",
    "being reviewed before any feeding recommendation is written."
  ),

  paste(
    "Suppose the hatchery asks for the expected weight of a chick on Diet 3",
    "at day 18, together with a 95% confidence interval for the mean response.",
    "The interval has already been computed from the fitted model."
  ),

  paste(
    "Between day 10 and day 18, the model predicts different gains for Diet 1",
    "and Diet 4. Management wants to know whether the larger predicted increase",
    "should be described as evidence of a stronger growth trajectory."
  ),

  paste(
    "Because each chick was weighed repeatedly, the rows in ChickWeight are not",
    "independent observations from different animals. A simple linear model still",
    "produces predictions, but the inferential wording requires care."
  ),

  paste(
    "For Chick 21, the fixed-effects model gives one predicted weight at day 12",
    "under its observed diet. A colleague then proposes using the same model to",
    "predict a brand-new chick labelled 99."
  ),

  paste(
    "At day 20, two diets receive similar point predictions but noticeably",
    "different uncertainty ranges. The report must distinguish uncertainty about",
    "a mean response from uncertainty about one future chick."
  ),

  paste(
    "Although the observed measurements stop at the trial's later time points,",
    "someone requests a prediction at day 30. The software returns a number,",
    "but the scientific interpretation is not automatic."
  ),

  paste(
    "One summary table compares predicted weights for Diet 2 and Diet 3 at day 16.",
    "The difference is calculated as Diet 3 minus Diet 2, and the team must explain",
    "what that contrast does and does not establish."
  ),

  paste(
    "For the final feeding-strategy note, the analyst combines a point prediction,",
    "the Time-by-Diet interaction and the repeated-measures structure into one",
    "balanced conclusion rather than reporting a single number without context."
  )
)

agriculture_styles <- c(
  "trial-summary",
  "interaction-interpretation",
  "confidence-interval",
  "growth-contrast",
  "design-limitation",
  "new-level-warning",
  "uncertainty-comparison",
  "extrapolation",
  "diet-contrast",
  "balanced-reporting"
)

agriculture_tasks <- c(
  "common_age_comparison",
  "interaction_meaning",
  "interpret_confidence_interval",
  "compare_growth_gain",
  "repeated_measurement_caution",
  "unseen_chick_level",
  "confidence_vs_prediction",
  "extrapolation_warning",
  "interpret_diet_contrast",
  "balanced_prediction_summary"
)

cw_newdata <- list(

  data.frame(
    Time = c(14, 14, 14, 14),
    Diet = make_factor(
      c(1, 2, 3, 4),
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = c(8, 20),
    Diet = make_factor(
      c(4, 4),
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = 18,
    Diet = make_factor(
      3,
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = c(10, 18, 10, 18),
    Diet = make_factor(
      c(1, 1, 4, 4),
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = 12,
    Diet = make_factor(
      2,
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = 12,
    Diet = make_factor(
      1,
      ChickWeight$Diet
    ),
    Chick = make_factor(
      21,
      ChickWeight$Chick
    )
  ),

  data.frame(
    Time = c(20, 20),
    Diet = make_factor(
      c(2, 3),
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = 30,
    Diet = make_factor(
      4,
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = c(16, 16),
    Diet = make_factor(
      c(2, 3),
      ChickWeight$Diet
    )
  ),

  data.frame(
    Time = 18,
    Diet = make_factor(
      4,
      ChickWeight$Diet
    )
  )
)

build_agriculture_question <- function(i) {

  task_name <- agriculture_tasks[i]
  nd <- cw_newdata[[i]]

  if (task_name == "common_age_comparison") {

    preds <- as.numeric(
      predict(
        chick_population_model,
        newdata = nd
      )
    )

    best_index <- which.max(
      preds
    )

    best_diet <- as.character(
      nd$Diet[best_index]
    )

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "At Time = 14, the fitted predictions are: Diet 1 = ",
      fmt_num(preds[1]),
      ", Diet 2 = ",
      fmt_num(preds[2]),
      ", Diet 3 = ",
      fmt_num(preds[3]),
      ", and Diet 4 = ",
      fmt_num(preds[4]),
      ". Interpret this comparison in context."
    )

    reference_answer <- paste0(
      "At the common age of 14 days, Diet ",
      best_diet,
      " has the largest model-predicted mean weight. ",
      "The ranking compares expected weights after holding Time fixed, but it does not by itself prove that the highest predicted diet is optimal in every flock or that every individual chick will follow the ranking."
    )

    solution_steps <- paste0(
      "1. Compare the four fitted values at the same Time. ",
      "2. Identify Diet ",
      best_diet,
      " as having the highest prediction. ",
      "3. Interpret the values as model-based mean predictions rather than guaranteed individual outcomes. ",
      "4. Avoid extending the result beyond the trial without considering uncertainty and design."
    )

  } else if (task_name == "interaction_meaning") {

    preds <- as.numeric(
      predict(
        chick_population_model,
        newdata = nd
      )
    )

    change <- preds[2] - preds[1]

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "For Diet 4, the predicted weight is ",
      fmt_num(preds[1]),
      " at day 8 and ",
      fmt_num(preds[2]),
      " at day 20. The predicted change is ",
      fmt_num(change),
      ". What does the interaction imply about interpreting Diet coefficients?"
    )

    reference_answer <- paste0(
      "The interaction means that the expected effect of Diet depends on Time, and the expected effect of Time depends on Diet. ",
      "Therefore, a Diet coefficient cannot be read as one constant difference that applies at every age. ",
      "The ",
      fmt_num(change),
      "-unit increase describes the fitted Diet 4 trajectory between days 8 and 20."
    )

    solution_steps <- paste0(
      "1. Note that the model contains Time * Diet. ",
      "2. This allows diet-specific slopes over Time. ",
      "3. Interpret the calculated change within Diet 4 and over the stated time interval. ",
      "4. Do not treat a main-effect coefficient as a universal diet difference."
    )

  } else if (task_name == "interpret_confidence_interval") {

    result <- predict_confidence(
      chick_population_model,
      nd
    )

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "The fitted mean is ",
      fmt_num(result$fit),
      " with a 95% confidence interval of ",
      format_interval(
        result$lwr,
        result$upr
      ),
      ". Explain what this interval represents."
    )

    reference_answer <- paste0(
      "The interval estimates the mean weight for chicks comparable to those in the trial on Diet 3 at day 18. ",
      "It reflects uncertainty in the estimated mean response, not the full range in which the weight of one future chick is expected to fall."
    )

    solution_steps <- paste0(
      "1. Identify the target as the conditional mean weight. ",
      "2. State the fitted value and interval. ",
      "3. Distinguish a confidence interval for the mean from a prediction interval for an individual chick. ",
      "4. Keep the interpretation tied to Diet 3 and day 18."
    )

  } else if (task_name == "compare_growth_gain") {

    preds <- as.numeric(
      predict(
        chick_population_model,
        newdata = nd
      )
    )

    diet1_gain <- preds[2] - preds[1]
    diet4_gain <- preds[4] - preds[3]
    gain_difference <- diet4_gain - diet1_gain

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "Predicted gain from day 10 to day 18 is ",
      fmt_num(diet1_gain),
      " for Diet 1 and ",
      fmt_num(diet4_gain),
      " for Diet 4. Diet 4 therefore exceeds Diet 1 by ",
      fmt_num(gain_difference),
      " units. Interpret this result."
    )

    reference_answer <- paste0(
      "Over the day-10-to-day-18 interval, the fitted model predicts a larger increase under Diet 4 than under Diet 1 by ",
      fmt_num(gain_difference),
      " weight units. ",
      "This supports a steeper predicted growth trajectory for Diet 4 over that interval, but uncertainty and the repeated-measures design should be considered before treating the difference as definitive."
    )

    solution_steps <- paste0(
      "1. Calculate the within-diet changes. ",
      "2. Compare the two predicted gains. ",
      "3. Interpret the difference as a model-based contrast in growth over a specific interval. ",
      "4. Avoid turning the point estimate into a certainty claim."
    )

  } else if (task_name == "repeated_measurement_caution") {

    pred <- predict_point(
      chick_population_model,
      nd
    )

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "For Diet 2 at day 12, the simple model predicts ",
      fmt_num(pred),
      ". Why should the prediction's inferential interpretation remain cautious?"
    )

    reference_answer <- paste0(
      "The point prediction is a valid output of the fitted equation, but repeated measurements from the same chick are correlated. ",
      "A simple lm model treats residual observations as independent, so its standard errors and intervals may be too optimistic if within-chick dependence is ignored. ",
      "A mixed-effects model with a chick-level random effect would better reflect the design."
    )

    solution_steps <- paste0(
      "1. Separate point prediction from uncertainty estimation. ",
      "2. Recognise that multiple rows belong to the same chick. ",
      "3. Explain why within-chick dependence violates the simple independence assumption. ",
      "4. Recommend a mixed-effects model for more appropriate uncertainty."
    )

  } else if (task_name == "unseen_chick_level") {

    pred <- predict_point(
      chick_fixed_model,
      nd
    )

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "For the existing Chick 21, the model predicts ",
      fmt_num(pred),
      ". Why can the same fixed-effects model not automatically produce a comparable prediction for a new Chick 99?"
    )

    reference_answer <- paste0(
      "The model contains Chick as a fixed factor and therefore estimates separate adjustments only for chick levels observed during fitting. ",
      "Chick 99 has no fitted coefficient, so it is an unseen factor level. ",
      "A population-level model or a mixed-effects model is more suitable when predictions are required for new chicks."
    )

    solution_steps <- paste0(
      "1. Note that Chick is included as a factor in the fixed-effects model. ",
      "2. Existing chick levels have estimated coefficients. ",
      "3. A new level has no corresponding estimate. ",
      "4. Explain why random-effects or population-average approaches generalise more naturally to new animals."
    )

  } else if (task_name == "confidence_vs_prediction") {

    ci_results <- predict_confidence(
      chick_population_model,
      nd
    )

    pi_results <- predict_observation(
      chick_population_model,
      nd
    )

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "For Diet 2 at day 20, the mean-response confidence interval is ",
      format_interval(
        ci_results$lwr[1],
        ci_results$upr[1]
      ),
      ", while the individual prediction interval is ",
      format_interval(
        pi_results$lwr[1],
        pi_results$upr[1]
      ),
      ". Why is the prediction interval wider?"
    )

    reference_answer <- paste0(
      "The confidence interval reflects uncertainty about the average weight at the stated Time and Diet. ",
      "The prediction interval must also allow for chick-to-chick residual variation, so it is wider. ",
      "The same distinction applies to Diet 3."
    )

    solution_steps <- paste0(
      "1. Identify the confidence interval's target as the conditional mean. ",
      "2. Identify the prediction interval's target as one future observation. ",
      "3. Explain that individual variability is added to estimation uncertainty. ",
      "4. Conclude that the prediction interval should be wider."
    )

  } else if (task_name == "extrapolation_warning") {

    pred <- predict_point(
      chick_population_model,
      nd
    )

    max_time <- max(
      ChickWeight$Time
    )

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "The model returns ",
      fmt_num(pred),
      " at day 30, whereas the largest observed Time is ",
      fmt_num(max_time, 0),
      ". How should this prediction be treated?"
    )

    reference_answer <- paste0(
      "The day-30 value is an extrapolation beyond the observed time range. ",
      "It relies on the fitted linear trend continuing after day ",
      fmt_num(max_time, 0),
      ", which the data do not verify. ",
      "The number may be useful for scenario exploration, but it should not be presented with the same confidence as an in-range prediction."
    )

    solution_steps <- paste0(
      "1. Compare the requested Time with the observed range. ",
      "2. Recognise that day 30 lies outside the data. ",
      "3. State the extra assumption of continued linear growth. ",
      "4. Qualify the result as extrapolative and potentially unreliable."
    )

  } else if (task_name == "interpret_diet_contrast") {

    preds <- as.numeric(
      predict(
        chick_population_model,
        newdata = nd
      )
    )

    contrast <- preds[2] - preds[1]

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "At day 16, predicted weight is ",
      fmt_num(preds[1]),
      " for Diet 2 and ",
      fmt_num(preds[2]),
      " for Diet 3. The contrast Diet 3 minus Diet 2 is ",
      fmt_num(contrast),
      ". Interpret this contrast."
    )

    reference_answer <- paste0(
      "At the common age of 16 days, the model predicts Diet 3 to exceed Diet 2 by ",
      fmt_num(contrast),
      " weight units on average. ",
      "The contrast is conditional on the fitted model and the chosen day; it is not a universal diet difference that applies at all times."
    )

    solution_steps <- paste0(
      "1. Hold Time fixed at 16. ",
      "2. Subtract the Diet 2 prediction from the Diet 3 prediction. ",
      "3. Interpret the result as a conditional mean difference. ",
      "4. Link the time-specific interpretation to the interaction model."
    )

  } else {

    pred <- predict_point(
      chick_population_model,
      nd
    )

    result <- predict_confidence(
      chick_population_model,
      nd
    )

    question <- paste0(
      agriculture_scenarios[i],
      "\n\n",
      "For Diet 4 at day 18, the fitted mean is ",
      fmt_num(pred),
      " with a 95% confidence interval of ",
      format_interval(
        result$lwr,
        result$upr
      ),
      ". Write a balanced interpretation."
    )

    reference_answer <- paste0(
      "The model predicts an average weight of ",
      fmt_num(pred),
      " for Diet 4 at day 18, with the stated interval describing uncertainty in that mean. ",
      "Because the model includes a Time-by-Diet interaction, the Diet 4 comparison is age-specific. ",
      "Because chicks are repeatedly measured, a mixed-effects analysis would be preferable for fully design-aware uncertainty."
    )

    solution_steps <- paste0(
      "1. Report the point prediction and confidence interval. ",
      "2. Interpret the estimate at the stated Diet and Time. ",
      "3. Explain that interaction effects are time-specific. ",
      "4. Add the repeated-measures limitation and avoid treating the estimate as an individual guarantee."
    )
  }

  data.frame(
    id = sprintf(
      "R800_039_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_039",
    dataset_name = "ChickWeight",
    statistical_concept = "Prediction",
    task = "prediction_interpretation",
    template_id = paste0(
      "prediction_from_lm_",
      task_name
    ),
    difficulty = "hard",
    scenario = "agriculture",
    language_style = agriculture_styles[i],
    question_type = "interpretation",
    predictor = "Time, Diet, Chick",
    response = "weight",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "written_interpretation",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_040
# mtcars + Transportation + Medium + Short Answer
# ============================================================

transport_scenarios <- c(

  paste(
    "Rush-hour fuel planning begins with a hypothetical vehicle weighing",
    "3.2 thousand pounds and producing 140 horsepower. The fitted mpg model",
    "returns a value that now needs a practical interpretation."
  ),

  paste(
    "On the route-planning dashboard, two vehicles share the same horsepower",
    "but differ by one thousand pounds in weight. Their predicted fuel economies",
    "are separated by several mpg."
  ),

  paste(
    "Suppose a fleet operator replaces a 180-horsepower model with a",
    "120-horsepower version at the same weight. The regression equation predicts",
    "an increase in mpg."
  ),

  paste(
    "Before approving a lightweight redesign, engineers calculate the predicted",
    "mpg change associated with reducing wt by 0.5 while holding hp fixed."
  ),

  paste(
    "One transport memo reports a 95% confidence interval for mean mpg;",
    "another reports a prediction interval for one future vehicle.",
    "The two ranges are not equally wide."
  ),

  paste(
    "At the edge of the specification table sits a very heavy, high-power vehicle.",
    "Although predict() still returns a value, the proposed profile lies near or",
    "beyond the combinations well represented in mtcars."
  ),

  paste(
    "A city fleet compares a light, high-power car with a heavier, lower-power",
    "alternative. The model favours one on predicted mpg, but the final purchase",
    "decision involves more than fuel economy."
  ),

  paste(
    "After reading the fitted equation, a planner says that the wt coefficient",
    "is the total effect of weight on mpg in every possible vehicle market.",
    "That statement needs qualification."
  ),

  paste(
    "For a standardised comparison, mpg is predicted at the sample-average",
    "values of wt and hp. The result is close to the observed average mpg."
  ),

  paste(
    "A maintenance unit observes a vehicle whose actual mpg is lower than",
    "the model prediction. The difference is labelled a residual and must be",
    "explained in operational terms."
  ),

  paste(
    "Two candidate specifications produce almost identical fitted mpg values.",
    "One has lower weight but higher horsepower, showing how predictors can",
    "offset one another inside an additive model."
  ),

  paste(
    "A transport analyst proposes adding an interaction between wt and hp.",
    "The current model contains only separate additive terms, so the question",
    "is what the existing prediction assumes."
  ),

  paste(
    "The fleet office wants to use the mtcars equation for a new generation",
    "of electric vehicles. The numerical calculation is easy; the transfer",
    "of the model to a different vehicle population is the difficult part."
  ),

  paste(
    "For one proposed vehicle, the mean-response confidence interval is narrow",
    "enough for planning, yet the prediction interval for an individual unit",
    "remains substantially wider."
  ),

  paste(
    "To close the transport review, a short written answer must combine",
    "the fitted mpg, the roles of wt and hp, the limits of causal language",
    "and one recommendation for responsible use."
  )
)

transport_styles <- c(
  "rush-hour-planning",
  "fleet-comparison",
  "engine-option",
  "redesign",
  "interval-comparison",
  "range-check",
  "procurement",
  "coefficient-qualification",
  "benchmark-profile",
  "residual-interpretation",
  "offsetting-effects",
  "model-form",
  "transferability",
  "uncertainty",
  "balanced-summary"
)

transport_tasks <- c(
  "interpret_point_prediction",
  "compare_weight_profiles",
  "interpret_hp_change",
  "interpret_weight_reduction",
  "confidence_vs_prediction",
  "extrapolation_and_range",
  "prediction_for_decision",
  "coefficient_caution",
  "mean_profile_interpretation",
  "residual_meaning",
  "offsetting_predictors",
  "additive_model_assumption",
  "external_validity",
  "interval_use",
  "balanced_model_use"
)

transport_newdata <- list(

  data.frame(
    wt = 3.2,
    hp = 140
  ),

  data.frame(
    wt = c(2.7, 3.7),
    hp = c(130, 130)
  ),

  data.frame(
    wt = c(3.3, 3.3),
    hp = c(180, 120)
  ),

  data.frame(
    wt = c(3.4, 2.9),
    hp = c(150, 150)
  ),

  data.frame(
    wt = 3.0,
    hp = 110
  ),

  data.frame(
    wt = 5.5,
    hp = 280
  ),

  data.frame(
    wt = c(2.3, 4.0),
    hp = c(190, 120)
  ),

  data.frame(
    wt = 3.5,
    hp = 160
  ),

  data.frame(
    wt = mean(mtcars$wt),
    hp = mean(mtcars$hp)
  ),

  data.frame(
    wt = 3.1,
    hp = 150
  ),

  data.frame(
    wt = c(2.8, 3.4),
    hp = c(190, 120)
  ),

  data.frame(
    wt = 3.0,
    hp = 180
  ),

  data.frame(
    wt = 2.2,
    hp = 250
  ),

  data.frame(
    wt = 3.6,
    hp = 160
  ),

  data.frame(
    wt = 3.3,
    hp = 150
  )
)

build_transport_question <- function(i) {

  task_name <- transport_tasks[i]
  nd <- transport_newdata[[i]]

  if (task_name == "interpret_point_prediction") {

    pred <- predict_point(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The model predicts ",
      fmt_num(pred),
      " mpg. Explain what this number means and what it does not guarantee."
    )

    reference_answer <- paste0(
      "The value ",
      fmt_num(pred),
      " mpg is the model's estimated mean fuel economy for vehicles with wt = 3.2 and hp = 140, assuming the fitted linear relationship applies. ",
      "It is not a guaranteed outcome for every individual vehicle and should not be read as a causal effect."
    )

    solution_steps <- paste0(
      "1. Identify the prediction as conditional on wt and hp. ",
      "2. Interpret it as an expected or fitted mean response. ",
      "3. Distinguish the estimate from an individual guarantee. ",
      "4. Avoid causal wording."
    )

  } else if (task_name == "compare_weight_profiles") {

    preds <- as.numeric(
      predict(
        transport_model,
        newdata = nd
      )
    )

    difference <- preds[2] - preds[1]

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The lighter vehicle is predicted at ",
      fmt_num(preds[1]),
      " mpg and the heavier one at ",
      fmt_num(preds[2]),
      " mpg. The heavier-minus-lighter difference is ",
      fmt_num(difference),
      ". Interpret this comparison."
    )

    reference_answer <- paste0(
      "Holding horsepower at 130, the heavier vehicle is predicted to have ",
      fmt_num(abs(difference)),
      " fewer mpg than the lighter vehicle. ",
      "The comparison reflects the fitted linear model and does not establish that changing weight alone would produce exactly the same change in every design."
    )

    solution_steps <- paste0(
      "1. Note that horsepower is held fixed. ",
      "2. Compare the two fitted values. ",
      "3. Translate the negative difference into fewer predicted mpg for the heavier vehicle. ",
      "4. Qualify the result as model-based rather than deterministic."
    )

  } else if (task_name == "interpret_hp_change") {

    preds <- as.numeric(
      predict(
        transport_model,
        newdata = nd
      )
    )

    improvement <- preds[2] - preds[1]

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "Predicted mpg rises from ",
      fmt_num(preds[1]),
      " to ",
      fmt_num(preds[2]),
      ", a change of ",
      fmt_num(improvement),
      ". Explain the result."
    )

    reference_answer <- paste0(
      "At wt = 3.3, reducing horsepower from 180 to 120 is associated with a model-predicted increase of ",
      fmt_num(improvement),
      " mpg. ",
      "This is a conditional prediction from an observational model and should not be presented as a guaranteed engineering effect."
    )

    solution_steps <- paste0(
      "1. Hold wt fixed. ",
      "2. Compare the predictions at the two horsepower values. ",
      "3. Interpret the difference in mpg. ",
      "4. Separate predictive association from causation."
    )

  } else if (task_name == "interpret_weight_reduction") {

    preds <- as.numeric(
      predict(
        transport_model,
        newdata = nd
      )
    )

    gain <- preds[2] - preds[1]

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The model predicts an mpg increase of ",
      fmt_num(gain),
      ". Provide a justified interpretation."
    )

    reference_answer <- paste0(
      "With hp fixed at 150, lowering wt from 3.4 to 2.9 is associated with an increase of ",
      fmt_num(gain),
      " predicted mpg. ",
      "The estimate is useful for sensitivity analysis, but redesign decisions should also consider whether the linear model remains valid and whether other vehicle characteristics change."
    )

    solution_steps <- paste0(
      "1. Verify that hp is unchanged. ",
      "2. Calculate the prediction difference. ",
      "3. Interpret it as a conditional model-based change. ",
      "4. Add a limitation involving model validity or omitted changes."
    )

  } else if (task_name == "confidence_vs_prediction") {

    ci <- predict_confidence(
      transport_model,
      nd
    )

    pi <- predict_observation(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The 95% confidence interval is ",
      format_interval(
        ci$lwr,
        ci$upr
      ),
      ", while the 95% prediction interval is ",
      format_interval(
        pi$lwr,
        pi$upr
      ),
      ". Explain the difference."
    )

    reference_answer <- paste0(
      "The confidence interval concerns the mean mpg for vehicles with the specified wt and hp. ",
      "The prediction interval concerns one future vehicle and therefore includes both uncertainty in the mean and vehicle-to-vehicle residual variation. ",
      "That is why the prediction interval is wider."
    )

    solution_steps <- paste0(
      "1. Identify the target of each interval. ",
      "2. Explain the extra individual variability in a prediction interval. ",
      "3. Link the wider range to greater uncertainty for a single future observation."
    )

  } else if (task_name == "extrapolation_and_range") {

    pred <- predict_point(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The fitted value is ",
      fmt_num(pred),
      " mpg. Why should this number be treated cautiously?"
    )

    reference_answer <- paste0(
      "The requested wt-hp combination is poorly represented by the original sample and may involve extrapolation in one or both predictors. ",
      "The model assumes the same additive linear relationship continues into that region, so the numerical output may be unstable or unrealistic."
    )

    solution_steps <- paste0(
      "1. Compare the requested profile with the observed predictor ranges and combinations. ",
      "2. Recognise that predict() can return a number even outside well-supported regions. ",
      "3. Explain the unverified linearity assumption. ",
      "4. Recommend cautious use or additional data."
    )

  } else if (task_name == "prediction_for_decision") {

    preds <- as.numeric(
      predict(
        transport_model,
        newdata = nd
      )
    )

    better_index <- which.max(
      preds
    )

    better_label <- if (
      better_index == 1
    ) {
      "the light, high-power vehicle"
    } else {
      "the heavier, lower-power vehicle"
    }

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The predicted mpg values are ",
      fmt_num(preds[1]),
      " and ",
      fmt_num(preds[2]),
      ". How should the fleet interpret this comparison?"
    )

    reference_answer <- paste0(
      "On predicted fuel economy alone, ",
      better_label,
      " is preferred. ",
      "However, procurement should also consider purchase price, maintenance, capacity, safety, route needs and uncertainty in the model prediction."
    )

    solution_steps <- paste0(
      "1. Compare the two fitted mpg values. ",
      "2. Identify the higher predicted value. ",
      "3. Restrict the conclusion to fuel economy. ",
      "4. Add operational and financial criteria needed for a real decision."
    )

  } else if (task_name == "coefficient_caution") {

    pred <- predict_point(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "At wt = 3.5 and hp = 160, the fitted mpg is ",
      fmt_num(pred),
      ". Why is the planner's interpretation of the wt coefficient too strong?"
    )

    reference_answer <- paste0(
      "The wt coefficient describes the expected difference in mpg associated with a one-unit change in wt while hp is held fixed within this fitted sample. ",
      "It is not automatically a causal, universal or market-invariant effect because the data are observational and other relevant vehicle characteristics are omitted."
    )

    solution_steps <- paste0(
      "1. State the ceteris-paribus interpretation of the coefficient. ",
      "2. Note that it is conditional on hp and the fitted model. ",
      "3. Explain the observational and omitted-variable limitations. ",
      "4. Reject universal causal wording."
    )

  } else if (task_name == "mean_profile_interpretation") {

    pred <- predict_point(
      transport_model,
      nd
    )

    observed_mean <- mean(
      mtcars$mpg
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The fitted mpg at mean(wt) and mean(hp) is ",
      fmt_num(pred),
      ", while mean(mpg) is ",
      fmt_num(observed_mean),
      ". Explain why these values are close."
    )

    reference_answer <- paste0(
      "With an intercept in an ordinary least-squares model, the fitted regression surface passes through the point formed by the sample means. ",
      "Therefore, the prediction at mean wt and mean hp equals or is numerically very close to mean mpg, apart from rounding."
    )

    solution_steps <- paste0(
      "1. Recall the OLS mean property when an intercept is included. ",
      "2. Evaluate the model at the predictor means. ",
      "3. Compare the fitted value with the observed response mean. ",
      "4. Attribute any tiny discrepancy to rounding."
    )

  } else if (task_name == "residual_meaning") {

    pred <- predict_point(
      transport_model,
      nd
    )

    actual_mpg <- 16.0
    residual <- actual_mpg - pred

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "Suppose actual mpg is ",
      fmt_num(actual_mpg),
      " and predicted mpg is ",
      fmt_num(pred),
      ". The residual is ",
      fmt_num(residual),
      ". Interpret its sign and size."
    )

    reference_answer <- paste0(
      "The residual is actual minus predicted, so the negative value means the vehicle achieved ",
      fmt_num(abs(residual)),
      " fewer mpg than the model expected. ",
      "The gap may reflect ordinary unexplained variation, omitted predictors, measurement issues or an unusual vehicle."
    )

    solution_steps <- paste0(
      "1. Calculate actual - predicted. ",
      "2. Use the negative sign to identify underperformance relative to the model. ",
      "3. Translate the magnitude into mpg. ",
      "4. Offer plausible non-causal explanations for the discrepancy."
    )

  } else if (task_name == "offsetting_predictors") {

    preds <- as.numeric(
      predict(
        transport_model,
        newdata = nd
      )
    )

    difference <- preds[2] - preds[1]

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The two predictions are ",
      fmt_num(preds[1]),
      " and ",
      fmt_num(preds[2]),
      ", differing by ",
      fmt_num(difference),
      ". Explain how the predictors offset one another."
    )

    reference_answer <- paste0(
      "The second vehicle's greater weight lowers predicted mpg, while its lower horsepower raises predicted mpg relative to the first vehicle. ",
      "Because the model is additive, these contributions are summed, and the opposing effects produce similar fitted values."
    )

    solution_steps <- paste0(
      "1. Identify the direction of the wt contribution. ",
      "2. Identify the direction of the hp contribution. ",
      "3. Explain that additive terms are combined. ",
      "4. Show how opposing predictor changes can yield similar predictions."
    )

  } else if (task_name == "additive_model_assumption") {

    pred <- predict_point(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "For wt = 3.0 and hp = 180, predicted mpg is ",
      fmt_num(pred),
      ". What assumption does the current additive model make about wt and hp?"
    )

    reference_answer <- paste0(
      "The model assumes that the wt slope is the same at every hp value and the hp slope is the same at every wt value. ",
      "Without an interaction term, the effect of one predictor does not depend on the level of the other."
    )

    solution_steps <- paste0(
      "1. Inspect the model formula mpg ~ wt + hp. ",
      "2. Note the absence of wt:hp. ",
      "3. State the constant-slope additive assumption. ",
      "4. Explain what an interaction would allow."
    )

  } else if (task_name == "external_validity") {

    pred <- predict_point(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The equation predicts ",
      fmt_num(pred),
      " mpg for the proposed profile. Why may this not transfer reliably to electric vehicles?"
    )

    reference_answer <- paste0(
      "The model was fitted to the vehicle population represented by mtcars, with fuel-economy relationships shaped by conventional vehicle technology. ",
      "Electric vehicles may follow different mechanisms, predictor ranges and efficiency measures, so applying the equation without validation is a domain shift."
    )

    solution_steps <- paste0(
      "1. Identify the source population of the fitted model. ",
      "2. Compare it with the target vehicle population. ",
      "3. Explain domain shift and changed mechanisms. ",
      "4. Recommend refitting or validating on relevant electric-vehicle data."
    )

  } else if (task_name == "interval_use") {

    ci <- predict_confidence(
      transport_model,
      nd
    )

    pi <- predict_observation(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "The mean-response interval is ",
      format_interval(
        ci$lwr,
        ci$upr
      ),
      " and the individual prediction interval is ",
      format_interval(
        pi$lwr,
        pi$upr
      ),
      ". Which interval should be used for fleet-average planning and which for one specific vehicle?"
    )

    reference_answer <- paste0(
      "The confidence interval is appropriate for uncertainty about the mean mpg of comparable vehicles. ",
      "The prediction interval is appropriate for one specific future vehicle because it includes individual residual variation."
    )

    solution_steps <- paste0(
      "1. Match the confidence interval to the conditional mean. ",
      "2. Match the prediction interval to one future observation. ",
      "3. Explain why the latter is wider and more suitable for an individual unit."
    )

  } else {

    pred <- predict_point(
      transport_model,
      nd
    )

    ci <- predict_confidence(
      transport_model,
      nd
    )

    question <- paste0(
      transport_scenarios[i],
      "\n\n",
      "For wt = 3.3 and hp = 150, predicted mpg is ",
      fmt_num(pred),
      " with a 95% confidence interval of ",
      format_interval(
        ci$lwr,
        ci$upr
      ),
      ". Write the requested balanced short answer."
    )

    reference_answer <- paste0(
      "The model predicts mean fuel economy of ",
      fmt_num(pred),
      " mpg for vehicles with the stated weight and horsepower. ",
      "Greater wt and hp are each associated with lower mpg after holding the other variable fixed. ",
      "The result is predictive rather than automatically causal, and it should be used within the range and vehicle population represented by the data. ",
      "Validation on newer, decision-relevant fleet data would strengthen its use."
    )

    solution_steps <- paste0(
      "1. Report and interpret the fitted value and interval. ",
      "2. Explain the conditional roles of wt and hp. ",
      "3. Add causal and generalisability limitations. ",
      "4. Recommend validation using relevant transport data."
    )
  }

  data.frame(
    id = sprintf(
      "R800_040_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_040",
    dataset_name = "mtcars",
    statistical_concept = "Prediction",
    task = "prediction_reasoning",
    template_id = paste0(
      "prediction_from_lm_",
      task_name
    ),
    difficulty = "medium",
    scenario = "transportation",
    language_style = transport_styles[i],
    question_type = "short_answer",
    predictor = "wt, hp",
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

agriculture_questions <- do.call(
  rbind,
  lapply(
    seq_len(10),
    build_agriculture_question
  )
)

transport_questions <- do.call(
  rbind,
  lapply(
    seq_len(15),
    build_transport_question
  )
)

prediction_questions <- rbind(
  agriculture_questions,
  transport_questions
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
  nrow(prediction_questions) == 25
)

stopifnot(
  length(
    unique(prediction_questions$id)
  ) == 25
)

stopifnot(
  !anyDuplicated(
    prediction_questions$question
  )
)

stopifnot(
  sum(
    prediction_questions$blueprint_id ==
      "R800_039"
  ) == 10
)

stopifnot(
  sum(
    prediction_questions$blueprint_id ==
      "R800_040"
  ) == 15
)

stopifnot(
  all(
    agriculture_questions$difficulty ==
      "hard"
  )
)

stopifnot(
  all(
    agriculture_questions$question_type ==
      "interpretation"
  )
)

stopifnot(
  all(
    transport_questions$difficulty ==
      "medium"
  )
)

stopifnot(
  all(
    transport_questions$question_type ==
      "short_answer"
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
      prediction_questions$reference_answer
    ) >= 80
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
  "\nQuestion count by question type:\n"
)

print(
  table(
    prediction_questions$question_type
  )
)

cat(
  "\nQuestion count by difficulty:\n"
)

print(
  table(
    prediction_questions$difficulty
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
  "template_id"
)

print(
  prediction_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

cat(
  "\n\n================ R800_039 example ================\n\n"
)

cat(
  agriculture_questions$question[1],
  "\n\nReference answer:\n",
  agriculture_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  agriculture_questions$solution_steps[1],
  "\n"
)

cat(
  "\n\n================ R800_040 example ================\n\n"
)

cat(
  transport_questions$question[1],
  "\n\nReference answer:\n",
  transport_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  transport_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_039_R800_040_Prediction_v2.csv"
json_file <- "R800_039_R800_040_Prediction_v2.json"

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
  "R800_039 agriculture interpretation questions: ",
  nrow(agriculture_questions),
  "\n",
  sep = ""
)

cat(
  "R800_040 transportation short-answer questions: ",
  nrow(transport_questions),
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
