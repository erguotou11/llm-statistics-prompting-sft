# ============================================================
# Correlation Question Generator
#
# R800_031
# Dataset: mtcars
# Domain: Finance
# Difficulty: Medium
# Question type: Calculation
# Count: 20
#
# R800_033
# Dataset: mtcars
# Domain: Healthcare
# Difficulty: Easy
# Question type: Calculation
# Count: 15
#
# Output:
# 1. R800_031_R800_033_Correlation_v2.csv
# 2. R800_031_R800_033_Correlation_v2.json
# ============================================================

set.seed(20260711)

# ------------------------------------------------------------
# Package
# ------------------------------------------------------------

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
  
  abs_r <- abs(r)
  
  if (abs_r < 0.20) {
    "very weak"
  } else if (abs_r < 0.40) {
    "weak"
  } else if (abs_r < 0.60) {
    "moderate"
  } else if (abs_r < 0.80) {
    "strong"
  } else {
    "very strong"
  }
}

# ------------------------------------------------------------
# Correlation calculation
# ------------------------------------------------------------

calculate_correlation <- function(
    data,
    x,
    y
) {
  
  x_values <- data[[x]]
  y_values <- data[[y]]
  
  complete_rows <- complete.cases(
    x_values,
    y_values
  )
  
  x_values <- x_values[
    complete_rows
  ]
  
  y_values <- y_values[
    complete_rows
  ]
  
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
  
  t_value <- unname(
    test_result$statistic
  )
  
  p_value <- test_result$p.value
  
  ci_lower <- unname(
    test_result$conf.int[1]
  )
  
  ci_upper <- unname(
    test_result$conf.int[2]
  )
  
  covariance_value <- cov(
    x_values,
    y_values
  )
  
  sd_x <- sd(
    x_values
  )
  
  sd_y <- sd(
    y_values
  )
  
  r_squared <- r_value^2
  
  list(
    x = x,
    y = y,
    n = n_value,
    df = df_value,
    r = r_value,
    r_squared = r_squared,
    t_value = t_value,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    covariance = covariance_value,
    sd_x = sd_x,
    sd_y = sd_y,
    direction = direction_label(r_value),
    strength = strength_label(r_value)
  )
}

# ------------------------------------------------------------
# Calculate all variable-pair statistics
# ------------------------------------------------------------

cor_mpg_wt <- calculate_correlation(
  data = mtcars,
  x = "mpg",
  y = "wt"
)

cor_mpg_hp <- calculate_correlation(
  data = mtcars,
  x = "mpg",
  y = "hp"
)

cor_mpg_disp <- calculate_correlation(
  data = mtcars,
  x = "mpg",
  y = "disp"
)

cor_wt_hp <- calculate_correlation(
  data = mtcars,
  x = "wt",
  y = "hp"
)

cor_wt_disp <- calculate_correlation(
  data = mtcars,
  x = "wt",
  y = "disp"
)

cor_hp_disp <- calculate_correlation(
  data = mtcars,
  x = "hp",
  y = "disp"
)

correlation_lookup <- list(
  mpg_wt = cor_mpg_wt,
  mpg_hp = cor_mpg_hp,
  mpg_disp = cor_mpg_disp,
  wt_hp = cor_wt_hp,
  wt_disp = cor_wt_disp,
  hp_disp = cor_hp_disp
)

# ------------------------------------------------------------
# Diverse instruction banks
# ------------------------------------------------------------

basic_r_phrases <- c(
  "Calculate Pearson's r for %s and %s.",
  "What is the sample correlation between %s and %s?",
  "Quantify the linear association linking %s with %s.",
  "Using all 32 vehicles, obtain cor(%s, %s).",
  "Find the Pearson product-moment correlation for %s versus %s.",
  "How strongly are %s and %s linearly related in this sample?",
  "Derive the coefficient summarising the linear pattern between %s and %s.",
  "Report the value of r obtained from %s and %s.",
  "Use the observed data to measure the linear co-movement of %s and %s.",
  "Compute the coefficient describing the relationship between %s and %s."
)

significance_phrases <- c(
  "Test whether the population correlation differs from zero.",
  "Assess the evidence against H0: rho = 0.",
  "Determine whether the observed association is statistically distinguishable from zero.",
  "Carry out the two-sided significance test for the population correlation.",
  "Evaluate whether sampling variation alone could plausibly explain the observed r."
)

comparison_phrases <- c(
  "Which relationship is stronger in absolute terms?",
  "Compare the magnitudes of the two coefficients.",
  "Identify the variable showing the closer linear connection.",
  "Which pair exhibits the greater degree of linear co-movement?",
  "Rank the two relationships by the value of |r|."
)

r_squared_phrases <- c(
  "Square the coefficient and express the result as a percentage.",
  "Convert the correlation into shared linear variation.",
  "Calculate r-squared after obtaining r.",
  "What percentage is represented by the squared correlation?",
  "Report both the coefficient and its squared value."
)

# ============================================================
# R800_031
# Finance + Medium + Calculation
# ============================================================

finance_scenarios <- c(
  
  paste(
    "Fuel prices have risen sharply, and the investment committee is",
    "reassessing manufacturers whose product ranges are concentrated",
    "in heavier vehicles. The mtcars sample is being used to examine",
    "the link between vehicle weight and fuel efficiency."
  ),
  
  paste(
    "In a draft equity-research note, one sentence claims that more",
    "powerful cars generally deliver poorer fuel economy. Before the",
    "note is circulated, that claim must be backed by a numerical result."
  ),
  
  paste(
    "Future emissions rules could place greater compliance costs on firms",
    "selling vehicles with large engines. The relationship between engine",
    "displacement and mileage therefore matters to the valuation case."
  ),
  
  paste(
    "Two inputs in a vehicle-finance risk model appear to move together:",
    "heavier cars also seem to have greater horsepower. The modelling",
    "team needs to know how closely the two measures are related."
  ),
  
  paste(
    "A scatterplot of vehicle weight against engine displacement shows",
    "a pronounced upward pattern, but the strength of that pattern has",
    "not yet been reported numerically."
  ),
  
  paste(
    "During a review of automobile-sector indicators, the committee asks:",
    "\"Are horsepower and engine displacement providing largely the same",
    "information?\" The first step is a sample correlation."
  ),
  
  paste(
    "Rather than writing that heavier vehicles are simply 'less economical',",
    "the sustainability section must attach a number to the relationship",
    "between mpg and vehicle weight."
  ),
  
  paste(
    "The sample suggests that fuel economy falls as horsepower rises.",
    "The unresolved issue is whether this pattern is strong enough to",
    "be distinguished from zero in the wider population."
  ),
  
  paste(
    "A preliminary report contains only a point estimate for the association",
    "between mileage and engine displacement. The final report must also",
    "show the uncertainty around that estimate."
  ),
  
  paste(
    "Weight and engine displacement are both being considered as inputs",
    "to a vehicle-cost model. Before retaining both, the analyst wants",
    "to know how much linear variation they share."
  ),
  
  paste(
    "The phrase 'higher horsepower is linked to lower mileage' appears",
    "throughout an investment presentation. The formal test statistic",
    "supporting that statement is missing from the appendix."
  ),
  
  paste(
    "As part of an audit, the published mpg-weight correlation must be",
    "reproduced without calling cor() directly. The covariance and both",
    "sample standard deviations are available."
  ),
  
  paste(
    "Both horsepower and vehicle weight have been proposed as simple",
    "indicators of poor fuel economy. Only one will be retained in the",
    "first version of the screening model."
  ),
  
  paste(
    "Before finalising a valuation model, the quantitative team compares",
    "two relationships involving engine displacement: one with vehicle",
    "weight and the other with horsepower."
  ),
  
  paste(
    "Suppose a forecasting model rests on the assumption that heavier cars",
    "consume more fuel. The mtcars data can be used to quantify the sample",
    "evidence behind that assumption."
  ),
  
  paste(
    "The ESG section of an automotive report must translate the mpg-displacement",
    "relationship into a percentage rather than leaving it as a raw coefficient."
  ),
  
  paste(
    "Horsepower and weight are both candidates for inclusion in a forecasting",
    "equation. Their pairwise association is checked first because strongly",
    "related predictors may contribute overlapping information."
  ),
  
  paste(
    "Engine displacement is already included in a valuation model. Adding",
    "horsepower may help only if it contributes sufficiently distinct information."
  ),
  
  paste(
    "The data review has produced r for mpg and wt, but the significance-test",
    "calculation is absent from the working paper. It must be reconstructed",
    "from the sample size and the correlation coefficient."
  ),
  
  paste(
    "For the closing paragraph of the sector report, the mpg-horsepower result",
    "must be condensed into a defensible statement covering direction, strength",
    "and statistical evidence."
  )
)

finance_styles <- c(
  "decision-background",
  "research-note",
  "regulatory",
  "model-development",
  "graph-led",
  "committee-question",
  "reporting",
  "hypothesis-testing",
  "uncertainty-focused",
  "variance-focused",
  "audit",
  "formula-based",
  "comparative",
  "model-comparison",
  "assumption-check",
  "ESG-reporting",
  "forecasting",
  "multicollinearity",
  "verification",
  "executive-summary"
)

finance_tasks <- c(
  "calculate_r",
  "calculate_r",
  "calculate_r",
  "calculate_r",
  "calculate_r",
  "calculate_r",
  "direction_strength",
  "test_significance",
  "confidence_interval",
  "calculate_r_squared",
  "calculate_t",
  "manual_r",
  "compare_mpg_predictors",
  "compare_disp_relationships",
  "interpret_negative",
  "calculate_r_squared",
  "multicollinearity_screen",
  "multicollinearity_screen",
  "manual_t",
  "full_summary"
)

finance_pairs <- c(
  "mpg_wt",
  "mpg_hp",
  "mpg_disp",
  "wt_hp",
  "wt_disp",
  "hp_disp",
  "mpg_wt",
  "mpg_hp",
  "mpg_disp",
  "wt_disp",
  "mpg_hp",
  "mpg_wt",
  "mpg_wt",
  "wt_disp",
  "mpg_wt",
  "mpg_disp",
  "wt_hp",
  "hp_disp",
  "mpg_wt",
  "mpg_hp"
)

build_finance_question <- function(i) {
  
  stats <- correlation_lookup[[finance_pairs[i]]]
  
  task_name <- finance_tasks[i]
  
  x <- stats$x
  y <- stats$y
  
  if (task_name == "calculate_r") {
    
    instruction <- sprintf(
      basic_r_phrases[
        ((i - 1) %% length(basic_r_phrases)) + 1
      ],
      x,
      y
    )
    
    endings <- c(
      "Round the coefficient to three decimal places and identify its direction.",
      "Give the result to three decimal places, then say whether the pattern is positive or negative.",
      "Report r to three decimal places and briefly indicate how the variables move together.",
      "Provide the signed coefficient to three decimal places.",
      "State both the numerical value and the direction of the relationship.",
      "Use Pearson's method and retain three decimal places."
    )
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      instruction,
      " ",
      endings[((i - 1) %% length(endings)) + 1]
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; ",
      stats$direction,
      " correlation."
    )
    
    solution_steps <- paste0(
      "1. Use the complete ",
      x,
      " and ",
      y,
      " columns from mtcars. ",
      "2. Compute cor(mtcars$",
      x,
      ", mtcars$",
      y,
      ", method = \"pearson\"). ",
      "3. The resulting coefficient is ",
      fmt_num(stats$r),
      ". ",
      "4. Its sign indicates a ",
      stats$direction,
      " relationship."
    )
    
    answer_type <- "numeric"
    
  } else if (task_name == "direction_strength") {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Obtain Pearson's r for ",
      x,
      " and ",
      y,
      ". Then use |r| to classify the relationship as very weak, weak, ",
      "moderate, strong or very strong."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; ",
      stats$strength,
      " ",
      stats$direction,
      " linear association."
    )
    
    solution_steps <- paste0(
      "1. Calculate r = ",
      fmt_num(stats$r),
      ". ",
      "2. The sign is ",
      stats$direction,
      ". ",
      "3. The absolute value is ",
      fmt_num(abs(stats$r)),
      ". ",
      "4. Under the stated rule, this is a ",
      stats$strength,
      " association."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "test_significance") {
    
    test_instruction <- significance_phrases[
      ((i - 1) %% length(significance_phrases)) + 1
    ]
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      test_instruction,
      " Use a two-sided Pearson correlation test for ",
      x,
      " and ",
      y,
      ", and report r, t, the degrees of freedom and the p-value."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; t(",
      stats$df,
      ") = ",
      fmt_num(stats$t_value),
      "; p ",
      fmt_p(stats$p_value),
      "; reject H0."
    )
    
    solution_steps <- paste0(
      "1. The sample correlation is r = ",
      fmt_num(stats$r),
      ". ",
      "2. There are ",
      stats$n,
      " complete observations, so df = ",
      stats$n,
      " - 2 = ",
      stats$df,
      ". ",
      "3. cor.test() gives t = ",
      fmt_num(stats$t_value),
      " and p = ",
      fmt_num(stats$p_value, 6),
      ". ",
      "4. Since p < 0.05, reject H0: rho = 0."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "confidence_interval") {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Run a Pearson cor.test() for ",
      x,
      " and ",
      y,
      ". Report the sample coefficient together with the 95% confidence interval ",
      "for the population correlation."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; 95% CI = ",
      format_ci(
        stats$ci_lower,
        stats$ci_upper
      ),
      "."
    )
    
    solution_steps <- paste0(
      "1. Run cor.test(mtcars$",
      x,
      ", mtcars$",
      y,
      ", method = \"pearson\"). ",
      "2. The point estimate is r = ",
      fmt_num(stats$r),
      ". ",
      "3. The 95% confidence interval is ",
      format_ci(
        stats$ci_lower,
        stats$ci_upper
      ),
      "."
    )
    
    answer_type <- "numeric"
    
  } else if (task_name == "calculate_r_squared") {
    
    instruction <- r_squared_phrases[
      ((i - 1) %% length(r_squared_phrases)) + 1
    ]
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "First obtain Pearson's r for ",
      x,
      " and ",
      y,
      ". ",
      instruction
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; r-squared = ",
      fmt_num(stats$r_squared),
      ", or ",
      fmt_num(
        100 * stats$r_squared,
        1
      ),
      "%."
    )
    
    solution_steps <- paste0(
      "1. Pearson's r is ",
      fmt_num(stats$r),
      ". ",
      "2. Square it: r-squared = (",
      fmt_num(stats$r),
      ")^2 = ",
      fmt_num(stats$r_squared),
      ". ",
      "3. Multiplying by 100 gives ",
      fmt_num(
        100 * stats$r_squared,
        1
      ),
      "%."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "calculate_t") {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "The sample contains ",
      stats$n,
      " vehicles and gives r = ",
      fmt_num(stats$r),
      " for ",
      x,
      " and ",
      y,
      ". Evaluate\n",
      "t = r × sqrt((n - 2) / (1 - r^2))\n",
      "and report the resulting degrees of freedom."
    )
    
    reference_answer <- paste0(
      "t(",
      stats$df,
      ") = ",
      fmt_num(stats$t_value),
      "."
    )
    
    solution_steps <- paste0(
      "1. Substitute r = ",
      fmt_num(stats$r),
      " and n = ",
      stats$n,
      ". ",
      "2. Compute t = ",
      fmt_num(stats$r),
      " × sqrt((",
      stats$n,
      " - 2) / (1 - ",
      fmt_num(stats$r),
      "^2)). ",
      "3. This gives t = ",
      fmt_num(stats$t_value),
      ". ",
      "4. The degrees of freedom are ",
      stats$df,
      "."
    )
    
    answer_type <- "numeric"
    
  } else if (task_name == "manual_r") {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "For ",
      x,
      " and ",
      y,
      ", covariance = ",
      fmt_num(stats$covariance),
      ", sd(",
      x,
      ") = ",
      fmt_num(stats$sd_x),
      " and sd(",
      y,
      ") = ",
      fmt_num(stats$sd_y),
      ". Use\n",
      "r = covariance / (sd_x × sd_y)\n",
      "to recover the correlation."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "."
    )
    
    solution_steps <- paste0(
      "1. Substitute the supplied quantities. ",
      "2. r = ",
      fmt_num(stats$covariance),
      " / (",
      fmt_num(stats$sd_x),
      " × ",
      fmt_num(stats$sd_y),
      "). ",
      "3. Therefore r = ",
      fmt_num(stats$r),
      "."
    )
    
    answer_type <- "numeric"
    
  } else if (task_name == "compare_mpg_predictors") {
    
    stats_a <- cor_mpg_wt
    stats_b <- cor_mpg_hp
    
    stronger_variable <- if (
      abs(stats_a$r) > abs(stats_b$r)
    ) {
      "wt"
    } else {
      "hp"
    }
    
    comparison_instruction <- comparison_phrases[
      ((i - 1) %% length(comparison_phrases)) + 1
    ]
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Calculate cor(mpg, wt) and cor(mpg, hp). ",
      comparison_instruction
    )
    
    reference_answer <- paste0(
      "cor(mpg, wt) = ",
      fmt_num(stats_a$r),
      "; cor(mpg, hp) = ",
      fmt_num(stats_b$r),
      "; ",
      stronger_variable,
      " has the stronger association with mpg."
    )
    
    solution_steps <- paste0(
      "1. cor(mpg, wt) = ",
      fmt_num(stats_a$r),
      ". ",
      "2. cor(mpg, hp) = ",
      fmt_num(stats_b$r),
      ". ",
      "3. Compare |r| values: ",
      fmt_num(abs(stats_a$r)),
      " and ",
      fmt_num(abs(stats_b$r)),
      ". ",
      "4. The stronger relationship is between mpg and ",
      stronger_variable,
      "."
    )
    
    answer_type <- "numeric_and_comparison"
    
    x <- "wt, hp"
    y <- "mpg"
    
  } else if (task_name == "compare_disp_relationships") {
    
    stats_a <- cor_wt_disp
    stats_b <- cor_hp_disp
    
    stronger_pair <- if (
      abs(stats_a$r) > abs(stats_b$r)
    ) {
      "wt and disp"
    } else {
      "hp and disp"
    }
    
    difference <- abs(
      abs(stats_a$r) -
        abs(stats_b$r)
    )
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Find cor(wt, disp) and cor(hp, disp). Which pair is more strongly ",
      "related in absolute terms, and what is the difference between the two |r| values?"
    )
    
    reference_answer <- paste0(
      "cor(wt, disp) = ",
      fmt_num(stats_a$r),
      "; cor(hp, disp) = ",
      fmt_num(stats_b$r),
      "; stronger pair = ",
      stronger_pair,
      "; difference in |r| = ",
      fmt_num(difference),
      "."
    )
    
    solution_steps <- paste0(
      "1. cor(wt, disp) = ",
      fmt_num(stats_a$r),
      ". ",
      "2. cor(hp, disp) = ",
      fmt_num(stats_b$r),
      ". ",
      "3. Compare ",
      fmt_num(abs(stats_a$r)),
      " and ",
      fmt_num(abs(stats_b$r)),
      ". ",
      "4. The stronger pair is ",
      stronger_pair,
      ". ",
      "5. The absolute difference is ",
      fmt_num(difference),
      "."
    )
    
    answer_type <- "numeric_and_comparison"
    
    x <- "wt, hp"
    y <- "disp"
    
  } else if (task_name == "interpret_negative") {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Obtain r for ",
      x,
      " and ",
      y,
      ". Then translate the sign into a directional statement about how ",
      x,
      " tends to change as ",
      y,
      " increases."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      ". As ",
      y,
      " increases, ",
      x,
      " tends to decrease."
    )
    
    solution_steps <- paste0(
      "1. The coefficient is r = ",
      fmt_num(stats$r),
      ". ",
      "2. Its sign is negative. ",
      "3. Therefore larger values of ",
      y,
      " tend to occur with smaller values of ",
      x,
      "."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "multicollinearity_screen") {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Calculate the Pearson correlation between ",
      x,
      " and ",
      y,
      ". Based on this pairwise result alone, should the relationship be ",
      "examined further as a possible source of multicollinearity?"
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; ",
      stats$strength,
      " ",
      stats$direction,
      " association. Further multicollinearity assessment is warranted."
    )
    
    solution_steps <- paste0(
      "1. Calculate r = ",
      fmt_num(stats$r),
      ". ",
      "2. Its magnitude is |r| = ",
      fmt_num(abs(stats$r)),
      ", classified as ",
      stats$strength,
      ". ",
      "3. A strong pairwise correlation may indicate overlapping predictor information. ",
      "4. VIF and the full regression model should still be checked."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "manual_t") {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Using r = ",
      fmt_num(stats$r),
      " and n = ",
      stats$n,
      ", reconstruct the two-sided correlation-test statistic manually.",
      " Report both t and its degrees of freedom."
    )
    
    reference_answer <- paste0(
      "t(",
      stats$df,
      ") = ",
      fmt_num(stats$t_value),
      "."
    )
    
    solution_steps <- paste0(
      "1. Use t = r × sqrt((n - 2) / (1 - r^2)). ",
      "2. Substitute r = ",
      fmt_num(stats$r),
      " and n = ",
      stats$n,
      ". ",
      "3. The result is t = ",
      fmt_num(stats$t_value),
      ". ",
      "4. df = ",
      stats$n,
      " - 2 = ",
      stats$df,
      "."
    )
    
    answer_type <- "numeric"
    
  } else {
    
    question <- paste0(
      finance_scenarios[i],
      "\n\n",
      "Using the complete mtcars sample, report Pearson's r for ",
      x,
      " and ",
      y,
      ", describe its direction and strength, and determine whether it is ",
      "statistically significant at the 5% level."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; ",
      stats$strength,
      " ",
      stats$direction,
      " correlation; p ",
      fmt_p(stats$p_value),
      "; statistically significant."
    )
    
    solution_steps <- paste0(
      "1. Calculate r = ",
      fmt_num(stats$r),
      ". ",
      "2. The sign indicates a ",
      stats$direction,
      " relationship. ",
      "3. |r| = ",
      fmt_num(abs(stats$r)),
      ", indicating a ",
      stats$strength,
      " association. ",
      "4. cor.test() gives p = ",
      fmt_num(stats$p_value, 6),
      ". ",
      "5. Since p < 0.05, the correlation is statistically significant."
    )
    
    answer_type <- "numeric_and_interpretation"
  }
  
  data.frame(
    id = sprintf(
      "R800_031_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_031",
    dataset_name = "mtcars",
    statistical_concept = "Correlation",
    task = "correlation_calculation",
    template_id = paste0(
      "correlation_",
      task_name
    ),
    difficulty = "medium",
    scenario = "finance",
    language_style = finance_styles[i],
    question_type = "calculation",
    predictor = x,
    response = y,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_033
# Healthcare + Easy + Calculation
# ============================================================

healthcare_scenarios <- c(
  
  paste(
    "Before students are allowed to work with confidential patient records,",
    "they practise on mtcars. Vehicle weight is used as a stand-in for body",
    "mass and mpg as a proxy efficiency measure."
  ),
  
  paste(
    "The first exercise in a health-data workshop contains no patient data.",
    "Instead, mpg and engine displacement are used to practise a basic",
    "correlation calculation."
  ),
  
  paste(
    "A teaching example describes horsepower as a proxy for activity intensity",
    "and mpg as a proxy for physiological efficiency. The numerical relationship",
    "between the two benchmark measures is required."
  ),
  
  paste(
    "Imagine that engine displacement represented organ volume and vehicle",
    "weight represented body size. The mtcars values provide the practice data."
  ),
  
  paste(
    "Two continuous measurements have been placed side by side in a training",
    "dataset. The immediate question is whether they tend to rise together",
    "or move in opposite directions."
  ),
  
  paste(
    "The scatterplot used in a rehabilitation-data lesson slopes downward.",
    "A calculation using mpg and wt will confirm whether the graph has been",
    "read correctly."
  ),
  
  paste(
    "For a laboratory methods demonstration, horsepower and displacement",
    "serve as anonymous substitutes for two biochemical measurements."
  ),
  
  paste(
    "After calculating r, a trainee describes the mpg-horsepower relationship",
    "as positive. The sign of the actual coefficient needs to be checked."
  ),
  
  paste(
    "A mock clinical report needs something more precise than the words",
    "'related' or 'not related'. The relationship between mpg and wt must",
    "be quantified and classified."
  ),
  
  paste(
    "The lesson now turns from correlation to shared linear variation.",
    "The example uses mpg and engine displacement."
  ),
  
  paste(
    "A trainee has reported a negative association between mileage and",
    "horsepower. Both the signed coefficient and its magnitude are requested."
  ),
  
  paste(
    "No interpretation is required at the first stage of this exercise.",
    "The task is simply to calculate the Pearson correlation between weight",
    "and engine displacement."
  ),
  
  paste(
    "The benchmark measurements horsepower and displacement appear to follow",
    "nearly the same upward pattern. The exercise asks students to measure",
    "and classify that pattern."
  ),
  
  paste(
    "A cor.test() output is required for a software-practice session.",
    "The selected variables are mpg and wt."
  ),
  
  paste(
    "To finish the training session, the mpg-horsepower relationship must",
    "be summarised with one coefficient and one plain-language sentence."
  )
)

healthcare_styles <- c(
  "training-context",
  "workshop",
  "analogy",
  "hypothetical",
  "direction-focused",
  "graph-led",
  "laboratory",
  "error-checking",
  "plain-language",
  "stepwise",
  "magnitude-focused",
  "direct-calculation",
  "pattern-description",
  "software-practice",
  "summary"
)

healthcare_tasks <- c(
  "simple_r",
  "simple_r",
  "simple_r",
  "simple_r",
  "direction",
  "direction",
  "simple_r",
  "positive_or_negative",
  "strength",
  "simple_r_squared",
  "absolute_r",
  "simple_r",
  "strength",
  "r_and_p",
  "short_interpretation"
)

healthcare_pairs <- c(
  "mpg_wt",
  "mpg_disp",
  "mpg_hp",
  "wt_disp",
  "wt_hp",
  "mpg_wt",
  "hp_disp",
  "mpg_hp",
  "mpg_wt",
  "mpg_disp",
  "mpg_hp",
  "wt_disp",
  "hp_disp",
  "mpg_wt",
  "mpg_hp"
)

build_healthcare_question <- function(i) {
  
  stats <- correlation_lookup[[healthcare_pairs[i]]]
  
  task_name <- healthcare_tasks[i]
  
  x <- stats$x
  y <- stats$y
  
  if (task_name == "simple_r") {
    
    simple_prompts <- c(
      "Calculate Pearson's r for %s and %s and round to three decimal places.",
      "Using mtcars, what is cor(%s, %s)? Give three decimal places.",
      "Find the sample correlation between %s and %s.",
      "Obtain the Pearson coefficient describing %s and %s.",
      "Report r for the two variables %s and %s."
    )
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      sprintf(
        simple_prompts[
          ((i - 1) %% length(simple_prompts)) + 1
        ],
        x,
        y
      )
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "."
    )
    
    solution_steps <- paste0(
      "1. Use cor(mtcars$",
      x,
      ", mtcars$",
      y,
      "). ",
      "2. The Pearson correlation is ",
      fmt_num(stats$r),
      "."
    )
    
    answer_type <- "numeric"
    
  } else if (task_name == "direction") {
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Compute r for ",
      x,
      " and ",
      y,
      ". Based on the sign, state whether the association is positive or negative."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; ",
      stats$direction,
      " relationship."
    )
    
    solution_steps <- paste0(
      "1. Calculate r = ",
      fmt_num(stats$r),
      ". ",
      "2. Because the coefficient is ",
      ifelse(stats$r < 0, "below zero", "above zero"),
      ", the association is ",
      stats$direction,
      "."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "positive_or_negative") {
    
    movement <- if (
      stats$r > 0
    ) {
      "the same direction"
    } else {
      "opposite directions"
    }
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Find cor(",
      x,
      ", ",
      y,
      "). Do the two measures generally move in the same direction or in ",
      "opposite directions?"
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; the variables tend to move in ",
      movement,
      "."
    )
    
    solution_steps <- paste0(
      "1. Compute r = ",
      fmt_num(stats$r),
      ". ",
      "2. A ",
      stats$direction,
      " coefficient means the variables tend to move in ",
      movement,
      "."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "strength") {
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Calculate Pearson's r for ",
      x,
      " and ",
      y,
      ". Then classify the relationship as weak, moderate, strong or very strong."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; ",
      stats$strength,
      " association."
    )
    
    solution_steps <- paste0(
      "1. Calculate r = ",
      fmt_num(stats$r),
      ". ",
      "2. Use |r| = ",
      fmt_num(abs(stats$r)),
      " to assess strength. ",
      "3. This is classified as ",
      stats$strength,
      "."
    )
    
    answer_type <- "numeric_and_interpretation"
    
  } else if (task_name == "simple_r_squared") {
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "First calculate r between ",
      x,
      " and ",
      y,
      ". Then square the coefficient."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; r-squared = ",
      fmt_num(stats$r_squared),
      "."
    )
    
    solution_steps <- paste0(
      "1. Calculate r = ",
      fmt_num(stats$r),
      ". ",
      "2. Square it: ",
      fmt_num(stats$r),
      "^2 = ",
      fmt_num(stats$r_squared),
      "."
    )
    
    answer_type <- "numeric"
    
  } else if (task_name == "absolute_r") {
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Report both Pearson's r and |r| for ",
      x,
      " and ",
      y,
      "."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; |r| = ",
      fmt_num(abs(stats$r)),
      "."
    )
    
    solution_steps <- paste0(
      "1. The signed correlation is r = ",
      fmt_num(stats$r),
      ". ",
      "2. Removing the sign gives |r| = ",
      fmt_num(abs(stats$r)),
      "."
    )
    
    answer_type <- "numeric"
    
  } else if (task_name == "r_and_p") {
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Run cor.test() for ",
      x,
      " and ",
      y,
      ". Report the sample correlation and the p-value."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      "; p ",
      fmt_p(stats$p_value),
      "."
    )
    
    solution_steps <- paste0(
      "1. Run cor.test(mtcars$",
      x,
      ", mtcars$",
      y,
      "). ",
      "2. The coefficient is r = ",
      fmt_num(stats$r),
      ". ",
      "3. The p-value is ",
      fmt_num(stats$p_value, 6),
      "."
    )
    
    answer_type <- "numeric"
    
  } else {
    
    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Calculate Pearson's r for ",
      x,
      " and ",
      y,
      " and interpret the result in one sentence."
    )
    
    reference_answer <- paste0(
      "r = ",
      fmt_num(stats$r),
      ". This indicates a ",
      stats$strength,
      " ",
      stats$direction,
      " linear relationship."
    )
    
    solution_steps <- paste0(
      "1. Calculate r = ",
      fmt_num(stats$r),
      ". ",
      "2. Use the sign for direction and |r| for strength. ",
      "3. The result is a ",
      stats$strength,
      " ",
      stats$direction,
      " association."
    )
    
    answer_type <- "numeric_and_interpretation"
  }
  
  data.frame(
    id = sprintf(
      "R800_033_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_033",
    dataset_name = "mtcars",
    statistical_concept = "Correlation",
    task = "correlation_calculation",
    template_id = paste0(
      "correlation_",
      task_name
    ),
    difficulty = "easy",
    scenario = "healthcare",
    language_style = healthcare_styles[i],
    question_type = "calculation",
    predictor = x,
    response = y,
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

healthcare_questions <- do.call(
  rbind,
  lapply(
    seq_len(15),
    build_healthcare_question
  )
)

correlation_questions <- rbind(
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
  nrow(correlation_questions) == 35
)

stopifnot(
  length(
    unique(correlation_questions$id)
  ) == 35
)

stopifnot(
  !anyDuplicated(
    correlation_questions$question
  )
)

stopifnot(
  sum(
    correlation_questions$blueprint_id ==
      "R800_031"
  ) == 20
)

stopifnot(
  sum(
    correlation_questions$blueprint_id ==
      "R800_033"
  ) == 15
)

stopifnot(
  all(
    finance_questions$difficulty ==
      "medium"
  )
)

stopifnot(
  all(
    healthcare_questions$difficulty ==
      "easy"
  )
)

stopifnot(
  all(
    correlation_questions$question_type ==
      "calculation"
  )
)

stopifnot(
  all(
    correlation_questions$dataset_name ==
      "mtcars"
  )
)

stopifnot(
  all(
    nchar(
      correlation_questions$question
    ) > 100
  )
)

# Check solution-step completeness without imposing an unnecessarily high
# character threshold on easy questions.
solution_step_lengths <- nchar(correlation_questions$solution_steps)

stopifnot(
  all(
    !is.na(solution_step_lengths) &
      solution_step_lengths >= 40
  )
)

cat(
  "\nMinimum solution_steps length: ",
  min(solution_step_lengths),
  " characters.\n",
  sep = ""
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
  "\nQuestion count by difficulty:\n"
)

print(
  table(
    correlation_questions$difficulty
  )
)

cat(
  "\nQuestion count by variable pair:\n"
)

print(
  table(
    paste(
      correlation_questions$predictor,
      correlation_questions$response,
      sep = " -> "
    )
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
  correlation_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

# ------------------------------------------------------------
# Display full examples
# ------------------------------------------------------------

cat(
  "\n\n================ R800_031 example ================\n\n"
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
  "\n\n================ R800_033 example ================\n\n"
)

cat(
  healthcare_questions$question[1],
  "\n\nReference answer:\n",
  healthcare_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  healthcare_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV
# ------------------------------------------------------------

csv_file <- "R800_031_R800_033_Correlation_v2.csv"

write.csv(
  correlation_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

# ------------------------------------------------------------
# Export JSON
# ------------------------------------------------------------

json_file <- "R800_031_R800_033_Correlation_v2.json"

write_json(
  correlation_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

# ------------------------------------------------------------
# Completion message
# ------------------------------------------------------------

cat(
  "\n\nSuccessfully generated ",
  nrow(correlation_questions),
  " correlation questions.\n",
  sep = ""
)

cat(
  "R800_031 finance questions: ",
  nrow(finance_questions),
  "\n",
  sep = ""
)

cat(
  "R800_033 healthcare questions: ",
  nrow(healthcare_questions),
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