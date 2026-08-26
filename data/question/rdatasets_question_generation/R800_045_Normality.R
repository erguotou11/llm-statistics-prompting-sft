# ============================================================
# Normality Question Generator
#
# R800_045
# Dataset: faithful
# Domain: General Everyday
# Difficulty: Easy
# Question type: Calculation
# Count: 10
#
# Outputs:
# 1. R800_045_Normality_v2.csv
# 2. R800_045_Normality_v2.json
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

fmt_pct <- function(x, digits = 1) {
  paste0(
    formatC(
      100 * x,
      format = "f",
      digits = digits
    ),
    "%"
  )
}

# ------------------------------------------------------------
# Normality helpers
# ------------------------------------------------------------

normality_summary <- function(x) {

  shapiro_result <- shapiro.test(x)

  n_value <- length(x)
  mean_value <- mean(x)
  median_value <- median(x)
  sd_value <- sd(x)

  lower_1sd <- mean_value - sd_value
  upper_1sd <- mean_value + sd_value

  lower_2sd <- mean_value - 2 * sd_value
  upper_2sd <- mean_value + 2 * sd_value

  prop_1sd <- mean(
    x >= lower_1sd &
      x <= upper_1sd
  )

  prop_2sd <- mean(
    x >= lower_2sd &
      x <= upper_2sd
  )

  ordered_x <- sort(x)

  theoretical_quantiles <- qnorm(
    ppoints(
      n_value
    )
  )

  qq_correlation <- cor(
    ordered_x,
    theoretical_quantiles
  )

  list(
    n = n_value,
    mean = mean_value,
    median = median_value,
    sd = sd_value,
    mean_median_gap = abs(
      mean_value -
        median_value
    ),
    shapiro_w = unname(
      shapiro_result$statistic
    ),
    shapiro_p = shapiro_result$p.value,
    lower_1sd = lower_1sd,
    upper_1sd = upper_1sd,
    lower_2sd = lower_2sd,
    upper_2sd = upper_2sd,
    prop_1sd = prop_1sd,
    prop_2sd = prop_2sd,
    count_1sd = sum(
      x >= lower_1sd &
        x <= upper_1sd
    ),
    count_2sd = sum(
      x >= lower_2sd &
        x <= upper_2sd
    ),
    qq_correlation = qq_correlation,
    min = min(x),
    max = max(x)
  )
}

eruptions_stats <- normality_summary(
  faithful$eruptions
)

waiting_stats <- normality_summary(
  faithful$waiting
)

# Two naturally occurring groups used for simple within-group checks.
short_waiting <- faithful$waiting[
  faithful$waiting < 70
]

long_waiting <- faithful$waiting[
  faithful$waiting >= 70
]

short_waiting_stats <- normality_summary(
  short_waiting
)

long_waiting_stats <- normality_summary(
  long_waiting
)

# ------------------------------------------------------------
# Scenarios and task definitions
# ------------------------------------------------------------

everyday_scenarios <- c(

  paste(
    "Visitors often ask whether eruption durations cluster around one",
    "typical value. The full faithful dataset provides a quick numerical",
    "check before anyone assumes a bell-shaped pattern."
  ),

  paste(
    "At a visitor centre, waiting times are being summarised for a",
    "simple information board. Before using a normal model, the staff",
    "calculate a Shapiro-Wilk test."
  ),

  paste(
    "Rather than judging a histogram by eye, a short data exercise",
    "compares the mean and median eruption duration."
  ),

  paste(
    "For a practical rule-of-thumb check, waiting times within one",
    "standard deviation of the mean are counted and converted to a percentage."
  ),

  paste(
    "Another quick check looks at how many eruption durations fall",
    "within two standard deviations of their mean."
  ),

  paste(
    "A Q-Q plot would normally be inspected visually, but this exercise",
    "uses the correlation between ordered observations and theoretical",
    "normal quantiles as a simple numerical summary."
  ),

  paste(
    "Long waiting periods are separated from shorter ones so that the",
    "shape of each group can be examined without mixing two visibly",
    "different clusters."
  ),

  paste(
    "Among the shorter waiting periods, the Shapiro-Wilk statistic",
    "is calculated to check whether that subgroup is reasonably close",
    "to a normal pattern."
  ),

  paste(
    "One eruption lasted 4.5 minutes. To see whether that value is unusual",
    "relative to the full eruption distribution, it is converted into a z-score."
  ),

  paste(
    "To finish the exercise, the normality evidence for eruptions and",
    "waiting times is compared using their Shapiro-Wilk p-values."
  )
)

language_styles <- c(
  "visitor-question",
  "information-board",
  "summary-comparison",
  "rule-of-thumb",
  "empirical-rule",
  "qq-numerical",
  "subgroup-check",
  "focused-test",
  "standard-score",
  "comparative-summary"
)

normality_tasks <- c(
  "shapiro_eruptions",
  "shapiro_waiting",
  "mean_median_gap",
  "within_one_sd",
  "within_two_sd",
  "qq_correlation",
  "shapiro_long_waiting",
  "shapiro_short_waiting",
  "eruption_z_score",
  "compare_shapiro"
)

# ------------------------------------------------------------
# Build one question
# ------------------------------------------------------------

build_normality_question <- function(i) {

  task_name <- normality_tasks[i]

  if (task_name == "shapiro_eruptions") {

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "Run shapiro.test(faithful$eruptions). Report W and the p-value,",
      " then state whether normality would be rejected at the 5% level."
    )

    reject_text <- ifelse(
      eruptions_stats$shapiro_p < 0.05,
      "reject normality",
      "do not reject normality"
    )

    reference_answer <- paste0(
      "W = ",
      fmt_num(eruptions_stats$shapiro_w),
      "; p ",
      fmt_p(eruptions_stats$shapiro_p),
      "; ",
      reject_text,
      " at alpha = 0.05."
    )

    solution_steps <- paste0(
      "1. Run shapiro.test(faithful$eruptions). ",
      "2. Read W = ",
      fmt_num(eruptions_stats$shapiro_w),
      " and p = ",
      fmt_num(eruptions_stats$shapiro_p, 6),
      ". ",
      "3. Compare p with 0.05. ",
      "4. Because p is ",
      ifelse(
        eruptions_stats$shapiro_p < 0.05,
        "below",
        "above"
      ),
      " 0.05, ",
      reject_text,
      "."
    )

    predictor_value <- ""
    response_value <- "eruptions"
    answer_type <- "numeric_and_decision"

  } else if (task_name == "shapiro_waiting") {

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "Use shapiro.test(faithful$waiting). Give the test statistic and",
      " p-value, then make the 5% decision."
    )

    reject_text <- ifelse(
      waiting_stats$shapiro_p < 0.05,
      "reject normality",
      "do not reject normality"
    )

    reference_answer <- paste0(
      "W = ",
      fmt_num(waiting_stats$shapiro_w),
      "; p ",
      fmt_p(waiting_stats$shapiro_p),
      "; ",
      reject_text,
      "."
    )

    solution_steps <- paste0(
      "1. Apply the Shapiro-Wilk test to waiting. ",
      "2. W = ",
      fmt_num(waiting_stats$shapiro_w),
      " and p = ",
      fmt_num(waiting_stats$shapiro_p, 6),
      ". ",
      "3. Since p is ",
      ifelse(
        waiting_stats$shapiro_p < 0.05,
        "less than",
        "greater than"
      ),
      " 0.05, ",
      reject_text,
      "."
    )

    predictor_value <- ""
    response_value <- "waiting"
    answer_type <- "numeric_and_decision"

  } else if (task_name == "mean_median_gap") {

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "Calculate the mean and median of faithful$eruptions, then report",
      " the absolute difference between them."
    )

    reference_answer <- paste0(
      "Mean = ",
      fmt_num(eruptions_stats$mean),
      "; median = ",
      fmt_num(eruptions_stats$median),
      "; absolute difference = ",
      fmt_num(eruptions_stats$mean_median_gap),
      "."
    )

    solution_steps <- paste0(
      "1. mean(faithful$eruptions) = ",
      fmt_num(eruptions_stats$mean),
      ". ",
      "2. median(faithful$eruptions) = ",
      fmt_num(eruptions_stats$median),
      ". ",
      "3. Absolute difference = |",
      fmt_num(eruptions_stats$mean),
      " - ",
      fmt_num(eruptions_stats$median),
      "| = ",
      fmt_num(eruptions_stats$mean_median_gap),
      "."
    )

    predictor_value <- ""
    response_value <- "eruptions"
    answer_type <- "numeric"

  } else if (task_name == "within_one_sd") {

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "For faithful$waiting, calculate mean ± 1 SD. Then count the",
      " observations inside that interval and report the proportion."
    )

    reference_answer <- paste0(
      "Mean ± 1 SD = ",
      format_interval(
        waiting_stats$lower_1sd,
        waiting_stats$upper_1sd
      ),
      "; count = ",
      waiting_stats$count_1sd,
      " of ",
      waiting_stats$n,
      "; proportion = ",
      fmt_num(waiting_stats$prop_1sd),
      " (",
      fmt_pct(waiting_stats$prop_1sd),
      ")."
    )

    solution_steps <- paste0(
      "1. Mean waiting = ",
      fmt_num(waiting_stats$mean),
      " and SD = ",
      fmt_num(waiting_stats$sd),
      ". ",
      "2. The interval is ",
      format_interval(
        waiting_stats$lower_1sd,
        waiting_stats$upper_1sd
      ),
      ". ",
      "3. Count values in the interval: ",
      waiting_stats$count_1sd,
      ". ",
      "4. Divide by ",
      waiting_stats$n,
      " to obtain ",
      fmt_num(waiting_stats$prop_1sd),
      "."
    )

    predictor_value <- ""
    response_value <- "waiting"
    answer_type <- "numeric"

  } else if (task_name == "within_two_sd") {

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "For faithful$eruptions, calculate the interval mean ± 2 SD,",
      " then find the number and percentage of observations inside it."
    )

    reference_answer <- paste0(
      "Mean ± 2 SD = ",
      format_interval(
        eruptions_stats$lower_2sd,
        eruptions_stats$upper_2sd
      ),
      "; count = ",
      eruptions_stats$count_2sd,
      " of ",
      eruptions_stats$n,
      "; percentage = ",
      fmt_pct(eruptions_stats$prop_2sd),
      "."
    )

    solution_steps <- paste0(
      "1. Mean eruptions = ",
      fmt_num(eruptions_stats$mean),
      " and SD = ",
      fmt_num(eruptions_stats$sd),
      ". ",
      "2. Calculate mean - 2SD and mean + 2SD. ",
      "3. The interval is ",
      format_interval(
        eruptions_stats$lower_2sd,
        eruptions_stats$upper_2sd
      ),
      ". ",
      "4. ",
      eruptions_stats$count_2sd,
      " of ",
      eruptions_stats$n,
      " observations lie inside, equal to ",
      fmt_pct(eruptions_stats$prop_2sd),
      "."
    )

    predictor_value <- ""
    response_value <- "eruptions"
    answer_type <- "numeric"

  } else if (task_name == "qq_correlation") {

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "For faithful$waiting, calculate cor(sort(waiting),",
      " qnorm(ppoints(length(waiting)))). Report the result to three decimals."
    )

    reference_answer <- paste0(
      "Q-Q correlation = ",
      fmt_num(waiting_stats$qq_correlation),
      "."
    )

    solution_steps <- paste0(
      "1. Sort the waiting observations. ",
      "2. Generate theoretical normal quantiles with qnorm(ppoints(n)). ",
      "3. Correlate the two ordered vectors. ",
      "4. The resulting Q-Q correlation is ",
      fmt_num(waiting_stats$qq_correlation),
      "."
    )

    predictor_value <- ""
    response_value <- "waiting"
    answer_type <- "numeric"

  } else if (task_name == "shapiro_long_waiting") {

    decision <- ifelse(
      long_waiting_stats$shapiro_p < 0.05,
      "reject normality",
      "do not reject normality"
    )

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "For waiting times of 70 minutes or more, run a Shapiro-Wilk test.",
      " Report n, W, p and the 5% decision."
    )

    reference_answer <- paste0(
      "n = ",
      long_waiting_stats$n,
      "; W = ",
      fmt_num(long_waiting_stats$shapiro_w),
      "; p ",
      fmt_p(long_waiting_stats$shapiro_p),
      "; ",
      decision,
      "."
    )

    solution_steps <- paste0(
      "1. Keep waiting values greater than or equal to 70. ",
      "2. The subgroup contains ",
      long_waiting_stats$n,
      " observations. ",
      "3. shapiro.test() gives W = ",
      fmt_num(long_waiting_stats$shapiro_w),
      " and p = ",
      fmt_num(long_waiting_stats$shapiro_p, 6),
      ". ",
      "4. At alpha = 0.05, ",
      decision,
      "."
    )

    predictor_value <- ""
    response_value <- "waiting"
    answer_type <- "numeric_and_decision"

  } else if (task_name == "shapiro_short_waiting") {

    decision <- ifelse(
      short_waiting_stats$shapiro_p < 0.05,
      "reject normality",
      "do not reject normality"
    )

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "For waiting times below 70 minutes, calculate the Shapiro-Wilk",
      " statistic and p-value, then make the 5% decision."
    )

    reference_answer <- paste0(
      "n = ",
      short_waiting_stats$n,
      "; W = ",
      fmt_num(short_waiting_stats$shapiro_w),
      "; p ",
      fmt_p(short_waiting_stats$shapiro_p),
      "; ",
      decision,
      "."
    )

    solution_steps <- paste0(
      "1. Select waiting values below 70. ",
      "2. n = ",
      short_waiting_stats$n,
      ". ",
      "3. The test gives W = ",
      fmt_num(short_waiting_stats$shapiro_w),
      " and p = ",
      fmt_num(short_waiting_stats$shapiro_p, 6),
      ". ",
      "4. Compare p with 0.05 and ",
      decision,
      "."
    )

    predictor_value <- ""
    response_value <- "waiting"
    answer_type <- "numeric_and_decision"

  } else if (task_name == "eruption_z_score") {

    observed_value <- 4.5

    z_value <- (
      observed_value -
        eruptions_stats$mean
    ) /
      eruptions_stats$sd

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "Using the mean and SD of faithful$eruptions, calculate the",
      " z-score for an eruption lasting ",
      fmt_num(observed_value, 1),
      " minutes."
    )

    reference_answer <- paste0(
      "z = ",
      fmt_num(z_value),
      "."
    )

    solution_steps <- paste0(
      "1. Mean = ",
      fmt_num(eruptions_stats$mean),
      " and SD = ",
      fmt_num(eruptions_stats$sd),
      ". ",
      "2. Use z = (x - mean) / SD. ",
      "3. z = (",
      fmt_num(observed_value, 1),
      " - ",
      fmt_num(eruptions_stats$mean),
      ") / ",
      fmt_num(eruptions_stats$sd),
      " = ",
      fmt_num(z_value),
      "."
    )

    predictor_value <- ""
    response_value <- "eruptions"
    answer_type <- "numeric"

  } else {

    more_normal_variable <- if (
      eruptions_stats$shapiro_p >
        waiting_stats$shapiro_p
    ) {
      "eruptions"
    } else {
      "waiting"
    }

    question <- paste0(
      everyday_scenarios[i],
      "\n\n",
      "The Shapiro-Wilk p-values are ",
      fmt_num(eruptions_stats$shapiro_p, 6),
      " for eruptions and ",
      fmt_num(waiting_stats$shapiro_p, 6),
      " for waiting. Which variable has the larger p-value, and what are",
      " the two 5% decisions?"
    )

    reference_answer <- paste0(
      "Larger p-value: ",
      more_normal_variable,
      ". For eruptions, ",
      ifelse(
        eruptions_stats$shapiro_p < 0.05,
        "reject normality",
        "do not reject normality"
      ),
      "; for waiting, ",
      ifelse(
        waiting_stats$shapiro_p < 0.05,
        "reject normality",
        "do not reject normality"
      ),
      "."
    )

    solution_steps <- paste0(
      "1. Compare the two p-values. ",
      "2. The larger value belongs to ",
      more_normal_variable,
      ". ",
      "3. Compare each p-value with 0.05. ",
      "4. Record the normality decision separately for each variable."
    )

    predictor_value <- ""
    response_value <- "eruptions, waiting"
    answer_type <- "numeric_and_comparison"
  }

  data.frame(
    id = sprintf(
      "R800_045_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_045",
    dataset_name = "faithful",
    statistical_concept = "Normality",
    task = "normality_calculation",
    template_id = paste0(
      "normality_shapiro_qq_",
      task_name
    ),
    difficulty = "easy",
    scenario = "general_everyday",
    language_style = language_styles[i],
    question_type = "calculation",
    predictor = predictor_value,
    response = response_value,
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

normality_questions <- normality_questions[
  ,
  required_columns
]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(
    names(normality_questions),
    required_columns
  )
)

stopifnot(
  nrow(normality_questions) == 10
)

stopifnot(
  length(
    unique(normality_questions$id)
  ) == 10
)

stopifnot(
  !anyDuplicated(
    normality_questions$question
  )
)

stopifnot(
  all(
    normality_questions$blueprint_id ==
      "R800_045"
  )
)

stopifnot(
  all(
    normality_questions$difficulty ==
      "easy"
  )
)

stopifnot(
  all(
    normality_questions$question_type ==
      "calculation"
  )
)

stopifnot(
  all(
    nchar(normality_questions$question) >= 80
  )
)

stopifnot(
  all(
    nchar(normality_questions$solution_steps) >= 40
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
    normality_questions$blueprint_id
  )
)

cat(
  "\nAnswer types:\n"
)

print(
  table(
    normality_questions$answer_type
  )
)

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
  normality_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

cat(
  "\n\n================ R800_045 example ================\n\n"
)

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

csv_file <- "R800_045_Normality_v2.csv"
json_file <- "R800_045_Normality_v2.json"

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
