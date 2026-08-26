# ============================================================
# R800_019 + R800_022 + R800_024
#
# R800_019
# Confidence Interval / ToothGrowth / Education
# Medium / Single Choice / 20
#
# R800_022
# Confidence Interval / ToothGrowth / Marketing
# Medium / Multiple Choice / 15
#
# R800_024
# Confidence Interval / ToothGrowth / General Everyday
# Easy / Single Choice / 10
#
# Output:
#   R800_019_022_024_questions.csv
#   R800_019_022_024_questions.json
#
# Design principles:
# - Real R data from ToothGrowth
# - One combined script and one combined CSV/JSON output pair
# - Strong variation in scenario, discourse form and information order
# - Medium items require conceptual judgement across several ideas
# - Easy items test one direct confidence-interval concept only
# - Single Choice: exactly one correct option
# - Multiple Choice: one or more correct options
# ============================================================

set.seed(1922024)

data(ToothGrowth)
TG <- ToothGrowth

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

# ============================================================
# Shared helpers
# ============================================================

fmt <- function(x, digits = 3) {
  format(
    round(as.numeric(x), digits),
    nsmall = digits,
    trim = TRUE,
    scientific = FALSE
  )
}

pick <- function(x) {
  sample(x, size = 1)
}

one_mean_ci <- function(x, conf_level = 0.95) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s / sqrt(n)
  alpha <- 1 - conf_level
  t_star <- qt(1 - alpha / 2, df = n - 1)
  margin <- t_star * se

  list(
    n = n,
    mean = m,
    sd = s,
    se = se,
    df = n - 1,
    t_star = t_star,
    margin = margin,
    lower = m - margin,
    upper = m + margin,
    width = 2 * margin
  )
}

welch_diff_ci <- function(x, y, conf_level = 0.95) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- var(x)
  vy <- var(y)

  diff_value <- mx - my
  se <- sqrt(vx / nx + vy / ny)

  df <- (vx / nx + vy / ny)^2 /
    ((vx / nx)^2 / (nx - 1) + (vy / ny)^2 / (ny - 1))

  alpha <- 1 - conf_level
  t_star <- qt(1 - alpha / 2, df = df)
  margin <- t_star * se

  list(
    nx = nx,
    ny = ny,
    mean_x = mx,
    mean_y = my,
    var_x = vx,
    var_y = vy,
    diff = diff_value,
    se = se,
    df = df,
    t_star = t_star,
    margin = margin,
    lower = diff_value - margin,
    upper = diff_value + margin,
    width = 2 * margin
  )
}

contains_zero <- function(lower, upper) {
  lower <= 0 && upper >= 0
}

shuffle_single <- function(options, correct_text) {
  idx <- sample(seq_along(options))
  shuffled <- options[idx]
  correct_letter <- LETTERS[match(correct_text, shuffled)]

  list(
    option_a = shuffled[1],
    option_b = shuffled[2],
    option_c = shuffled[3],
    option_d = shuffled[4],
    correct_answer = correct_letter
  )
}

shuffle_multiple <- function(options, correct_flags) {
  idx <- sample(seq_along(options))
  shuffled_options <- options[idx]
  shuffled_flags <- correct_flags[idx]

  list(
    option_a = shuffled_options[1],
    option_b = shuffled_options[2],
    option_c = shuffled_options[3],
    option_d = shuffled_options[4],
    correct_answer = paste(
      LETTERS[which(shuffled_flags)],
      collapse = ","
    )
  )
}

compose_prompt <- function(context, output_text, question_text, layout_id) {
  if (layout_id == 1) {
    paste0(
      context,
      "\n\nStatistical information:\n",
      output_text,
      "\n\n",
      question_text
    )
  } else if (layout_id == 2) {
    paste0(
      question_text,
      "\n\nUse the following evidence:\n",
      output_text,
      "\n\nContext:\n",
      context
    )
  } else if (layout_id == 3) {
    paste0(
      context,
      "\n\n",
      question_text,
      "\n\nRelevant output:\n",
      output_text
    )
  } else {
    paste0(
      "Relevant output:\n",
      output_text,
      "\n\n",
      context,
      "\n\n",
      question_text
    )
  }
}

make_record <- function(
    id,
    blueprint_id,
    difficulty,
    scenario,
    template_id,
    language_style,
    presentation_layout,
    cognitive_skill,
    statistical_output,
    question,
    option_a,
    option_b,
    option_c,
    option_d,
    correct_answer,
    reference_answer,
    solution_steps,
    answer_type
) {
  data.frame(
    id = id,
    source = "R-generated",
    blueprint_id = blueprint_id,
    dataset_name = "ToothGrowth",
    statistical_concept = "confidence_interval",
    task = "confidence_interval",
    template_id = template_id,
    difficulty = difficulty,
    scenario = scenario,
    language_style = language_style,
    presentation_layout = presentation_layout,
    cognitive_skill = cognitive_skill,
    question_type = answer_type,
    variables_used = "len, supp, dose",
    statistical_output = statistical_output,
    question = question,
    option_a = option_a,
    option_b = option_b,
    option_c = option_c,
    option_d = option_d,
    correct_answer = correct_answer,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v1.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_019
# Education / Medium / Single Choice
# ============================================================

education_styles <- c(
  "marking_feedback",
  "tutorial_dialogue",
  "lecture_poll",
  "methods_workshop",
  "coursework_review",
  "exam_vignette",
  "student_consultation",
  "assessment_design",
  "seminar_debate",
  "textbook_revision"
)

education_openings <- list(

  marking_feedback = c(
    "A marker has written, 'The interval is calculated correctly, but the interpretation needs revision.'",
    "An assessment script contains a plausible answer that confuses the population mean with individual observations.",
    "The feedback session focuses on which confidence-interval statement is statistically defensible."
  ),

  tutorial_dialogue = c(
    "Tutor: \"The interval is not a range containing 95% of the data. What does it estimate instead?\"\nStudent: \"It describes uncertainty about a population parameter.\"",
    "Student: \"The intervals overlap, so the groups cannot differ.\"\nTutor: \"That shortcut is not reliable.\"",
    "Tutor: \"Before choosing the formula, decide whether you are estimating one mean or a difference of means.\""
  ),

  lecture_poll = c(
    "A live lecture poll asks students to identify the best interpretation of a t-based confidence interval.",
    "The lecturer displays four plausible statements and asks the class to select the only correct one.",
    "A concept-check question uses real ToothGrowth output to test understanding rather than arithmetic."
  ),

  methods_workshop = c(
    "A statistics workshop compares several possible interval methods for the same research question.",
    "Participants must decide which confidence interval matches the design and parameter of interest.",
    "A methods session asks why Welch's interval is often preferred for two independent groups."
  ),

  coursework_review = c(
    "A coursework review panel wants the item to test genuine interval reasoning rather than memorised wording.",
    "A submitted report gives the endpoints but not a correct explanation of what they mean.",
    "The revision task asks which conclusion follows from the interval and which conclusions overreach."
  ),

  exam_vignette = c(
    "A health-statistics examination presents a short ToothGrowth scenario and four candidate answers.",
    "An exam item asks students to choose the correct confidence-interval interpretation.",
    "A test question combines a real numerical interval with a method-selection decision."
  ),

  student_consultation = c(
    "A student brings a ToothGrowth analysis to office hours and asks whether zero inclusion proves the groups are identical.",
    "During a consultation, the tutor notices confusion between confidence level, interval width and sample size.",
    "A student wants to know why a 99% interval is wider than a 95% interval."
  ),

  assessment_design = c(
    "Course designers are checking whether distractors represent common statistical misconceptions.",
    "An assessment-development meeting reviews four possible answers to a confidence-interval question.",
    "The item is intended to distinguish careful interpretation from threshold-based guessing."
  ),

  seminar_debate = c(
    "A seminar group debates whether separate intervals for two means can replace an interval for their difference.",
    "Students discuss whether a non-significant interval result establishes equality.",
    "A classroom debate centres on statistical significance versus practical importance."
  ),

  textbook_revision = c(
    "A textbook editor asks which explanation should accompany the confidence-interval example.",
    "The next edition of a statistics text needs a more precise statement about long-run coverage.",
    "A worked example is being revised to remove a misleading probability interpretation."
  )
)

education_tasks <- c(
  "choose_one_mean_method",
  "choose_mean_difference_method",
  "interpret_one_mean_interval",
  "interpret_difference_interval",
  "interpret_zero_inclusion",
  "confidence_level_vs_width",
  "sample_size_vs_width",
  "welch_method_reason",
  "separate_intervals_warning",
  "long_run_coverage",
  "identify_parameter",
  "practical_threshold",
  "non_significance_not_equality",
  "correct_reporting_statement",
  "margin_of_error_meaning",
  "effect_of_variability",
  "paired_vs_independent",
  "extrapolation_limit",
  "confidence_not_probability",
  "best_follow_up"
)

generate_r800_019 <- function(i) {

  task_type <- education_tasks[i]
  style <- pick(education_styles)
  context <- pick(education_openings[[style]])
  layout_id <- sample(1:4, 1)

  dose <- pick(sort(unique(TG$dose)))
  supp <- pick(levels(TG$supp))

  group <- subset(
    TG,
    dose == dose & supp == supp
  )$len

  x <- subset(
    TG,
    dose == dose & supp == "OJ"
  )$len

  y <- subset(
    TG,
    dose == dose & supp == "VC"
  )$len

  mean_ci <- one_mean_ci(group, 0.95)
  diff_ci <- welch_diff_ci(x, y, 0.95)

  if (task_type == "choose_one_mean_method") {

    output_text <- paste0(
      "Goal: estimate the population mean len for ",
      supp, " at dose ", dose,
      "\nPopulation SD is unknown.",
      "\nSample size = ", length(group)
    )

    question_text <- pick(c(
      "Which confidence-interval method is most appropriate?",
      "Select the interval that matches this estimation problem.",
      "Which procedure should be used for the stated parameter?"
    ))

    correct <- "A one-sample t confidence interval for the population mean."

    options <- c(
      correct,
      "A Welch interval for the difference between two independent means.",
      "A confidence interval for a population proportion.",
      "A paired-mean interval because all observations come from one dataset."
    )

    explanation <- paste0(
      "Only one numerical sample is used and the population SD is unknown, so a one-sample t interval is appropriate."
    )

  } else if (task_type == "choose_mean_difference_method") {

    output_text <- paste0(
      "Goal: estimate mean(OJ) - mean(VC) at dose ",
      dose,
      "\nThe groups contain different observations.",
      "\nEqual population variances are not assumed."
    )

    question_text <- pick(c(
      "Which method directly estimates the required parameter?",
      "Select the most suitable interval procedure.",
      "Which confidence interval respects the design?"
    ))

    correct <- "A Welch two-sample confidence interval for the difference in population means."

    options <- c(
      correct,
      "A one-sample interval for all len values combined.",
      "A paired interval because OJ and VC are two levels of the same variable.",
      "Two unrelated confidence intervals for the group means, with no interval for their difference."
    )

    explanation <- paste0(
      "The target parameter is a difference between two independent population means, and Welch's method avoids assuming equal variances."
    )

  } else if (task_type == "interpret_one_mean_interval") {

    output_text <- paste0(
      "Group = ", supp,
      " at dose ", dose,
      "\n95% CI for the population mean = [",
      fmt(mean_ci$lower), ", ",
      fmt(mean_ci$upper), "]"
    )

    question_text <- pick(c(
      "Which interpretation is correct?",
      "How should this interval be explained?",
      "Select the statistically accurate statement."
    ))

    correct <- "The interval gives a range of plausible values for the population mean tooth length under this treatment condition."

    options <- c(
      correct,
      "Ninety-five percent of individual tooth lengths must lie inside the interval.",
      "There is a 95% probability that the sample mean lies inside the interval.",
      "The interval contains every possible future observation."
    )

    explanation <- paste0(
      "The interval estimates a population mean. It is not an individual-observation range."
    )

  } else if (task_type == "interpret_difference_interval") {

    output_text <- paste0(
      "95% CI for mean(OJ) - mean(VC) = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]"
    )

    question_text <- pick(c(
      "Which statement best describes this interval?",
      "Choose the correct interpretation of the mean-difference interval.",
      "What parameter is being estimated?"
    ))

    correct <- "The interval estimates the population mean difference between OJ and VC at the selected dose."

    options <- c(
      correct,
      "The interval estimates the difference between two individual observations.",
      "The interval is the range of all possible sample means.",
      "The interval estimates the percentage of subjects assigned to OJ."
    )

    explanation <- paste0(
      "The parameter is a population mean contrast, not an individual difference or a proportion."
    )

  } else if (task_type == "interpret_zero_inclusion") {

    output_text <- paste0(
      "95% CI for OJ - VC = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]"
    )

    question_text <- pick(c(
      "Which conclusion follows at the 5% two-sided level?",
      "What does zero inclusion or exclusion imply?",
      "Select the most defensible conclusion."
    ))

    if (contains_zero(diff_ci$lower, diff_ci$upper)) {
      correct <- "The interval includes zero, so the data do not provide sufficient evidence of a population mean difference at the 5% two-sided level."
    } else {
      correct <- "The interval excludes zero, so the data provide evidence of a non-zero population mean difference at the 5% two-sided level."
    }

    options <- c(
      correct,
      "Zero inclusion proves that the population means are exactly equal.",
      "Zero exclusion proves that every OJ observation exceeds every VC observation.",
      "Whether zero is included determines the sample size."
    )

    explanation <- paste0(
      "Zero inclusion connects to the corresponding hypothesis test, but does not prove equality or individual separation."
    )

  } else if (task_type == "confidence_level_vs_width") {

    output_text <- paste0(
      "Same sample and method",
      "\n90% interval width = ",
      fmt(one_mean_ci(group, 0.90)$width),
      "\n99% interval width = ",
      fmt(one_mean_ci(group, 0.99)$width)
    )

    question_text <- pick(c(
      "Why is the 99% interval wider?",
      "Which explanation correctly describes the confidence-precision trade-off?",
      "Select the best reason for the width difference."
    ))

    correct <- "A higher confidence level uses a larger critical value, which increases the margin of error."

    options <- c(
      correct,
      "The sample mean changes when the confidence level changes.",
      "The sample size becomes smaller at 99% confidence.",
      "A wider interval means the data contain more observations."
    )

    explanation <- paste0(
      "The data and sample mean remain fixed; only the critical value and margin change."
    )

  } else if (task_type == "sample_size_vs_width") {

    output_text <- paste0(
      "Assume the sample SD remains similar.",
      "\nCurrent n = ", length(group),
      "\nProposed n = ", length(group) * 4
    )

    question_text <- pick(c(
      "What should happen approximately to the interval width?",
      "Which prediction follows from the square-root rule?",
      "How would quadrupling n affect precision?"
    ))

    correct <- "The standard error and interval width would be approximately halved."

    options <- c(
      correct,
      "The interval width would become one quarter as large.",
      "The interval width would double.",
      "The confidence level would automatically rise to 100%."
    )

    explanation <- paste0(
      "SE scales approximately as 1/sqrt(n), so multiplying n by four divides SE and width by two."
    )

  } else if (task_type == "welch_method_reason") {

    output_text <- paste0(
      "OJ variance = ", fmt(var(x)),
      "\nVC variance = ", fmt(var(y)),
      "\nMethod under consideration: Welch interval"
    )

    question_text <- pick(c(
      "Why is Welch's method a sensible choice?",
      "Which justification is strongest?",
      "Select the correct reason for using the unequal-variance interval."
    ))

    correct <- "Welch's method does not require the two population variances to be equal."

    options <- c(
      correct,
      "Welch's method is used because it always produces the narrowest interval.",
      "Welch's method converts the observations into ranks.",
      "Welch's method is required whenever the sample means differ."
    )

    explanation <- paste0(
      "Welch adjusts the standard error and degrees of freedom when equal variances are uncertain."
    )

  } else if (task_type == "separate_intervals_warning") {

    a <- one_mean_ci(x, 0.95)
    b <- one_mean_ci(y, 0.95)

    output_text <- paste0(
      "OJ mean CI = [", fmt(a$lower), ", ",
      fmt(a$upper), "]",
      "\nVC mean CI = [", fmt(b$lower), ", ",
      fmt(b$upper), "]"
    )

    question_text <- pick(c(
      "Why should the comparison not rely only on visual overlap of these intervals?",
      "Which statement is correct about comparing two separate mean intervals?",
      "What is the better method for the group contrast?"
    ))

    correct <- "The direct confidence interval for mean(OJ) - mean(VC) answers the comparison question more appropriately than visually judging overlap."

    options <- c(
      correct,
      "Any overlap proves that the population means are equal.",
      "No overlap is required before a two-sample interval can be constructed.",
      "Separate intervals automatically account for the covariance of paired observations."
    )

    explanation <- paste0(
      "Intervals for two separate means are not equivalent to an interval for their difference."
    )

  } else if (task_type == "long_run_coverage") {

    output_text <- "A 95% confidence-interval procedure is repeated over many independent samples."

    question_text <- pick(c(
      "Which statement correctly describes 95% confidence?",
      "What is the long-run interpretation?",
      "Select the accurate coverage statement."
    ))

    correct <- "About 95% of intervals constructed by this procedure would contain the true population parameter in repeated sampling."

    options <- c(
      correct,
      "Each realised interval has a 95% chance of changing after publication.",
      "Ninety-five percent of the observations lie inside every interval.",
      "The true parameter changes from sample to sample."
    )

    explanation <- paste0(
      "Confidence refers to the long-run performance of the procedure, not a probability assigned to a fixed parameter after the interval is observed."
    )

  } else if (task_type == "identify_parameter") {

    output_text <- paste0(
      "Reported interval: [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]",
      "\nLabel: OJ - VC at dose ", dose
    )

    question_text <- pick(c(
      "Which population parameter does this interval target?",
      "What quantity is being estimated?",
      "Select the correct parameter."
    ))

    correct <- "The difference between the population mean tooth lengths for OJ and VC at the selected dose."

    options <- c(
      correct,
      "The difference between the two sample standard deviations.",
      "The proportion of observations above the sample mean.",
      "The difference between the maximum observed values."
    )

    explanation <- paste0(
      "The interval is explicitly defined for a mean contrast."
    )

  } else if (task_type == "practical_threshold") {

    threshold <- pick(c(1, 2, 3))

    output_text <- paste0(
      "95% CI for OJ - VC = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]",
      "\nMinimum practically important difference = ",
      threshold
    )

    question_text <- pick(c(
      "Which reasoning is correct?",
      "How should practical importance be judged?",
      "Select the best threshold-based interpretation."
    ))

    if (diff_ci$lower > threshold) {
      correct <- "The entire interval exceeds the practical threshold, so the data support an effect large enough to matter by this criterion."
    } else if (diff_ci$upper < threshold) {
      correct <- "The interval does not reach the practical threshold, so the required effect size is not supported."
    } else {
      correct <- "The interval crosses the practical threshold, so the evidence is inconclusive about whether the effect is large enough to matter."
    }

    options <- c(
      correct,
      "Only zero should ever be compared with a confidence interval.",
      "Any positive point estimate automatically exceeds the practical threshold.",
      "Practical importance is determined solely by the confidence level."
    )

    explanation <- paste0(
      "Practical importance requires comparing the entire plausible range with the chosen substantive threshold."
    )

  } else if (task_type == "non_significance_not_equality") {

    output_text <- paste0(
      "95% CI for OJ - VC = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]"
    )

    question_text <- pick(c(
      "Which statement avoids the equality fallacy?",
      "How should an interval containing zero be described?",
      "Select the statistically careful wording."
    ))

    correct <- "An interval containing zero means that no difference remains plausible; it does not prove exact equality."

    options <- c(
      correct,
      "An interval containing zero proves the two treatments are identical.",
      "An interval containing zero means the sample means must be equal.",
      "An interval containing zero makes the confidence interval invalid."
    )

    explanation <- paste0(
      "Failure to rule out zero is not proof that the true difference is exactly zero."
    )

  } else if (task_type == "correct_reporting_statement") {

    output_text <- paste0(
      "Estimated OJ - VC difference = ",
      fmt(diff_ci$diff),
      "\n95% CI = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]"
    )

    question_text <- pick(c(
      "Which is the best results sentence?",
      "Select the most complete and accurate report.",
      "Which wording should appear in the assignment?"
    ))

    correct <- paste0(
      "At dose ", dose,
      ", the estimated mean difference OJ - VC is ",
      fmt(diff_ci$diff),
      " units, with a 95% confidence interval from ",
      fmt(diff_ci$lower), " to ",
      fmt(diff_ci$upper), "."
    )

    options <- c(
      correct,
      "OJ is proven better because its sample mean is larger.",
      "The population difference is exactly equal to the sample difference.",
      "Ninety-five percent of all individual treatment effects lie inside the interval."
    )

    explanation <- paste0(
      "A good report states the comparison, point estimate, interval and condition without proof language."
    )

  } else if (task_type == "margin_of_error_meaning") {

    output_text <- paste0(
      "Sample mean = ", fmt(mean_ci$mean),
      "\nMargin of error = ", fmt(mean_ci$margin)
    )

    question_text <- pick(c(
      "What does the margin of error represent?",
      "Which description is correct?",
      "How is the margin used?"
    ))

    correct <- "The margin of error is the amount added to and subtracted from the point estimate to form the interval."

    options <- c(
      correct,
      "It is the largest observed measurement error in the sample.",
      "It is the difference between the sample mean and every observation.",
      "It is the probability that the interval is wrong."
    )

    explanation <- paste0(
      "For a symmetric t interval, endpoint = estimate ± margin."
    )

  } else if (task_type == "effect_of_variability") {

    output_text <- paste0(
      "Two hypothetical samples have the same n and mean.",
      "\nSample A SD = 3",
      "\nSample B SD = 8"
    )

    question_text <- pick(c(
      "Which sample would produce the wider confidence interval?",
      "How does greater variability affect interval width?",
      "Select the correct conclusion."
    ))

    correct <- "Sample B would produce the wider interval because its larger SD gives a larger standard error."

    options <- c(
      correct,
      "Sample A would produce the wider interval because its SD is smaller.",
      "Both intervals would have the same width because their means are equal.",
      "Variability affects only the midpoint, not the width."
    )

    explanation <- paste0(
      "With n fixed, SE = s/sqrt(n), so a larger SD produces a wider interval."
    )

  } else if (task_type == "paired_vs_independent") {

    output_text <- paste0(
      "Hypothetical redesign: each subject is measured once under OJ and once under VC."
    )

    question_text <- pick(c(
      "Which interval would then be appropriate?",
      "How would the method change?",
      "Select the interval that respects the paired structure."
    ))

    correct <- "A confidence interval for the mean within-subject difference."

    options <- c(
      correct,
      "A Welch interval treating all observations as independent.",
      "A one-sample interval for the combined raw observations.",
      "Two unrelated intervals for the group means with no pairing."
    )

    explanation <- paste0(
      "When both measurements come from the same subject, the analysis should use within-subject differences."
    )

  } else if (task_type == "extrapolation_limit") {

    output_text <- paste0(
      "Interval estimated at dose ", dose,
      "\nProposed interpretation: apply the same interval to a much larger unobserved dose"
    )

    question_text <- pick(c(
      "Which criticism is correct?",
      "Why is the proposed use questionable?",
      "Select the best limitation."
    ))

    correct <- "The interval is specific to the observed dose and should not be transferred to an unobserved dose without additional modelling assumptions."

    options <- c(
      correct,
      "Confidence intervals can never be used with numerical predictors.",
      "The interval becomes exact when applied outside the data range.",
      "Extrapolation is valid whenever the confidence level is 95%."
    )

    explanation <- paste0(
      "The estimated parameter and uncertainty apply to the condition represented by the data."
    )

  } else if (task_type == "confidence_not_probability") {

    output_text <- paste0(
      "Observed 95% interval = [",
      fmt(mean_ci$lower), ", ",
      fmt(mean_ci$upper), "]"
    )

    question_text <- pick(c(
      "Which probability statement is correct?",
      "How should 95% confidence be described after the interval has been calculated?",
      "Select the non-Bayesian interpretation."
    ))

    correct <- "The interval was produced by a method that captures the fixed population mean in about 95% of repeated samples."

    options <- c(
      correct,
      "There is exactly a 95% probability that the fixed population mean is inside this realised interval.",
      "There is a 95% probability that every future observation lies inside the interval.",
      "The sample mean has a 95% probability of being equal to the population mean."
    )

    explanation <- paste0(
      "In frequentist inference, the parameter is fixed and the interval procedure has long-run coverage."
    )

  } else {

    output_text <- paste0(
      "Research question: compare supplement, dose and whether the supplement difference changes by dose."
    )

    question_text <- pick(c(
      "Which follow-up method is most appropriate?",
      "What analysis would address the expanded question?",
      "Select the best next step."
    ))

    correct <- "Fit a regression or ANOVA model including supplement, dose and their interaction."

    options <- c(
      correct,
      "Construct one confidence interval after combining all groups and doses.",
      "Run many unadjusted intervals and report only the narrowest one.",
      "Ignore dose because it is not the response variable."
    )

    explanation <- paste0(
      "A joint model can estimate main effects and whether the supplement contrast changes across dose."
    )
  }

  shuffled <- shuffle_single(options, correct)

  full_question <- compose_prompt(
    context,
    output_text,
    question_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_019_%03d", i),
    blueprint_id = "R800_019",
    difficulty = "medium",
    scenario = "education",
    template_id = paste0(
      "ci_t_interval_template_",
      task_type
    ),
    language_style = style,
    presentation_layout = paste0(
      "layout_",
      layout_id
    ),
    cognitive_skill = "conceptual_interval_reasoning_and_method_selection",
    statistical_output = output_text,
    question = full_question,
    option_a = shuffled$option_a,
    option_b = shuffled$option_b,
    option_c = shuffled$option_c,
    option_d = shuffled$option_d,
    correct_answer = shuffled$correct_answer,
    reference_answer = correct,
    solution_steps = explanation,
    answer_type = "single_choice"
  )
}

# ============================================================
# R800_022
# Marketing / Medium / Multiple Choice
#
# len  -> customer response score
# supp -> campaign format
# dose -> exposure intensity
# ============================================================

marketing_styles <- c(
  "brand_tracking_report",
  "campaign_debrief",
  "consumer_insight_memo",
  "agency_pitch_review",
  "executive_email",
  "dashboard_annotation",
  "market_research_vendor",
  "creative_team_dialogue",
  "segmentation_case",
  "client_presentation_check"
)

marketing_openings <- list(

  brand_tracking_report = c(
    "A brand-tracking report uses anonymised ToothGrowth values as customer-response scores.",
    "The analytics team compares average response under two campaign formats at a fixed exposure level.",
    "A tracking study asks which confidence-interval statements can safely appear in the report."
  ),

  campaign_debrief = c(
    "A campaign debrief includes a point estimate and confidence interval for the difference between two formats.",
    "The post-campaign review asks analysts to separate valid conclusions from promotional overstatement.",
    "A media-performance summary contains several candidate interpretations."
  ),

  consumer_insight_memo = c(
    "A consumer-insight memo reports average response scores and interval estimates.",
    "The research team wants to communicate uncertainty without losing the practical message.",
    "An internal memo asks the reader to select every statement supported by the output."
  ),

  agency_pitch_review = c(
    "An agency pitch converts a statistical comparison into several possible client claims.",
    "The research director is checking which claims are defensible before the presentation.",
    "A pitch deck includes both accurate and exaggerated interpretations of the confidence interval."
  ),

  executive_email = c(
    "A marketing director emails the analytics team asking what can be concluded from the interval.",
    "Senior management wants a plain-language explanation of the campaign comparison.",
    "An executive summary needs statistically valid statements selected from four alternatives."
  ),

  dashboard_annotation = c(
    "A dashboard displays the estimated campaign-format difference and its confidence interval.",
    "Users are asked to choose all valid annotations for the chart.",
    "A marketing analytics dashboard needs labels that reflect both magnitude and uncertainty."
  ),

  market_research_vendor = c(
    "An external research vendor provides a confidence interval and several interpretive notes.",
    "The client asks which vendor statements are statistically correct.",
    "A supplier report is being checked for interval misuse."
  ),

  creative_team_dialogue = c(
    "Creative lead: \"Can we say campaign OJ always performs better?\"\nAnalyst: \"Only if the evidence supports that exact claim.\"",
    "Client: \"The point estimate is positive. Is the decision settled?\"\nResearcher: \"We still need to inspect the interval.\"",
    "Strategist: \"Why did the 99% interval get wider?\"\nStatistician: \"Higher confidence requires more uncertainty in the range.\""
  ),

  segmentation_case = c(
    "A segmentation exercise treats supplement labels as anonymised campaign groups.",
    "The marketing team uses a mean-difference interval to compare two response segments.",
    "A customer-research case asks which conclusions remain valid after uncertainty is considered."
  ),

  client_presentation_check = c(
    "A client presentation contains four statements about the same confidence interval.",
    "The presentation team asks the analyst to approve every statistically sound claim.",
    "A final slide is being checked for overclaiming and missing uncertainty."
  )
)

marketing_tasks <- c(
  "valid_parameter_statements",
  "valid_zero_inclusion_statements",
  "valid_interval_width_statements",
  "valid_confidence_level_statements",
  "valid_sample_size_statements",
  "valid_welch_statements",
  "valid_practical_threshold_statements",
  "valid_reporting_statements",
  "valid_non_significance_statements",
  "valid_overlap_statements",
  "valid_margin_statements",
  "valid_variability_statements",
  "valid_causality_statements",
  "valid_extrapolation_statements",
  "valid_follow_up_statements"
)

generate_r800_022 <- function(i) {

  task_type <- marketing_tasks[i]
  style <- pick(marketing_styles)
  context <- pick(marketing_openings[[style]])
  layout_id <- sample(1:4, 1)

  dose <- pick(sort(unique(TG$dose)))

  x <- subset(
    TG,
    dose == dose & supp == "OJ"
  )$len

  y <- subset(
    TG,
    dose == dose & supp == "VC"
  )$len

  ci95 <- welch_diff_ci(x, y, 0.95)

  if (task_type == "valid_parameter_statements") {

    output_text <- paste0(
      "95% CI for mean response difference OJ - VC = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    question_text <- "Which statements correctly identify the parameter? Select all that apply."

    options <- c(
      "The interval targets the difference between two population mean response scores.",
      "The comparison is defined as OJ minus VC.",
      "The interval targets the difference between two individual customers.",
      "The interval estimates the proportion of customers who prefer OJ."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The interval estimates a population mean contrast in the stated direction."
    )

  } else if (task_type == "valid_zero_inclusion_statements") {

    output_text <- paste0(
      "95% CI for OJ - VC = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    question_text <- "Which statements about zero inclusion are valid? Select all that apply."

    if (contains_zero(ci95$lower, ci95$upper)) {
      statement_1 <- "Zero is inside the interval, so no population mean difference remains plausible."
      statement_2 <- "The corresponding two-sided 5% test would not reject equal population means."
    } else {
      statement_1 <- "Zero is outside the interval, supporting a non-zero population mean difference."
      statement_2 <- "The corresponding two-sided 5% test would reject equal population means."
    }

    options <- c(
      statement_1,
      statement_2,
      "Zero inclusion proves that the two campaign formats are identical.",
      "Zero exclusion proves that every customer responds more strongly to one format."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Zero inclusion connects to the hypothesis test, but not to exact equality or individual guarantees."
    )

  } else if (task_type == "valid_interval_width_statements") {

    output_text <- paste0(
      "95% interval width = ", fmt(ci95$width),
      "\nSE of estimated difference = ", fmt(ci95$se)
    )

    question_text <- "Which statements about interval width are correct? Select all that apply."

    options <- c(
      "A wider interval indicates less precise estimation.",
      "A larger standard error generally produces a wider interval.",
      "A wider interval proves that the point estimate is biased.",
      "Interval width is determined only by the sample mean."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Width reflects uncertainty and depends on SE and the critical value, not only the midpoint."
    )

  } else if (task_type == "valid_confidence_level_statements") {

    ci90 <- welch_diff_ci(x, y, 0.90)
    ci99 <- welch_diff_ci(x, y, 0.99)

    output_text <- paste0(
      "90% CI width = ", fmt(ci90$width),
      "\n99% CI width = ", fmt(ci99$width)
    )

    question_text <- "Which statements about changing the confidence level are valid? Select all that apply."

    options <- c(
      "The 99% interval is wider because it uses a larger critical value.",
      "Higher confidence generally reduces precision.",
      "The sample mean changes when the confidence level changes.",
      "A 99% interval guarantees that 99% of customers fall inside it."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The data and point estimate remain fixed; the critical value and width change."
    )

  } else if (task_type == "valid_sample_size_statements") {

    output_text <- paste0(
      "Current group sizes = ", length(x),
      " and ", length(y),
      "\nAssume both sample sizes are quadrupled and variances remain similar."
    )

    question_text <- "Which statements about the larger study are correct? Select all that apply."

    options <- c(
      "The standard error would be approximately halved.",
      "The confidence interval would become narrower.",
      "The standard error would become one quarter of its current value.",
      "The confidence level would automatically increase."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "SE scales approximately with 1/sqrt(n), so quadrupling n halves SE."
    )

  } else if (task_type == "valid_welch_statements") {

    output_text <- paste0(
      "OJ variance = ", fmt(var(x)),
      "\nVC variance = ", fmt(var(y)),
      "\nWelch df = ", fmt(ci95$df)
    )

    question_text <- "Which statements about the Welch interval are correct? Select all that apply."

    options <- c(
      "It does not require equal population variances.",
      "Its degrees of freedom may be non-integer.",
      "It is selected because it always gives the narrowest interval.",
      "It is a non-parametric rank-based interval."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Welch uses an unequal-variance SE and approximate degrees of freedom."
    )

  } else if (task_type == "valid_practical_threshold_statements") {

    threshold <- pick(c(1, 2, 3))

    output_text <- paste0(
      "95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nMinimum commercially worthwhile difference = ",
      threshold
    )

    question_text <- "Which statements correctly use the commercial threshold? Select all that apply."

    if (ci95$lower > threshold) {
      threshold_statement <- "The entire interval exceeds the threshold, supporting a commercially worthwhile effect."
    } else if (ci95$upper < threshold) {
      threshold_statement <- "The interval remains below the threshold, so the required commercial effect is not supported."
    } else {
      threshold_statement <- "The interval crosses the threshold, so commercial importance remains uncertain."
    }

    options <- c(
      threshold_statement,
      "Commercial importance should be judged against the stated threshold, not only against zero.",
      "Any positive point estimate is automatically commercially important.",
      "The confidence level alone determines commercial value."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Statistical and commercial thresholds answer different questions."
    )

  } else if (task_type == "valid_reporting_statements") {

    output_text <- paste0(
      "Estimated OJ - VC difference = ",
      fmt(ci95$diff),
      "\n95% CI = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    question_text <- "Which elements belong in a strong client-facing report? Select all that apply."

    options <- c(
      "The direction and size of the estimated difference.",
      "The confidence interval and the exposure level being compared.",
      "Only the phrase 'statistically significant' with no numerical context.",
      "A guarantee that every customer responds in the same direction."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Good reporting includes magnitude, uncertainty and context while avoiding universal claims."
    )

  } else if (task_type == "valid_non_significance_statements") {

    output_text <- paste0(
      "95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    question_text <- "Which statements are appropriate if the interval includes zero? Select all that apply."

    options <- c(
      "The data do not provide clear evidence of a population mean difference at the 5% two-sided level.",
      "The interval still shows the range of effects compatible with the data.",
      "The two campaign formats have been proved exactly equal.",
      "The study contains no useful information."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Failure to exclude zero is not proof of equality, and the interval remains informative."
    )

  } else if (task_type == "valid_overlap_statements") {

    a <- one_mean_ci(x, 0.95)
    b <- one_mean_ci(y, 0.95)

    output_text <- paste0(
      "OJ mean CI = [", fmt(a$lower),
      ", ", fmt(a$upper), "]",
      "\nVC mean CI = [", fmt(b$lower),
      ", ", fmt(b$upper), "]",
      "\nDirect difference CI = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    question_text <- "Which statements about interval overlap are valid? Select all that apply."

    options <- c(
      "The direct interval for OJ - VC answers the comparison question most directly.",
      "Visual overlap of separate mean intervals is not equivalent to a formal test of the mean difference.",
      "Any overlap proves exact equality of the population means.",
      "Separate mean intervals automatically replace the need for a contrast interval."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The contrast interval directly estimates the parameter of interest."
    )

  } else if (task_type == "valid_margin_statements") {

    output_text <- paste0(
      "Point estimate = ", fmt(ci95$diff),
      "\nMargin of error = ", fmt(ci95$margin)
    )

    question_text <- "Which statements about the margin of error are correct? Select all that apply."

    options <- c(
      "The interval endpoints are point estimate minus and plus the margin.",
      "The margin combines the critical value and standard error.",
      "The margin is the largest raw observation in the sample.",
      "The margin is the probability that the campaign claim is false."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "For a symmetric t interval, margin = critical value × SE."
    )

  } else if (task_type == "valid_variability_statements") {

    output_text <- paste0(
      "Suppose two studies have the same group sizes and point estimate.",
      "\nStudy A has smaller within-group SDs than Study B."
    )

    question_text <- "Which statements are valid? Select all that apply."

    options <- c(
      "Study A would generally have the smaller standard error.",
      "Study A would generally have the narrower confidence interval.",
      "The studies must have identical interval widths because their point estimates match.",
      "Within-group variability affects only the midpoint."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Smaller variability reduces SE and therefore interval width when other factors are fixed."
    )

  } else if (task_type == "valid_causality_statements") {

    output_text <- paste0(
      "Observed 95% CI for campaign-format mean difference = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    question_text <- "Which statements about causality are valid? Select all that apply."

    options <- c(
      "The interval estimates an average group difference.",
      "A causal conclusion requires appropriate treatment assignment and control of confounding.",
      "Excluding zero automatically proves causation.",
      "A narrow interval guarantees that no hidden bias exists."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Precision and non-zero association do not by themselves establish causation."
    )

  } else if (task_type == "valid_extrapolation_statements") {

    output_text <- paste0(
      "Interval estimated at exposure level ", dose,
      "\nProposed use: apply the same interval to a much higher unobserved exposure level"
    )

    question_text <- "Which statements about extrapolation are correct? Select all that apply."

    options <- c(
      "The interval is specific to the observed exposure condition.",
      "Using it at an unobserved exposure level requires additional assumptions.",
      "A 95% confidence level makes extrapolation automatically valid.",
      "The same campaign difference must hold at every exposure level."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Condition-specific estimates should not be transferred outside the observed setting without modelling support."
    )

  } else {

    output_text <- paste0(
      "Research goal: compare both campaign format and exposure intensity, including whether the format difference changes with exposure."
    )

    question_text <- "Which follow-up approaches are reasonable? Select all that apply."

    options <- c(
      "Fit a regression or ANOVA model including format, exposure and their interaction.",
      "Use planned or adjusted comparisons after fitting the joint model.",
      "Run many unadjusted intervals and report only the most favourable one.",
      "Ignore exposure because it is not the response variable."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "A joint model can estimate main effects, interaction and controlled follow-up comparisons."
    )
  }

  shuffled <- shuffle_multiple(
    options,
    correct_flags
  )

  full_question <- compose_prompt(
    context,
    output_text,
    question_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_022_%03d", i),
    blueprint_id = "R800_022",
    difficulty = "medium",
    scenario = "marketing",
    template_id = paste0(
      "ci_t_interval_template_",
      task_type
    ),
    language_style = style,
    presentation_layout = paste0(
      "layout_",
      layout_id
    ),
    cognitive_skill = "multiple_statement_interval_reasoning",
    statistical_output = output_text,
    question = full_question,
    option_a = shuffled$option_a,
    option_b = shuffled$option_b,
    option_c = shuffled$option_c,
    option_d = shuffled$option_d,
    correct_answer = shuffled$correct_answer,
    reference_answer = paste(
      options[correct_flags],
      collapse = " | "
    ),
    solution_steps = explanation,
    answer_type = "multiple_choice"
  )
}

# ============================================================
# R800_024
# General Everyday / Easy / Single Choice
# ============================================================

everyday_styles <- c(
  "plain_explanation",
  "shopping_comparison",
  "neighbourhood_chat",
  "news_snippet",
  "household_decision",
  "simple_class_example",
  "advice_column",
  "community_notice",
  "short_dialogue",
  "basic_data_note"
)

everyday_openings <- list(

  plain_explanation = c(
    "A simple data summary reports an average and a confidence interval.",
    "Someone wants a plain-language explanation of a confidence interval.",
    "A short example uses ToothGrowth to illustrate uncertainty around an average."
  ),

  shopping_comparison = c(
    "Two everyday options are being compared using average scores.",
    "A buyer sees two average results and asks what the interval means.",
    "A comparison note includes a confidence interval for the difference between two options."
  ),

  neighbourhood_chat = c(
    "Neighbour A: \"The interval includes zero. Does that mean the options are exactly the same?\"\nNeighbour B: \"Not necessarily.\"",
    "A casual conversation turns to the meaning of a 95% confidence interval.",
    "Two people discuss whether a wider interval means more or less certainty."
  ),

  news_snippet = c(
    "A short news item reports a mean estimate with a confidence interval.",
    "A public-facing summary includes one statistical sentence that needs checking.",
    "A brief article asks readers to interpret an interval correctly."
  ),

  household_decision = c(
    "A household decision is based on an estimated average difference.",
    "Someone wants to know whether the interval rules out no difference.",
    "A practical comparison uses a confidence interval rather than a single number."
  ),

  simple_class_example = c(
    "An introductory statistics class asks one direct question about confidence intervals.",
    "A basic classroom example tests one interval concept at a time.",
    "Students choose the correct statement from four short options."
  ),

  advice_column = c(
    "An advice column explains what an interval can and cannot tell a reader.",
    "A reader asks whether 95% confidence means 95% of individual observations are covered.",
    "The response needs a simple but accurate explanation."
  ),

  community_notice = c(
    "A community notice reports an average result with uncertainty.",
    "A public information sheet includes a confidence interval.",
    "A short notice asks readers not to confuse the interval with a range of all observations."
  ),

  short_dialogue = c(
    "Reader: \"Why did the interval get wider at 99% confidence?\"\nAnalyst: \"Because higher confidence needs a larger margin.\"",
    "User: \"Does a bigger sample make the interval narrower?\"\nStatistician: \"Usually, yes.\"",
    "Reader: \"What does the margin of error do?\"\nAnalyst: \"It determines the distance from the estimate to each endpoint.\""
  ),

  basic_data_note = c(
    "The ToothGrowth data provide a numerical mean and a t-based confidence interval.",
    "A basic statistical note reports the sample size, mean and standard deviation.",
    "The following interval was computed from one treatment group."
  )
)

easy_tasks <- c(
  "identify_interval_target",
  "interpret_one_mean",
  "interpret_zero",
  "higher_confidence_wider",
  "larger_n_narrower",
  "margin_endpoint",
  "identify_welch",
  "individual_values_warning",
  "practical_threshold_basic",
  "correct_plain_report"
)

generate_r800_024 <- function(i) {

  task_type <- easy_tasks[i]
  style <- pick(everyday_styles)
  context <- pick(everyday_openings[[style]])
  layout_id <- sample(1:4, 1)

  dose <- pick(sort(unique(TG$dose)))
  supp <- pick(levels(TG$supp))

  group <- subset(
    TG,
    dose == dose & supp == supp
  )$len

  x <- subset(
    TG,
    dose == dose & supp == "OJ"
  )$len

  y <- subset(
    TG,
    dose == dose & supp == "VC"
  )$len

  mean_ci <- one_mean_ci(group, 0.95)
  diff_ci <- welch_diff_ci(x, y, 0.95)

  if (task_type == "identify_interval_target") {

    output_text <- paste0(
      "95% CI = [", fmt(mean_ci$lower),
      ", ", fmt(mean_ci$upper), "]",
      "\nBased on one treatment group"
    )

    question_text <- "What does this interval estimate?"

    correct <- "The population mean tooth length for that treatment group."

    options <- c(
      correct,
      "Every individual tooth length in the population.",
      "The percentage of observations above the sample mean.",
      "The difference between OJ and VC."
    )

    explanation <- "A one-sample mean interval estimates one population mean."

  } else if (task_type == "interpret_one_mean") {

    output_text <- paste0(
      "95% CI for the mean = [",
      fmt(mean_ci$lower), ", ",
      fmt(mean_ci$upper), "]"
    )

    question_text <- "Which interpretation is correct?"

    correct <- "The interval gives plausible values for the population mean."

    options <- c(
      correct,
      "Ninety-five percent of individual observations must lie inside it.",
      "The sample mean has a 95% probability of changing.",
      "Every future observation will fall inside it."
    )

    explanation <- "The interval concerns uncertainty about a population mean."

  } else if (task_type == "interpret_zero") {

    output_text <- paste0(
      "95% CI for OJ - VC = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]"
    )

    question_text <- "What does zero inclusion or exclusion tell us?"

    if (contains_zero(diff_ci$lower, diff_ci$upper)) {
      correct <- "Because zero is included, no population mean difference remains plausible."
    } else {
      correct <- "Because zero is excluded, the interval supports a non-zero population mean difference."
    }

    options <- c(
      correct,
      "Zero inclusion proves the two groups are exactly identical.",
      "Zero exclusion means every observation differs.",
      "Zero determines the confidence level."
    )

    explanation <- "Zero is the no-difference value for a mean contrast."

  } else if (task_type == "higher_confidence_wider") {

    output_text <- "The same sample is used to construct 90% and 99% confidence intervals."

    question_text <- "Which interval is usually wider?"

    correct <- "The 99% confidence interval."

    options <- c(
      correct,
      "The 90% confidence interval.",
      "They must always have exactly the same width.",
      "The interval with the smaller sample mean."
    )

    explanation <- "Higher confidence uses a larger critical value and margin."

  } else if (task_type == "larger_n_narrower") {

    output_text <- "Assume variability and confidence level stay the same while sample size increases."

    question_text <- "What usually happens to the confidence interval?"

    correct <- "It becomes narrower because the standard error decreases."

    options <- c(
      correct,
      "It becomes wider because more data create more uncertainty.",
      "Its midpoint becomes zero.",
      "Its confidence level automatically becomes 100%."
    )

    explanation <- "SE decreases roughly as sample size increases."

  } else if (task_type == "margin_endpoint") {

    output_text <- paste0(
      "Point estimate = ", fmt(mean_ci$mean),
      "\nMargin of error = ", fmt(mean_ci$margin)
    )

    question_text <- "How are the interval endpoints formed?"

    correct <- "Subtract and add the margin of error to the point estimate."

    options <- c(
      correct,
      "Multiply the point estimate by the margin.",
      "Add the sample size to the point estimate.",
      "Use the largest and smallest observations."
    )

    explanation <- "A symmetric t interval is estimate ± margin."

  } else if (task_type == "identify_welch") {

    output_text <- "Two independent group means are compared, and equal variances are not assumed."

    question_text <- "Which interval method is appropriate?"

    correct <- "A Welch confidence interval for the difference in means."

    options <- c(
      correct,
      "A one-sample mean interval.",
      "A paired interval for the raw observations.",
      "A confidence interval for a proportion."
    )

    explanation <- "Welch is suitable for two independent means without an equal-variance assumption."

  } else if (task_type == "individual_values_warning") {

    output_text <- paste0(
      "95% CI for the population mean = [",
      fmt(mean_ci$lower), ", ",
      fmt(mean_ci$upper), "]"
    )

    question_text <- "Which statement is false?"

    correct <- "Ninety-five percent of individual observations must lie inside this interval."

    options <- c(
      correct,
      "The interval estimates a population mean.",
      "The interval reflects uncertainty in the estimate.",
      "The sample size affects the interval width."
    )

    explanation <- "A mean confidence interval is not a range for individual observations."

  } else if (task_type == "practical_threshold_basic") {

    threshold <- pick(c(1, 2))

    output_text <- paste0(
      "95% CI for OJ - VC = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]",
      "\nUseful difference threshold = ", threshold
    )

    question_text <- "How should the threshold be used?"

    if (diff_ci$lower > threshold) {
      correct <- "The full interval exceeds the threshold, supporting a useful difference."
    } else if (diff_ci$upper < threshold) {
      correct <- "The interval does not reach the threshold, so the useful difference is not supported."
    } else {
      correct <- "The interval crosses the threshold, so usefulness remains uncertain."
    }

    options <- c(
      correct,
      "Any positive estimate automatically exceeds the threshold.",
      "Only zero can be compared with an interval.",
      "The threshold changes the sample mean."
    )

    explanation <- "Compare the whole interval with the practical threshold."

  } else {

    output_text <- paste0(
      "Estimated OJ - VC difference = ",
      fmt(diff_ci$diff),
      "\n95% CI = [",
      fmt(diff_ci$lower), ", ",
      fmt(diff_ci$upper), "]"
    )

    question_text <- "Which is the best plain-language report?"

    correct <- paste0(
      "The estimated average difference is ",
      fmt(diff_ci$diff),
      " units, with a 95% confidence interval from ",
      fmt(diff_ci$lower), " to ",
      fmt(diff_ci$upper), "."
    )

    options <- c(
      correct,
      "OJ is proven better for everyone.",
      "The population difference is exactly the sample difference.",
      "Ninety-five percent of all observations lie between the interval limits."
    )

    explanation <- "A good report gives the estimate and uncertainty without proof language."
  }

  shuffled <- shuffle_single(options, correct)

  full_question <- compose_prompt(
    context,
    output_text,
    question_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_024_%03d", i),
    blueprint_id = "R800_024",
    difficulty = "easy",
    scenario = "general_everyday",
    template_id = paste0(
      "ci_t_interval_template_",
      task_type
    ),
    language_style = style,
    presentation_layout = paste0(
      "layout_",
      layout_id
    ),
    cognitive_skill = "direct_confidence_interval_concept_recognition",
    statistical_output = output_text,
    question = full_question,
    option_a = shuffled$option_a,
    option_b = shuffled$option_b,
    option_c = shuffled$option_c,
    option_d = shuffled$option_d,
    correct_answer = shuffled$correct_answer,
    reference_answer = correct,
    solution_steps = explanation,
    answer_type = "single_choice"
  )
}

# ============================================================
# Generate all three blueprints
# ============================================================

R800_019 <- do.call(
  rbind,
  lapply(seq_len(20), generate_r800_019)
)

R800_022 <- do.call(
  rbind,
  lapply(seq_len(15), generate_r800_022)
)

R800_024 <- do.call(
  rbind,
  lapply(seq_len(10), generate_r800_024)
)

ALL <- rbind(
  R800_019,
  R800_022,
  R800_024
)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_019) == 20)
stopifnot(nrow(R800_022) == 15)
stopifnot(nrow(R800_024) == 10)
stopifnot(nrow(ALL) == 45)
stopifnot(length(unique(ALL$id)) == 45)

stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$option_a)))
stopifnot(!any(is.na(ALL$option_b)))
stopifnot(!any(is.na(ALL$option_c)))
stopifnot(!any(is.na(ALL$option_d)))
stopifnot(!any(is.na(ALL$correct_answer)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))

stopifnot(length(unique(R800_019$template_id)) == 20)
stopifnot(length(unique(R800_022$template_id)) == 15)
stopifnot(length(unique(R800_024$template_id)) == 10)

# Single Choice must contain exactly one answer letter
stopifnot(
  all(nchar(R800_019$correct_answer) == 1)
)

stopifnot(
  all(nchar(R800_024$correct_answer) == 1)
)

# Multiple Choice answer format
stopifnot(
  all(
    grepl(
      "^[A-D](,[A-D])*$",
      R800_022$correct_answer
    )
  )
)

# Difficulty separation
stopifnot(
  all(
    R800_024$cognitive_skill ==
      "direct_confidence_interval_concept_recognition"
  )
)

stopifnot(
  all(
    R800_019$cognitive_skill ==
      "conceptual_interval_reasoning_and_method_selection"
  )
)

stopifnot(
  all(
    R800_022$cognitive_skill ==
      "multiple_statement_interval_reasoning"
  )
)

# ============================================================
# Export one combined CSV and one combined JSON
# ============================================================

write.csv(
  ALL,
  "R800_019_022_024_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

jsonlite::write_json(
  ALL,
  "R800_019_022_024_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

# ============================================================
# Preview
# ============================================================

print(
  ALL[, c(
    "id",
    "blueprint_id",
    "difficulty",
    "scenario",
    "language_style",
    "template_id",
    "correct_answer"
  )]
)

cat(
  "\nGenerated successfully:\n",
  "- R800_019: 20 Education Medium Single Choice questions\n",
  "- R800_022: 15 Marketing Medium Multiple Choice questions\n",
  "- R800_024: 10 General Everyday Easy Single Choice questions\n",
  "- Combined CSV: R800_019_022_024_questions.csv\n",
  "- Combined JSON: R800_019_022_024_questions.json\n"
)
