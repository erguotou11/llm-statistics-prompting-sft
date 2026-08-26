# ============================================================
# R800_013 + R800_015
#
# R800_013
# t-test / ToothGrowth / Education / Medium / Single Choice / 20
#
# R800_015
# t-test / iris / Marketing / Medium / Multiple Choice / 15
#
# Design goals
# - Real R datasets and computed statistical output
# - Rich variation in discourse form and sentence structure
# - Medium difficulty through conceptual judgement and method selection
# - Plausible distractors based on common statistical misunderstandings
# - Single Choice: exactly one correct option
# - Multiple Choice: one or more correct options
# ============================================================

set.seed(80001315)

data(ToothGrowth)
data(iris)

TG <- ToothGrowth
IR <- iris

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

p_text <- function(p) {
  p <- as.numeric(p)

  if (is.na(p)) {
    return("NA")
  }

  if (p < 0.001) {
    return("< 0.001")
  }

  paste0("= ", fmt(p, 3))
}

pick <- function(x) {
  sample(x, size = 1)
}

welch_parts <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- var(x)
  vy <- var(y)

  se <- sqrt(vx / nx + vy / ny)
  t_value <- (mx - my) / se

  df <- (vx / nx + vy / ny)^2 /
    ((vx / nx)^2 / (nx - 1) + (vy / ny)^2 / (ny - 1))

  list(
    nx = nx,
    ny = ny,
    mx = mx,
    my = my,
    vx = vx,
    vy = vy,
    se = se,
    t = t_value,
    df = df
  )
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
    dataset_name,
    difficulty,
    scenario,
    template_id,
    language_style,
    presentation_layout,
    cognitive_skill,
    variables_used,
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
    dataset_name = dataset_name,
    statistical_concept = "t_test",
    task = "t-test",
    template_id = template_id,
    difficulty = difficulty,
    scenario = scenario,
    language_style = language_style,
    presentation_layout = presentation_layout,
    cognitive_skill = cognitive_skill,
    question_type = answer_type,
    variables_used = variables_used,
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
# R800_013
# ToothGrowth / Education / Medium / Single Choice
# ============================================================

r800_013_tasks <- c(
  "choose_test_design",
  "interpret_p_value",
  "identify_response_grouping",
  "choose_welch_over_pooled",
  "interpret_confidence_interval",
  "interpret_non_significance",
  "identify_one_sided_condition",
  "recognise_independence_issue",
  "select_valid_conclusion",
  "interpret_effect_size",
  "identify_multiple_testing_problem",
  "choose_paired_or_independent",
  "interpret_t_sign",
  "identify_null_hypothesis",
  "evaluate_causal_claim",
  "choose_summary_for_report",
  "interpret_standard_error",
  "recognise_subgroup_issue",
  "select_follow_up_method",
  "identify_reporting_error"
)

r800_013_styles <- c(
  "exam_feedback",
  "teaching_demo",
  "student_consultation",
  "marking_comment",
  "seminar_discussion",
  "coursework_vignette",
  "tutorial_dialogue",
  "assessment_review",
  "methods_workshop",
  "lecture_poll"
)

r800_013_openings <- list(

  exam_feedback = c(
    "An instructor is reviewing a student's answer to a t-test question based on the ToothGrowth dataset.",
    "A marked script contains a numerical result but the interpretation needs checking.",
    "An examination feedback session focuses on whether the student chose and interpreted the test correctly."
  ),

  teaching_demo = c(
    "A lecturer uses the ToothGrowth data to demonstrate how method choice depends on the structure of the comparison.",
    "A classroom demonstration contrasts several possible analyses before selecting the appropriate t-test.",
    "A teaching example asks students to connect the statistical question with the correct test."
  ),

  student_consultation = c(
    "A student brings a ToothGrowth analysis to office hours and asks which conclusion is defensible.",
    "During a consultation, the tutor notices that the student is mixing up group means, p-values and causal claims.",
    "A student wants help deciding how to compare two treatment conditions."
  ),

  marking_comment = c(
    "A marker has written, 'The calculation is fine, but the reasoning is incomplete.'",
    "The assessment comment asks the student to distinguish what the output shows from what it cannot show.",
    "A marking note highlights a common misunderstanding about t-tests."
  ),

  seminar_discussion = c(
    "A seminar group debates whether the reported result is enough to support a broad treatment claim.",
    "Students compare several interpretations of the same ToothGrowth output.",
    "A discussion question asks which statement best matches the evidence."
  ),

  coursework_vignette = c(
    "A coursework vignette presents a short ToothGrowth analysis and four possible conclusions.",
    "A methods assignment asks students to identify the most appropriate statistical reasoning.",
    "The question is designed to test method selection rather than arithmetic."
  ),

  tutorial_dialogue = c(
    "Tutor: \"You have the output. Now decide what it actually means.\"\nStudent: \"So I should not just repeat the p-value?\"",
    "Student: \"The means differ, so one treatment must cause the increase.\"\nTutor: \"That conclusion needs a closer look.\"",
    "Tutor: \"Before choosing a test, tell me whether the observations are paired or independent.\""
  ),

  assessment_review = c(
    "A programme team is checking whether this item tests genuine statistical understanding.",
    "The assessment review panel wants a question that distinguishes careful reasoning from memorised rules.",
    "A revised exam item focuses on choosing the best interpretation of real output."
  ),

  methods_workshop = c(
    "A research methods workshop uses the ToothGrowth dataset to practise selecting between related t-test procedures.",
    "Participants are asked to identify the assumption or design feature that determines the correct method.",
    "A workshop exercise presents several plausible but not equally valid approaches."
  ),

  lecture_poll = c(
    "A live lecture poll asks students to choose the most defensible statement.",
    "The lecturer presents four responses and asks the class to vote.",
    "A concept-check question uses the ToothGrowth output to test statistical judgement."
  )
)

generate_r800_013 <- function(i) {

  task_type <- r800_013_tasks[i]
  style <- pick(r800_013_styles)
  context <- pick(r800_013_openings[[style]])
  layout_id <- sample(1:4, 1)

  dose <- pick(sort(unique(TG$dose)))
  x <- subset(TG, dose == dose & supp == "OJ")$len
  y <- subset(TG, dose == dose & supp == "VC")$len

  tst <- t.test(x, y, var.equal = FALSE)
  parts <- welch_parts(x, y)
  diff <- mean(x) - mean(y)
  ci <- tst$conf.int

  if (task_type == "choose_test_design") {

    output_text <- paste0(
      "Question: compare mean len between OJ and VC at dose ",
      dose,
      "\nGroups are formed by supplement type.",
      "\nEach observation belongs to one supplement group."
    )

    question_text <- pick(c(
      "Which method is most appropriate?",
      "Which test best matches this design?",
      "Select the analysis that directly addresses the stated comparison."
    ))

    correct <- "An independent two-sample t-test, because the response is numerical and the two supplement groups contain different observations."

    options <- c(
      correct,
      "A paired t-test, because both groups were measured in the same dataset.",
      "A one-sample t-test, because only one response variable is analysed.",
      "A chi-squared test, because supplement type has two categories."
    )

    explanation <- paste0(
      "len is numerical and supp defines two independent groups. ",
      "The observations are not naturally paired, so an independent two-sample t-test is appropriate."
    )

  } else if (task_type == "interpret_p_value") {

    output_text <- paste0(
      "Dose = ", dose,
      "\nWelch t = ", fmt(tst$statistic),
      "\nTwo-sided p-value ", p_text(tst$p.value)
    )

    question_text <- pick(c(
      "Which interpretation of the p-value is correct?",
      "What does this p-value mean under the null hypothesis?",
      "Choose the statistically accurate explanation."
    ))

    correct <- "Assuming the population means are equal, the p-value is the probability of obtaining a t statistic at least as extreme as the observed one."

    options <- c(
      correct,
      "It is the probability that the null hypothesis is true.",
      "It is the percentage of observations incorrectly measured.",
      "It is the probability that OJ causes greater tooth growth than VC."
    )

    explanation <- paste0(
      "A p-value is computed under the null model. ",
      "It is not the probability that the null hypothesis is true and does not itself establish causation."
    )

  } else if (task_type == "identify_response_grouping") {

    output_text <- paste0(
      "Variables available: len, supp and dose.",
      "\nPlanned comparison: OJ versus VC at a fixed dose."
    )

    question_text <- pick(c(
      "Which variable roles are correct for this t-test?",
      "How should the variables be assigned in the analysis?",
      "Which description correctly identifies the response and grouping variable?"
    ))

    correct <- "len is the numerical response, supp is the grouping variable, and dose is held fixed for the comparison."

    options <- c(
      correct,
      "supp is the numerical response and len defines the two groups.",
      "dose is the response and len is the categorical predictor.",
      "len, supp and dose should all be treated as three independent samples."
    )

    explanation <- paste0(
      "The test compares mean len between supplement groups at one dose. ",
      "Therefore len is the continuous outcome, supp defines groups and dose is controlled by subsetting."
    )

  } else if (task_type == "choose_welch_over_pooled") {

    output_text <- paste0(
      "OJ SD = ", fmt(sd(x)),
      "\nVC SD = ", fmt(sd(y)),
      "\nGroup sizes = ", length(x), " and ", length(y)
    )

    question_text <- pick(c(
      "Why is Welch's test a sensible default here?",
      "Which justification for using Welch's method is strongest?",
      "Select the best reason for preferring the unequal-variance procedure."
    ))

    correct <- "Welch's test does not require equal population variances and remains reliable when the group variances differ."

    options <- c(
      correct,
      "Welch's test is always chosen because it produces the smallest p-value.",
      "Welch's test is required whenever the sample means are unequal.",
      "Welch's test converts the numerical response into ranks."
    )

    explanation <- paste0(
      "Welch's method adjusts the standard error and degrees of freedom when equal variances are uncertain. ",
      "Method choice should not depend on which p-value is more favourable."
    )

  } else if (task_type == "interpret_confidence_interval") {

    output_text <- paste0(
      "Estimated difference (OJ - VC) = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )

    question_text <- pick(c(
      "Which statement best interprets this interval?",
      "Choose the most accurate conclusion from the confidence interval.",
      "How should the interval be reported?"
    ))

    if (ci[1] > 0) {
      correct <- "The interval contains only positive values, supporting a higher population mean for OJ at this dose."
    } else if (ci[2] < 0) {
      correct <- "The interval contains only negative values, supporting a lower population mean for OJ at this dose."
    } else {
      correct <- "The interval includes zero, so no population mean difference remains compatible with the data."
    }

    options <- c(
      correct,
      "Ninety-five percent of individual tooth lengths must fall inside this interval.",
      "The interval proves that the observed sample difference is exactly the population difference.",
      "The interval gives the range of possible p-values for the test."
    )

    explanation <- paste0(
      "The confidence interval concerns the population mean difference, not individual observations. ",
      "Whether zero is included determines consistency with no mean difference."
    )

  } else if (task_type == "interpret_non_significance") {

    # Choose the dose with the largest p-value for coherence.
    doses <- sort(unique(TG$dose))
    pvals <- sapply(
      doses,
      function(z) {
        a <- subset(TG, dose == z & supp == "OJ")$len
        b <- subset(TG, dose == z & supp == "VC")$len
        t.test(a, b)$p.value
      }
    )

    chosen <- doses[which.max(pvals)]
    a <- subset(TG, dose == chosen & supp == "OJ")$len
    b <- subset(TG, dose == chosen & supp == "VC")$len
    tt <- t.test(a, b)

    output_text <- paste0(
      "Dose = ", chosen,
      "\nMean difference = ", fmt(mean(a) - mean(b)),
      "\n95% CI = [", fmt(tt$conf.int[1]), ", ",
      fmt(tt$conf.int[2]), "]",
      "\np-value ", p_text(tt$p.value)
    )

    question_text <- pick(c(
      "Which conclusion is most appropriate?",
      "How should a non-significant result be described?",
      "Select the statement that avoids claiming equality."
    ))

    correct <- "The data do not provide sufficient evidence of a population mean difference at the chosen level, but exact equality has not been proved."

    options <- c(
      correct,
      "The two population means are known to be exactly equal.",
      "The study proves that supplement type has no effect under any dose.",
      "The result must be ignored because non-significant results contain no information."
    )

    explanation <- paste0(
      "Failure to reject does not prove the null hypothesis. ",
      "The confidence interval describes the range of effects still compatible with the data."
    )

  } else if (task_type == "identify_one_sided_condition") {

    output_text <- paste0(
      "Proposed alternative: mean(OJ) > mean(VC)",
      "\nThe direction was discussed after the data were inspected."
    )

    question_text <- pick(c(
      "When would a one-sided test be defensible?",
      "Which condition is required before using a one-sided alternative?",
      "Choose the valid justification for a one-sided test."
    ))

    correct <- "The directional hypothesis must be specified before examining the data, and an effect in the opposite direction must not support the same claim."

    options <- c(
      correct,
      "A one-sided test is valid whenever the observed mean difference is positive.",
      "A one-sided test should be selected if it gives significance when the two-sided test does not.",
      "A one-sided test is required whenever only two groups are compared."
    )

    explanation <- paste0(
      "Directional testing must be pre-specified. ",
      "Choosing the direction after seeing the data inflates the false-positive risk."
    )

  } else if (task_type == "recognise_independence_issue") {

    output_text <- paste0(
      "Suppose the same experimental subject had been measured once under OJ and once under VC."
    )

    question_text <- pick(c(
      "Which analysis would then be most appropriate?",
      "How would the repeated-measurement design change the method?",
      "Select the test that respects the dependence structure."
    ))

    correct <- "A paired t-test, because the two observations would be linked within the same subject."

    options <- c(
      correct,
      "An independent two-sample t-test, because there are still two treatment labels.",
      "A one-sample t-test on all observations without forming differences.",
      "A chi-squared test, because repeated measurements are categorical."
    )

    explanation <- paste0(
      "When each subject contributes both measurements, the data are paired. ",
      "The analysis should use within-subject differences."
    )

  } else if (task_type == "select_valid_conclusion") {

    output_text <- paste0(
      "Dose = ", dose,
      "\nMean(OJ) = ", fmt(mean(x)),
      "\nMean(VC) = ", fmt(mean(y)),
      "\np-value ", p_text(tst$p.value)
    )

    question_text <- pick(c(
      "Which conclusion is best supported?",
      "Select the most defensible results statement.",
      "Which wording matches the statistical evidence?"
    ))

    if (tst$p.value < 0.05) {
      correct <- paste0(
        "At dose ", dose,
        ", the data provide evidence of a difference in mean tooth length between OJ and VC."
      )
    } else {
      correct <- paste0(
        "At dose ", dose,
        ", the data do not provide sufficient evidence of a difference in mean tooth length between OJ and VC."
      )
    }

    options <- c(
      correct,
      "OJ is guaranteed to produce better growth for every subject.",
      "The p-value proves that supplement type is the only factor affecting tooth length.",
      "The two sample means are population parameters and therefore contain no uncertainty."
    )

    explanation <- paste0(
      "A t-test supports a statement about population means under the study conditions. ",
      "It does not support individual guarantees or exclusive causal explanations."
    )

  } else if (task_type == "interpret_effect_size") {

    d <- (mean(x) - mean(y)) /
      sqrt(
        ((length(x) - 1) * var(x) +
           (length(y) - 1) * var(y)) /
          (length(x) + length(y) - 2)
      )

    output_text <- paste0(
      "Mean difference (OJ - VC) = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\np-value ", p_text(tst$p.value)
    )

    question_text <- pick(c(
      "Which statement correctly distinguishes Cohen's d from the p-value?",
      "How should these two quantities be interpreted together?",
      "Select the accurate description of effect size and significance."
    ))

    correct <- "Cohen's d describes the difference in pooled-SD units, while the p-value describes evidence against equal population means under the null model."

    options <- c(
      correct,
      "Cohen's d is the probability that the alternative hypothesis is true.",
      "The p-value measures the practical size of the treatment difference.",
      "Cohen's d and the p-value are two names for the same quantity."
    )

    explanation <- paste0(
      "Effect size and statistical evidence answer different questions. ",
      "Cohen's d concerns magnitude; the p-value concerns compatibility with the null."
    )

  } else if (task_type == "identify_multiple_testing_problem") {

    output_text <- paste0(
      "Separate OJ-versus-VC tests are planned at doses 0.5, 1 and 2.",
      "\nEach test would use alpha = 0.05."
    )

    question_text <- pick(c(
      "What is the main statistical concern?",
      "Which issue should be addressed before interpreting all three tests?",
      "Select the best description of the problem."
    ))

    correct <- "Running several unadjusted tests increases the family-wise chance of at least one false positive."

    options <- c(
      correct,
      "Running several tests guarantees that all p-values become larger than 0.05.",
      "Multiple testing makes the sample means biased.",
      "Three tests require replacing t-tests with chi-squared tests."
    )

    explanation <- paste0(
      "Each test offers another opportunity for a Type I error. ",
      "Adjustment or a joint model may be needed."
    )

  } else if (task_type == "choose_paired_or_independent") {

    output_text <- paste0(
      "Design A: different subjects receive OJ and VC.",
      "\nDesign B: each subject receives both treatments at different times."
    )

    question_text <- pick(c(
      "Which mapping between design and test is correct?",
      "Choose the correct analysis for each design.",
      "Which statement properly distinguishes independent and paired data?"
    ))

    correct <- "Design A uses an independent two-sample t-test; Design B uses a paired t-test."

    options <- c(
      correct,
      "Both designs use a paired t-test because the same response variable is measured.",
      "Both designs use an independent t-test because there are two treatments.",
      "Design A uses a one-sample test and Design B uses a chi-squared test."
    )

    explanation <- paste0(
      "The test depends on whether measurements are linked within observational units, not merely on the number of treatments."
    )

  } else if (task_type == "interpret_t_sign") {

    output_text <- paste0(
      "Difference is defined as OJ - VC.",
      "\nObserved t = ", fmt(tst$statistic)
    )

    question_text <- pick(c(
      "What does the sign of t indicate?",
      "How should the direction of the test statistic be interpreted?",
      "Choose the correct explanation of a positive or negative t value."
    ))

    if (unname(tst$statistic) > 0) {
      correct <- "The positive t value indicates that the observed OJ mean is higher than the observed VC mean."
    } else {
      correct <- "The negative t value indicates that the observed OJ mean is lower than the observed VC mean."
    }

    options <- c(
      correct,
      "The sign of t indicates whether the p-value is valid.",
      "A negative t value means some tooth lengths are negative.",
      "The sign of t determines whether the test is one-sided or two-sided."
    )

    explanation <- paste0(
      "The standard error is positive, so t has the same sign as the defined mean difference."
    )

  } else if (task_type == "identify_null_hypothesis") {

    output_text <- paste0(
      "Comparison: mean len for OJ versus mean len for VC at dose ",
      dose
    )

    question_text <- pick(c(
      "Which null hypothesis is correct?",
      "What hypothesis does the two-sample t-test evaluate?",
      "Select the appropriate null statement."
    ))

    correct <- "H0: the population mean tooth lengths are equal for OJ and VC at this dose."

    options <- c(
      correct,
      "H0: every OJ observation equals every VC observation.",
      "H0: the sample means must be exactly equal.",
      "H0: supplement type and dose are both numerical variables."
    )

    explanation <- paste0(
      "The test concerns equality of population means, not equality of all individual observations or sample means."
    )

  } else if (task_type == "evaluate_causal_claim") {

    output_text <- paste0(
      "Observed mean difference = ", fmt(diff),
      "\np-value ", p_text(tst$p.value)
    )

    question_text <- pick(c(
      "Which statement about causation is most accurate?",
      "Can the t-test output alone establish a causal effect?",
      "Choose the conclusion that respects the limits of the design."
    ))

    correct <- "A causal conclusion requires information about treatment assignment and control of confounding; the t-test output alone is not enough."

    options <- c(
      correct,
      "Any p-value below 0.05 proves causation.",
      "A difference in sample means automatically establishes a treatment effect for every subject.",
      "Causation is established whenever Welch's test is used."
    )

    explanation <- paste0(
      "Statistical association and causal identification are different. ",
      "Causal interpretation depends on design and assumptions."
    )

  } else if (task_type == "choose_summary_for_report") {

    output_text <- paste0(
      "Available quantities: group means, mean difference, confidence interval, t statistic, p-value."
    )

    question_text <- pick(c(
      "Which combination gives the most informative concise report?",
      "What should a strong results sentence include?",
      "Select the best reporting practice."
    ))

    correct <- "Report the group means, estimated difference, confidence interval, test method and p-value."

    options <- c(
      correct,
      "Report only whether p is below 0.05.",
      "Report only the larger sample mean.",
      "Report only the t statistic without group labels."
    )

    explanation <- paste0(
      "A good report includes magnitude, uncertainty, method and evidence, not a threshold alone."
    )

  } else if (task_type == "interpret_standard_error") {

    output_text <- paste0(
      "Estimated difference = ", fmt(diff),
      "\nStandard error = ", fmt(parts$se)
    )

    question_text <- pick(c(
      "Which interpretation of the standard error is correct?",
      "What does the SE describe in this comparison?",
      "Choose the accurate explanation."
    ))

    correct <- "The standard error describes the sampling variability of the estimated difference in group means."

    options <- c(
      correct,
      "The standard error is the average measurement error in each tooth length.",
      "The standard error is the percentage of subjects assigned to the wrong group.",
      "The standard error is the population mean difference."
    )

    explanation <- paste0(
      "SE measures uncertainty in the estimator, not raw measurement error or effect size."
    )

  } else if (task_type == "recognise_subgroup_issue") {

    output_text <- paste0(
      "Overall comparison mixes observations from doses 0.5, 1 and 2.",
      "\nDose is strongly related to tooth length."
    )

    question_text <- pick(c(
      "Why may the overall supplement comparison be misleading?",
      "Which issue arises when dose is ignored?",
      "Select the best explanation."
    ))

    correct <- "Combining doses can obscure or distort supplement differences because dose is an important stratifying variable."

    options <- c(
      correct,
      "Combining doses is invalid because t-tests cannot use more than one numerical variable.",
      "Dose must be ignored because it is not the response variable.",
      "An overall comparison automatically adjusts for dose."
    )

    explanation <- paste0(
      "If dose affects the response, a pooled comparison may confound or hide subgroup patterns."
    )

  } else if (task_type == "select_follow_up_method") {

    output_text <- paste0(
      "The researcher wants to study supplement, dose and whether the supplement difference changes across dose."
    )

    question_text <- pick(c(
      "Which follow-up analysis is most suitable?",
      "What model would address the expanded research question?",
      "Select the method that can evaluate both main effects and interaction."
    ))

    correct <- "A regression or ANOVA model including supplement, dose and a supplement-by-dose interaction."

    options <- c(
      correct,
      "Three unrelated one-sample t-tests with no adjustment.",
      "A chi-squared test of supplement labels only.",
      "A paired t-test after combining all doses."
    )

    explanation <- paste0(
      "A joint model can estimate supplement and dose effects and test whether the supplement contrast varies by dose."
    )

  } else {

    output_text <- paste0(
      "Draft statement: \"Because p ", p_text(tst$p.value),
      ", OJ is proven better than VC.\""
    )

    question_text <- pick(c(
      "What is the main reporting error?",
      "Which criticism is most appropriate?",
      "Why should this sentence be revised?"
    ))

    correct <- "The statement overstates the evidence by using 'proven better' and omits the dose, effect size and uncertainty."

    options <- c(
      correct,
      "The statement is incorrect only because p-values should never be reported.",
      "The statement is wrong because sample means cannot be compared.",
      "The statement is valid whenever the p-value is below 0.10."
    )

    explanation <- paste0(
      "Results should report the specific comparison, magnitude and uncertainty, while avoiding absolute proof language."
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
    id = sprintf("R800_013_%03d", i),
    blueprint_id = "R800_013",
    dataset_name = "ToothGrowth",
    difficulty = "medium",
    scenario = "education",
    template_id = paste0("t_test_template_", task_type),
    language_style = style,
    presentation_layout = paste0("layout_", layout_id),
    cognitive_skill = "conceptual_reasoning_and_method_selection",
    variables_used = "len, supp, dose",
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
# R800_015
# iris / Marketing / Medium / Multiple Choice
# ============================================================

r800_015_tasks <- c(
  "valid_variable_roles",
  "valid_mean_difference_interpretations",
  "valid_p_value_statements",
  "valid_confidence_interval_statements",
  "valid_method_choices",
  "valid_non_significant_statements",
  "valid_effect_size_statements",
  "valid_marketing_claims",
  "valid_assumption_checks",
  "valid_welch_statements",
  "valid_multiple_testing_responses",
  "valid_reporting_elements",
  "valid_group_overlap_statements",
  "valid_causality_statements",
  "valid_follow_up_analyses"
)

r800_015_styles <- c(
  "brand_research_brief",
  "campaign_review",
  "consumer_insight_memo",
  "agency_pitch",
  "product_positioning_case",
  "market_testing_report",
  "executive_email",
  "creative_team_dialogue",
  "research_vendor_note",
  "dashboard_annotation"
)

r800_015_openings <- list(

  brand_research_brief = c(
    "A brand research team uses iris measurements as a neutral training dataset for learning how to compare customer segments.",
    "A market-research workshop treats iris species as stand-ins for three consumer groups.",
    "A brand analytics brief asks which statistical statements could safely appear in a segment-comparison report."
  ),

  campaign_review = c(
    "A campaign review includes several claims derived from a two-group t-test.",
    "The marketing team is checking whether the proposed interpretation overstates the evidence.",
    "A campaign report presents a group comparison and asks which conclusions are defensible."
  ),

  consumer_insight_memo = c(
    "A consumer-insight memo uses plant measurements to practise interpreting differences between market segments.",
    "The insight team wants to separate valid statistical statements from common reporting errors.",
    "An internal memo asks analysts to identify all conclusions supported by the output."
  ),

  agency_pitch = c(
    "An agency pitch turns a statistical comparison into a set of possible advertising claims.",
    "The client asks which claims are accurate enough to survive a methods review.",
    "A pitch deck contains several interpretations of the same group comparison."
  ),

  product_positioning_case = c(
    "A product-positioning case treats species labels as anonymised customer segments.",
    "The marketing strategy team is deciding how strongly to describe observed group differences.",
    "A segmentation exercise asks which conclusions remain valid after uncertainty is considered."
  ),

  market_testing_report = c(
    "A market-testing report includes means, a confidence interval and a p-value.",
    "The analytics unit reviews a test output before it enters a client report.",
    "A research summary asks the reader to select all statistically valid statements."
  ),

  executive_email = c(
    "An executive email asks for a plain-language explanation of the statistical output.",
    "A marketing director wants to know which conclusions can be stated without qualification.",
    "Senior management has highlighted several claims and asks the analytics team to approve or reject each one."
  ),

  creative_team_dialogue = c(
    "Creative lead: \"Can we say this segment is always larger?\"\nAnalyst: \"Only if the statistics support that exact claim.\"",
    "Strategist: \"The p-value is tiny, so every observation must differ.\"\nResearcher: \"That is not what the test shows.\"",
    "Client: \"Which statements can go into the presentation?\"\nConsultant: \"Let us evaluate each one.\""
  ),

  research_vendor_note = c(
    "A research vendor provides a short methodological note alongside the iris comparison.",
    "The external research team flags several statements for statistical review.",
    "A vendor report asks the client to distinguish evidence, magnitude and causation."
  ),

  dashboard_annotation = c(
    "A dashboard shows a group difference and invites users to select all valid annotations.",
    "The marketing analytics dashboard displays a t-test summary with several candidate explanations.",
    "A segment-comparison graphic needs statistically accurate labels."
  )
)

generate_r800_015 <- function(i) {

  task_type <- r800_015_tasks[i]
  style <- pick(r800_015_styles)
  context <- pick(r800_015_openings[[style]])
  layout_id <- sample(1:4, 1)

  trait <- pick(c("Sepal.Length", "Petal.Length"))
  species_pair <- sample(levels(IR$Species), 2, replace = FALSE)

  x <- IR[IR$Species == species_pair[1], trait]
  y <- IR[IR$Species == species_pair[2], trait]

  tst <- t.test(x, y)
  parts <- welch_parts(x, y)
  diff <- mean(x) - mean(y)
  ci <- tst$conf.int

  d <- (mean(x) - mean(y)) /
    sqrt(
      ((length(x) - 1) * var(x) +
         (length(y) - 1) * var(y)) /
        (length(x) + length(y) - 2)
    )

  if (task_type == "valid_variable_roles") {

    output_text <- paste0(
      "Response under study: ", trait,
      "\nCompared groups: ", species_pair[1],
      " and ", species_pair[2]
    )

    question_text <- "Which statements correctly describe the variable roles? Select all that apply."

    options <- c(
      paste0(trait, " is the numerical response variable."),
      "Species is the categorical grouping variable.",
      "Species is the numerical response being averaged.",
      paste0(trait, " defines the two categorical groups.")
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The t-test compares the mean of the numerical trait across two species groups."
    )

  } else if (task_type == "valid_mean_difference_interpretations") {

    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      "\nDifference (first - second) = ", fmt(diff)
    )

    question_text <- "Which interpretations of the mean difference are valid? Select all that apply."

    if (diff > 0) {
      valid_direction <- paste0(
        species_pair[1], " has the higher sample mean."
      )
    } else {
      valid_direction <- paste0(
        species_pair[2], " has the higher sample mean."
      )
    }

    options <- c(
      valid_direction,
      paste0(
        "The two sample means differ by ",
        fmt(abs(diff)), " units."
      ),
      "Every observation in the higher-mean group exceeds every observation in the other group.",
      "The mean difference proves that group membership causes the trait value."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "A mean difference describes group averages, not perfect individual separation or causation."
    )

  } else if (task_type == "valid_p_value_statements") {

    output_text <- paste0(
      "Welch t = ", fmt(tst$statistic),
      "\np-value ", p_text(tst$p.value)
    )

    question_text <- "Which statements about the p-value are correct? Select all that apply."

    options <- c(
      "The p-value is calculated under the null hypothesis of equal population means.",
      "A small p-value indicates that the observed result would be unusual if the null hypothesis were true.",
      "The p-value is the probability that the null hypothesis is true.",
      "The p-value measures the practical size of the group difference."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The p-value concerns compatibility with the null model. ",
      "It is neither a posterior probability nor an effect-size measure."
    )

  } else if (task_type == "valid_confidence_interval_statements") {

    output_text <- paste0(
      "95% CI for ", species_pair[1], " - ",
      species_pair[2], " = [",
      fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )

    question_text <- "Which statements correctly interpret this confidence interval? Select all that apply."

    if (ci[1] > 0) {
      directional_statement <- paste0(
        "The interval supports a higher population mean for ",
        species_pair[1], "."
      )
    } else if (ci[2] < 0) {
      directional_statement <- paste0(
        "The interval supports a lower population mean for ",
        species_pair[1], "."
      )
    } else {
      directional_statement <- "The interval includes zero, so no population mean difference remains plausible."
    }

    options <- c(
      directional_statement,
      "The interval describes uncertainty around the population mean difference.",
      "Ninety-five percent of individual observations must lie inside this interval.",
      "The confidence interval is a range of possible sample means already observed."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The interval concerns uncertainty in the population mean difference, not individual observations."
    )

  } else if (task_type == "valid_method_choices") {

    output_text <- paste0(
      "Goal: compare mean ", trait,
      " between ", species_pair[1],
      " and ", species_pair[2], "."
    )

    question_text <- "Which methods or design descriptions are appropriate? Select all that apply."

    options <- c(
      "An independent two-sample t-test is appropriate for comparing the two group means.",
      "Welch's version is reasonable when equal variances are uncertain.",
      "A paired t-test is required merely because both groups appear in the same dataset.",
      "A chi-squared test is appropriate because the response is numerical."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The response is continuous and the species groups are independent. ",
      "Welch's method avoids requiring equal population variances."
    )

  } else if (task_type == "valid_non_significant_statements") {

    # Find the least significant available comparison.
    combos <- expand.grid(
      trait = c("Sepal.Length", "Petal.Length"),
      a = levels(IR$Species),
      b = levels(IR$Species),
      stringsAsFactors = FALSE
    )
    combos <- combos[combos$a < combos$b, ]

    pvals <- mapply(
      function(tr, a, b) {
        xa <- IR[IR$Species == a, tr]
        xb <- IR[IR$Species == b, tr]
        t.test(xa, xb)$p.value
      },
      combos$trait,
      combos$a,
      combos$b
    )

    k <- which.max(pvals)
    tr2 <- combos$trait[k]
    a2 <- combos$a[k]
    b2 <- combos$b[k]

    xa <- IR[IR$Species == a2, tr2]
    xb <- IR[IR$Species == b2, tr2]
    tt2 <- t.test(xa, xb)

    output_text <- paste0(
      "Trait = ", tr2,
      "\nGroups = ", a2, " and ", b2,
      "\nMean difference = ", fmt(mean(xa) - mean(xb)),
      "\n95% CI = [", fmt(tt2$conf.int[1]), ", ",
      fmt(tt2$conf.int[2]), "]",
      "\np-value ", p_text(tt2$p.value)
    )

    question_text <- "Which statements are appropriate for a non-significant result? Select all that apply."

    options <- c(
      "The data do not provide sufficient evidence of a population mean difference at the chosen level.",
      "The confidence interval shows the range of effects still compatible with the data.",
      "The two population means have been proved exactly equal.",
      "A non-significant result contains no useful information."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Failure to reject is not proof of equality. ",
      "The interval remains informative about uncertainty."
    )

  } else if (task_type == "valid_effect_size_statements") {

    output_text <- paste0(
      "Mean difference = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\np-value ", p_text(tst$p.value)
    )

    question_text <- "Which statements correctly distinguish effect size from significance? Select all that apply."

    options <- c(
      "Cohen's d expresses the mean difference in pooled-standard-deviation units.",
      "The p-value and Cohen's d answer different questions.",
      "A small p-value automatically means the effect is large in practical terms.",
      "Cohen's d is the probability that the alternative hypothesis is true."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Cohen's d concerns magnitude, while the p-value concerns evidence under the null."
    )

  } else if (task_type == "valid_marketing_claims") {

    output_text <- paste0(
      "Observed group difference in ", trait,
      " = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )

    question_text <- "Which marketing statements are statistically defensible? Select all that apply."

    options <- c(
      paste0(
        "The two groups differ in average ",
        trait, " in this dataset."
      ),
      "The estimate should be reported with its confidence interval.",
      "Every member of the higher-mean group has a larger value than every member of the other group.",
      "The t-test proves that group identity causes the observed trait difference."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The analysis supports a group-average comparison with uncertainty, not universal individual separation or causation."
    )

  } else if (task_type == "valid_assumption_checks") {

    output_text <- paste0(
      species_pair[1], ": n = ", length(x),
      ", SD = ", fmt(sd(x)),
      "\n", species_pair[2], ": n = ", length(y),
      ", SD = ", fmt(sd(y))
    )

    question_text <- "Which checks are relevant before relying on the t-test? Select all that apply."

    options <- c(
      "Check whether observations are independent within and between groups.",
      "Check for severe outliers or strongly irregular distributions.",
      "Require the two sample means to be identical before running the test.",
      "Convert the numerical response into categories before analysis."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Independence and severe distributional problems matter. ",
      "Equal sample means are not an assumption."
    )

  } else if (task_type == "valid_welch_statements") {

    output_text <- paste0(
      "Welch t = ", fmt(tst$statistic),
      "\nWelch df = ", fmt(tst$parameter),
      "\nGroup variances = ", fmt(var(x)),
      " and ", fmt(var(y))
    )

    question_text <- "Which statements about Welch's test are correct? Select all that apply."

    options <- c(
      "Welch's test does not require equal population variances.",
      "Welch degrees of freedom may be non-integer.",
      "Welch's test is chosen because it always produces the smallest p-value.",
      "Welch's test is a non-parametric rank test."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Welch adjusts the standard error and uses an approximate degrees of freedom formula."
    )

  } else if (task_type == "valid_multiple_testing_responses") {

    output_text <- paste0(
      "Several species pairs are tested across both Sepal.Length and Petal.Length.",
      "\nEach test initially uses alpha = 0.05."
    )

    question_text <- "Which responses to the multiple-testing problem are valid? Select all that apply."

    options <- c(
      "Use a multiplicity adjustment such as Holm or Bonferroni.",
      "Use a broader model followed by planned comparisons.",
      "Ignore multiplicity because each t-test is individually valid.",
      "Choose only the smallest p-value and report it as if it were pre-specified."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Multiple tests increase the chance of at least one false positive. ",
      "Adjustment or planned modelling addresses this."
    )

  } else if (task_type == "valid_reporting_elements") {

    output_text <- paste0(
      "Available output: group means, mean difference, confidence interval, t, df, p and Cohen's d."
    )

    question_text <- "Which elements belong in a strong concise report? Select all that apply."

    options <- c(
      "The group means and direction of the difference.",
      "A confidence interval and p-value.",
      "Only the word 'significant' without numerical context.",
      "A claim that the higher sample mean guarantees better performance for every individual."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Good reporting includes magnitude and uncertainty and avoids threshold-only or universal claims."
    )

  } else if (task_type == "valid_group_overlap_statements") {

    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      ", SD = ", fmt(sd(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      ", SD = ", fmt(sd(y))
    )

    question_text <- "Which statements about group overlap are valid? Select all that apply."

    options <- c(
      "Different group means can coexist with overlapping individual observations.",
      "The SDs provide information about within-group spread.",
      "A statistically significant mean difference implies perfect classification.",
      "If the means differ, the two distributions cannot overlap."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "Means describe centres and SDs describe spread. ",
      "Mean differences do not imply perfect separation."
    )

  } else if (task_type == "valid_causality_statements") {

    output_text <- paste0(
      "Observed association between Species and ", trait,
      "\np-value ", p_text(tst$p.value)
    )

    question_text <- "Which statements about causality are correct? Select all that apply."

    options <- c(
      "The t-test supports a comparison of population means.",
      "Causal interpretation requires more than a small p-value.",
      "The result proves that changing species would cause the trait to change.",
      "A significant result guarantees that no confounding or structural differences exist."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "The test establishes evidence of a group mean difference, not a manipulable causal effect."
    )

  } else {

    output_text <- paste0(
      "The analyst wants to compare all three species and then examine selected pairwise differences."
    )

    question_text <- "Which follow-up analyses are reasonable? Select all that apply."

    options <- c(
      "Use ANOVA or regression to compare all three species jointly.",
      "Use planned or adjusted pairwise comparisons after the overall model.",
      "Run many unadjusted t-tests and report only the smallest p-value.",
      "Replace the numerical response with arbitrary categories before analysis."
    )

    correct_flags <- c(TRUE, TRUE, FALSE, FALSE)

    explanation <- paste0(
      "A joint model and controlled follow-up comparisons are suitable for three-group analysis."
    )
  }

  shuffled <- shuffle_multiple(options, correct_flags)

  full_question <- compose_prompt(
    context,
    output_text,
    question_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_015_%03d", i),
    blueprint_id = "R800_015",
    dataset_name = "iris",
    difficulty = "medium",
    scenario = "marketing",
    template_id = paste0("t_test_template_", task_type),
    language_style = style,
    presentation_layout = paste0("layout_", layout_id),
    cognitive_skill = "conceptual_reasoning_and_multiple_statement_evaluation",
    variables_used = "Sepal.Length, Petal.Length, Species",
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
# Generate
# ============================================================

R800_013 <- do.call(
  rbind,
  lapply(seq_len(20), generate_r800_013)
)

R800_015 <- do.call(
  rbind,
  lapply(seq_len(15), generate_r800_015)
)

ALL <- rbind(R800_013, R800_015)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_013) == 20)
stopifnot(nrow(R800_015) == 15)
stopifnot(nrow(ALL) == 35)
stopifnot(length(unique(ALL$id)) == 35)

stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$option_a)))
stopifnot(!any(is.na(ALL$option_b)))
stopifnot(!any(is.na(ALL$option_c)))
stopifnot(!any(is.na(ALL$option_d)))
stopifnot(!any(is.na(ALL$correct_answer)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))

stopifnot(length(unique(R800_013$template_id)) == 20)
stopifnot(length(unique(R800_015$template_id)) == 15)

# Exactly one letter for all Single Choice rows
stopifnot(
  all(
    nchar(R800_013$correct_answer) == 1
  )
)

# Multiple Choice rows must contain one or more valid letters
stopifnot(
  all(
    grepl(
      "^[A-D](,[A-D])*$",
      R800_015$correct_answer
    )
  )
)

# ============================================================
# Export
# ============================================================

write.csv(
  R800_013,
  "R800_013_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  R800_015,
  "R800_015_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  ALL,
  "R800_013_015_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

jsonlite::write_json(
  R800_013,
  "R800_013_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  R800_015,
  "R800_015_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  ALL,
  "R800_013_015_questions.json",
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
    "difficulty",
    "scenario",
    "language_style",
    "template_id",
    "correct_answer"
  )]
)

cat(
  "\nGenerated successfully:\n",
  "- R800_013: 20 Medium Single Choice questions\n",
  "- R800_015: 15 Medium Multiple Choice questions\n",
  "- Separate and combined CSV/JSON files saved\n"
)
