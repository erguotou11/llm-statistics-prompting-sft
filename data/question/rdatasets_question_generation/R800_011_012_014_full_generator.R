# ============================================================
# R800_011 + R800_012 + R800_014 — V2
#
# R800_011: t-test / ToothGrowth / Healthcare / Easy / Calculation / 25
# R800_012: t-test / ToothGrowth / Healthcare / Medium / Calculation / 25
# R800_014: t-test / iris / Agriculture / Easy / Calculation / 20
# ============================================================

set.seed(8111214)

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
  if (p < 0.001) "< 0.001" else paste0("= ", fmt(p, 3))
}

pick <- function(x) sample(x, 1)

welch_parts <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- var(x)
  vy <- var(y)
  se <- sqrt(vx / nx + vy / ny)
  t <- (mx - my) / se
  df <- (vx / nx + vy / ny)^2 /
    ((vx / nx)^2 / (nx - 1) + (vy / ny)^2 / (ny - 1))
  
  list(
    nx = nx, ny = ny,
    mx = mx, my = my,
    vx = vx, vy = vy,
    se = se, t = t, df = df
  )
}

one_parts <- function(x, mu0) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s / sqrt(n)
  t <- (m - mu0) / se
  list(n = n, m = m, s = s, se = se, t = t, df = n - 1)
}

cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  sp <- sqrt(
    ((nx - 1) * var(x) + (ny - 1) * var(y)) /
      (nx + ny - 2)
  )
  (mean(x) - mean(y)) / sp
}

make_record <- function(
    id, blueprint_id, dataset_name, difficulty, scenario,
    template_id, style, cognitive_skill, variables_used,
    question, reference_answer, solution_steps, answer_type
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
    language_style = style,
    cognitive_skill = cognitive_skill,
    question_type = "calculation",
    variables_used = variables_used,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_011 — ToothGrowth / Healthcare / Easy / Calculation
# One principal numerical operation per question
# ============================================================

easy_tg_tasks <- c(
  "mean_from_values",
  "mean_difference",
  "standard_error",
  "one_sample_t",
  "two_sample_t",
  "margin_of_error"
)

easy_tg_styles <- c(
  "clinical_case_note",
  "laboratory_log",
  "manuscript_check",
  "assessment_item",
  "trial_update",
  "data_audit",
  "team_dialogue",
  "results_caption"
)

easy_tg_question <- function(i) {
  
  task <- easy_tg_tasks[(i - 1) %% length(easy_tg_tasks) + 1]
  style <- pick(easy_tg_styles)
  dose <- pick(sort(unique(TG$dose)))
  supp <- pick(levels(TG$supp))
  g <- subset(TG, dose == dose & supp == supp)$len
  
  if (task == "mean_from_values") {
    
    shown <- sample(g, min(6, length(g)), replace = FALSE)
    ans <- mean(shown)
    
    templates <- c(
      sprintf(
        paste0(
          "Clinical case note\n\n",
          "Six tooth-growth measurements from the %s supplement arm at dose %.1f were selected for a rapid verification: %s.\n\n",
          "The report needs a single descriptive value before it can be signed off. Calculate the sample mean of these six measurements."
        ),
        supp, dose, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Laboratory log, entry 17\n\n",
          "For the %s condition at dose %.1f, the technician recorded the following len values in the verification batch: %s.\n\n",
          "What average tooth length should be entered in the log?"
        ),
        supp, dose, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "A draft table in a dental-growth manuscript lists six observations but leaves the mean blank.\n\n",
          "Condition: supplement %s, dose %.1f\n",
          "Observed lengths: %s\n\n",
          "Complete the missing mean."
        ),
        supp, dose, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Research lead: \"Before the meeting, can you check the average for this small batch?\"\n",
          "Analyst: \"The measurements are %s, all from %s at dose %.1f.\"\n\n",
          "Calculate the requested mean."
        ),
        paste(shown, collapse = ", "), supp, dose
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Mean = (", paste(shown, collapse = " + "), ") / ",
      length(shown), " = ", fmt(ans), "."
    )
    
  } else if (task == "mean_difference") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    mx <- mean(x)
    my <- mean(y)
    ans <- mx - my
    
    templates <- c(
      sprintf(
        paste0(
          "An interim trial update reports a mean tooth length of %s for OJ and %s for VC at dose %.1f.\n\n",
          "The treatment contrast is defined as OJ minus VC. Calculate that contrast."
        ),
        fmt(mx), fmt(my), dose
      ),
      sprintf(
        paste0(
          "The results caption for dose %.1f contains the two group means below:\n",
          "OJ: %s\nVC: %s\n\n",
          "By how many units does the OJ mean differ from the VC mean? Use OJ - VC."
        ),
        dose, fmt(mx), fmt(my)
      ),
      sprintf(
        paste0(
          "A data audit has confirmed the averages but not the reported difference. ",
          "At dose %.1f, the verified means are %s for OJ and %s for VC.\n\n",
          "Recalculate the mean difference used in the report."
        ),
        dose, fmt(mx), fmt(my)
      ),
      sprintf(
        paste0(
          "Clinician: \"The OJ and VC averages are %s and %s at this dose. What is the numerical gap?\"\n\n",
          "Report OJ minus VC."
        ),
        fmt(mx), fmt(my)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Difference = ", fmt(mx), " - ", fmt(my),
      " = ", fmt(ans), "."
    )
    
  } else if (task == "standard_error") {
    
    n <- length(g)
    s <- sd(g)
    ans <- s / sqrt(n)
    
    templates <- c(
      sprintf(
        paste0(
          "The quality-control sheet for supplement %s at dose %.1f gives n = %d and sample SD = %s.\n\n",
          "Calculate the standard error of the mean tooth length."
        ),
        supp, dose, n, fmt(s)
      ),
      sprintf(
        paste0(
          "A teaching slide omits one number from the summary of a ToothGrowth subgroup:\n",
          "n = %d, s = %s.\n\n",
          "Find s/sqrt(n)."
        ),
        n, fmt(s)
      ),
      sprintf(
        paste0(
          "Before a confidence interval is produced, the analyst needs the uncertainty attached to the group mean. ",
          "For %s at dose %.1f, the sample contains %d observations with SD %s.\n\n",
          "What is the standard error?"
        ),
        supp, dose, n, fmt(s)
      ),
      sprintf(
        paste0(
          "The laboratory summary reports a standard deviation of %s from %d observations. ",
          "The standard-error field is blank.\n\n",
          "Complete it."
        ),
        fmt(s), n
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = s/sqrt(n) = ", fmt(s), "/sqrt(", n,
      ") = ", fmt(ans), "."
    )
    
  } else if (task == "one_sample_t") {
    
    mu0 <- round(mean(g) + pick(c(-3, -2, 2, 3)), 1)
    parts <- one_parts(g, mu0)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "A historical benchmark states that mean tooth length under this condition is %.1f. ",
          "For supplement %s at dose %.1f, the current study reports x-bar = %s, s = %s and n = %d.\n\n",
          "Calculate the one-sample t statistic."
        ),
        mu0, supp, dose, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "The trial monitor asks for a numerical check against the benchmark mu0 = %.1f.\n",
          "Observed summary: mean %s, SD %s, n %d.\n\n",
          "Using t = (x-bar - mu0)/(s/sqrt(n)), calculate t."
        ),
        mu0, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "A one-sample comparison appears in the appendix, but the test statistic is missing.\n\n",
          "Condition: %s, dose %.1f\n",
          "x-bar = %s, s = %s, n = %d, mu0 = %.1f\n\n",
          "Fill in the missing t value."
        ),
        supp, dose, fmt(parts$m), fmt(parts$s), parts$n, mu0
      ),
      sprintf(
        paste0(
          "Statistician: \"The benchmark is %.1f. The subgroup mean is %s from %d observations, with SD %s.\"\n\n",
          "What one-sample t statistic follows from these values?"
        ),
        mu0, fmt(parts$m), parts$n, fmt(parts$s)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(parts$s), "/sqrt(", parts$n,
      ") = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$m), " - ", mu0,
      ")/", fmt(parts$se), " = ", fmt(ans), "."
    )
    
  } else if (task == "two_sample_t") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    parts <- welch_parts(x, y)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "At dose %.1f, the OJ group has mean %s, variance %s and n = %d; ",
          "the VC group has mean %s, variance %s and n = %d.\n\n",
          "Calculate the Welch t statistic for OJ minus VC."
        ),
        dose, fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny
      ),
      sprintf(
        paste0(
          "The manuscript gives enough information to reconstruct the unequal-variance comparison:\n",
          "OJ: x-bar = %s, s^2 = %s, n = %d\n",
          "VC: x-bar = %s, s^2 = %s, n = %d\n\n",
          "What test statistic should appear in the results table?"
        ),
        fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny
      ),
      sprintf(
        paste0(
          "A dental researcher wants a quick numerical comparison of the two supplements at dose %.1f. ",
          "Use the summary statistics below to compute the Welch t value.\n\n",
          "OJ (%d observations): mean %s, variance %s\n",
          "VC (%d observations): mean %s, variance %s"
        ),
        dose, parts$nx, fmt(parts$mx), fmt(parts$vx),
        parts$ny, fmt(parts$my), fmt(parts$vy)
      ),
      sprintf(
        paste0(
          "Reviewer comment: \"Please verify the t statistic for the OJ-versus-VC contrast at dose %.1f.\"\n\n",
          "The group summaries are (%s, %s, %d) for OJ and (%s, %s, %d) for VC, ",
          "where each triple is mean, variance and sample size."
        ),
        dose,
        fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = sqrt(", fmt(parts$vx), "/", parts$nx,
      " + ", fmt(parts$vy), "/", parts$ny,
      ") = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$mx), " - ", fmt(parts$my),
      ")/", fmt(parts$se), " = ", fmt(ans), "."
    )
    
  } else {
    
    n <- length(g)
    s <- sd(g)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    ans <- t_star * se
    
    templates <- c(
      sprintf(
        paste0(
          "The 95%% confidence interval for supplement %s at dose %.1f is being assembled. ",
          "The group has n = %d, SD = %s, and the relevant t critical value is %s.\n\n",
          "Calculate the margin of error."
        ),
        supp, dose, n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "A results table shows the sample mean but leaves the plus-or-minus quantity blank.\n",
          "Use n = %d, s = %s and t* = %s.\n\n",
          "What margin should accompany the mean?"
        ),
        n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "For a small ToothGrowth subgroup, the analyst has already selected the 95%% critical value %s. ",
          "With SD %s from %d observations, find t* x SE."
        ),
        fmt(t_star), fmt(s), n
      ),
      sprintf(
        paste0(
          "Trial note: \"The interval centre is ready; only the margin remains.\"\n",
          "Group size %d, sample SD %s, t critical %s.\n\n",
          "Calculate the missing margin of error."
        ),
        n, fmt(s), fmt(t_star)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(s), "/sqrt(", n, ") = ", fmt(se), ".\n",
      "Margin = ", fmt(t_star), " x ", fmt(se),
      " = ", fmt(ans), "."
    )
  }
  
  make_record(
    id = sprintf("R800_011_%03d", i),
    blueprint_id = "R800_011",
    dataset_name = "ToothGrowth",
    difficulty = "easy",
    scenario = "healthcare",
    template_id = paste0("t_test_template_", task),
    style = style,
    cognitive_skill = "single_step_numerical_calculation",
    variables_used = "len, supp, dose",
    question = question,
    reference_answer = fmt(ans),
    solution_steps = solution,
    answer_type = "numeric"
  )
}

# ============================================================
# R800_012 — ToothGrowth / Healthcare / Medium / Calculation
# Linked calculations + inferential decision/comparison
# ============================================================

medium_tg_tasks <- c(
  "welch_test_decision",
  "confidence_interval_decision",
  "two_dose_contrast",
  "effect_size_and_test",
  "one_sample_test_decision",
  "pooled_vs_welch",
  "power_precision_change",
  "dose_response_comparison"
)

medium_tg_styles <- c(
  "trial_protocol_query",
  "peer_review_response",
  "clinical_meeting",
  "analysis_plan",
  "manuscript_revision",
  "teaching_vignette",
  "audit_investigation",
  "statistical_consultation",
  "conference_abstract",
  "results_reconciliation"
)

medium_tg_question <- function(i) {
  
  task <- medium_tg_tasks[(i - 1) %% length(medium_tg_tasks) + 1]
  style <- pick(medium_tg_styles)
  dose <- pick(sort(unique(TG$dose)))
  
  if (task == "welch_test_decision") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    parts <- welch_parts(x, y)
    tst <- t.test(x, y)
    alpha <- pick(c(0.05, 0.01))
    decision <- ifelse(tst$p.value < alpha, "reject H0", "do not reject H0")
    
    templates <- c(
      sprintf(
        paste0(
          "The interim analysis plan specifies a two-sided Welch test for the OJ-versus-VC comparison at dose %.1f. ",
          "The verified summaries are:\n",
          "OJ: n = %d, mean = %s, variance = %s\n",
          "VC: n = %d, mean = %s, variance = %s\n\n",
          "Calculate the standard error, t statistic, Welch degrees of freedom and two-sided p-value. ",
          "Using alpha = %.2f, state the decision."
        ),
        dose, parts$nx, fmt(parts$mx), fmt(parts$vx),
        parts$ny, fmt(parts$my), fmt(parts$vy), alpha
      ),
      sprintf(
        paste0(
          "Peer-review response, statistical point 3\n\n",
          "The reviewer asks whether OJ and VC differ at dose %.1f. Reconstruct the full unequal-variance test from the group summaries: ",
          "(%d, %s, %s) for OJ and (%d, %s, %s) for VC, where each triple is n, mean and variance.\n\n",
          "Report SE, t, df, p and the conclusion at alpha = %.2f."
        ),
        dose,
        parts$nx, fmt(parts$mx), fmt(parts$vx),
        parts$ny, fmt(parts$my), fmt(parts$vy),
        alpha
      ),
      sprintf(
        paste0(
          "At a clinical research meeting, two analysts disagree about the dose-%.1f supplement comparison. ",
          "One wants only the difference in means; the other requests the complete Welch calculation.\n\n",
          "OJ: mean %s, variance %s, n %d\n",
          "VC: mean %s, variance %s, n %d\n\n",
          "Resolve the disagreement by computing t, approximate df and the two-sided p-value, then make the alpha = %.2f decision."
        ),
        dose,
        fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny,
        alpha
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "SE = ", fmt(parts$se),
      "; t = ", fmt(parts$t),
      "; df = ", fmt(parts$df),
      "; p ", p_text(tst$p.value),
      "; ", decision
    )
    solution <- paste0(
      "SE = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$mx), " - ", fmt(parts$my),
      ")/", fmt(parts$se), " = ", fmt(parts$t), ".\n",
      "Welch df = ", fmt(parts$df), ".\n",
      "Two-sided p ", p_text(tst$p.value), ".\n",
      "At alpha = ", alpha, ", ", decision, "."
    )
    
  } else if (task == "confidence_interval_decision") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    tst <- t.test(x, y, conf.level = 0.95)
    diff <- mean(x) - mean(y)
    excludes_zero <- tst$conf.int[1] > 0 || tst$conf.int[2] < 0
    
    templates <- c(
      sprintf(
        paste0(
          "A manuscript revision replaces a bare p-value with a 95%% confidence interval for the mean difference OJ - VC at dose %.1f.\n\n",
          "Using the ToothGrowth observations for that dose, calculate the estimated difference and its Welch confidence interval. ",
          "Then state whether the interval excludes zero."
        ),
        dose
      ),
      sprintf(
        paste0(
          "The conference abstract must report both magnitude and uncertainty. For dose %.1f, construct the 95%% confidence interval for mu_OJ - mu_VC from the real data.\n\n",
          "Give the point estimate, lower and upper limits, and the corresponding inference about equality of means."
        ),
        dose
      ),
      sprintf(
        paste0(
          "A clinical collaborator asks, \"How large might the supplement difference plausibly be at dose %.1f?\"\n\n",
          "Answer with the Welch 95%% interval for OJ minus VC, and indicate whether zero is a plausible value."
        ),
        dose
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "difference = ", fmt(diff),
      "; 95% CI [", fmt(tst$conf.int[1]), ", ",
      fmt(tst$conf.int[2]), "]; ",
      ifelse(excludes_zero, "excludes zero", "includes zero")
    )
    solution <- paste0(
      "Estimated difference = ", fmt(diff), ".\n",
      "Welch 95% CI = [", fmt(tst$conf.int[1]), ", ",
      fmt(tst$conf.int[2]), "].\n",
      ifelse(
        excludes_zero,
        "Because zero is outside the interval, the two-sided 5% test rejects equality of means.",
        "Because zero lies inside the interval, the two-sided 5% test does not reject equality of means."
      )
    )
    
  } else if (task == "two_dose_contrast") {
    
    doses <- sort(sample(sort(unique(TG$dose)), 2, replace = FALSE))
    supp <- pick(levels(TG$supp))
    x <- subset(TG, supp == supp & dose == doses[1])$len
    y <- subset(TG, supp == supp & dose == doses[2])$len
    parts <- welch_parts(x, y)
    tst <- t.test(x, y)
    diff <- mean(y) - mean(x)
    
    templates <- c(
      sprintf(
        paste0(
          "The dose-escalation section of a healthcare report compares %s at doses %.1f and %.1f. ",
          "The question is not merely whether the means differ, but by how much the higher-dose group exceeds the lower-dose group.\n\n",
          "Calculate the mean increase, the Welch t statistic for lower dose minus higher dose, and the two-sided p-value."
        ),
        supp, doses[1], doses[2]
      ),
      sprintf(
        paste0(
          "During a dose-review meeting, the %s arm is examined at %.1f and %.1f. ",
          "Use the real ToothGrowth values to quantify the increase in mean len and to test the difference with Welch's method.\n\n",
          "Report the increase, t and p."
        ),
        supp, doses[1], doses[2]
      ),
      sprintf(
        paste0(
          "A results-reconciliation check focuses on the %s supplement. ",
          "The lower and higher dose groups are %.1f and %.1f.\n\n",
          "Compute: (1) higher-dose mean minus lower-dose mean, ",
          "(2) the Welch statistic using lower minus higher, and (3) the two-sided p-value."
        ),
        supp, doses[1], doses[2]
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "increase = ", fmt(diff),
      "; t(lower-higher) = ", fmt(parts$t),
      "; p ", p_text(tst$p.value)
    )
    solution <- paste0(
      "Higher-dose increase = ", fmt(mean(y)), " - ",
      fmt(mean(x)), " = ", fmt(diff), ".\n",
      "Welch t for lower - higher = ", fmt(parts$t), ".\n",
      "Two-sided p ", p_text(tst$p.value), "."
    )
    
  } else if (task == "effect_size_and_test") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    d <- cohens_d(x, y)
    tst <- t.test(x, y)
    diff <- mean(x) - mean(y)
    
    templates <- c(
      sprintf(
        paste0(
          "A reviewer argues that the dose-%.1f comparison should include an effect size, not only a significance test.\n\n",
          "Using OJ minus VC, calculate the raw mean difference, Cohen's d based on the pooled SD, and the Welch two-sided p-value."
        ),
        dose
      ),
      sprintf(
        paste0(
          "The statistical consultation notes that a p-value alone does not express the size of the treatment contrast. ",
          "For dose %.1f, report the OJ - VC mean difference, pooled-SD Cohen's d and Welch p-value."
        ),
        dose
      ),
      sprintf(
        paste0(
          "To complete a conference table, three numbers are required for the dose-%.1f supplement comparison: ",
          "the difference in means, the standardised difference and the two-sided p-value.\n\n",
          "Calculate all three from ToothGrowth."
        ),
        dose
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "difference = ", fmt(diff),
      "; Cohen's d = ", fmt(d),
      "; p ", p_text(tst$p.value)
    )
    solution <- paste0(
      "Mean difference = ", fmt(diff), ".\n",
      "Cohen's d = ", fmt(d), ".\n",
      "Welch two-sided p ", p_text(tst$p.value), "."
    )
    
  } else if (task == "one_sample_test_decision") {
    
    supp <- pick(levels(TG$supp))
    g <- subset(TG, supp == supp & dose == dose)$len
    mu0 <- round(mean(g) + pick(c(-4, -3, 3, 4)), 1)
    parts <- one_parts(g, mu0)
    tst <- t.test(g, mu = mu0)
    alpha <- 0.05
    decision <- ifelse(tst$p.value < alpha, "reject H0", "do not reject H0")
    
    templates <- c(
      sprintf(
        paste0(
          "A historical control value of %.1f is used to benchmark the %s group at dose %.1f. ",
          "The current subgroup has mean %s, SD %s and n = %d.\n\n",
          "Calculate SE, t and the two-sided p-value, then make the 5%% decision."
        ),
        mu0, supp, dose, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "The trial protocol defines H0: mu = %.1f for %s at dose %.1f. ",
          "Using the current data, carry out the complete one-sample calculation and state whether the benchmark is rejected at alpha = 0.05."
        ),
        mu0, supp, dose
      ),
      sprintf(
        paste0(
          "A monitoring committee asks whether the observed mean for %s at dose %.1f is compatible with the benchmark %.1f.\n\n",
          "Report t, df, p and the decision."
        ),
        supp, dose, mu0
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "SE = ", fmt(parts$se),
      "; t = ", fmt(parts$t),
      "; df = ", parts$df,
      "; p ", p_text(tst$p.value),
      "; ", decision
    )
    solution <- paste0(
      "SE = ", fmt(parts$se), ".\n",
      "t = ", fmt(parts$t), ", df = ", parts$df, ".\n",
      "Two-sided p ", p_text(tst$p.value), ".\n",
      decision, " at alpha = 0.05."
    )
    
  } else if (task == "pooled_vs_welch") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    welch <- t.test(x, y, var.equal = FALSE)
    pooled <- t.test(x, y, var.equal = TRUE)
    
    templates <- c(
      sprintf(
        paste0(
          "Two analysts have produced different test outputs for the supplement comparison at dose %.1f. ",
          "One assumed equal variances; the other used Welch's method.\n\n",
          "Calculate both t statistics and both degrees of freedom, then report the absolute difference between the two t values."
        ),
        dose
      ),
      sprintf(
        paste0(
          "A statistical audit asks whether the equal-variance assumption materially changes the dose-%.1f comparison.\n\n",
          "From the ToothGrowth data, obtain the pooled t and df, the Welch t and df, and |t_pooled - t_Welch|."
        ),
        dose
      ),
      sprintf(
        paste0(
          "The analysis plan is being revised. Before choosing a default test, compare the numerical outputs of pooled and Welch two-sample t-tests at dose %.1f.\n\n",
          "Report both t values, both dfs and their absolute t difference."
        ),
        dose
      )
    )
    
    question <- pick(templates)
    tdif <- abs(unname(pooled$statistic) - unname(welch$statistic))
    answer <- paste0(
      "pooled t = ", fmt(pooled$statistic),
      ", df = ", fmt(pooled$parameter),
      "; Welch t = ", fmt(welch$statistic),
      ", df = ", fmt(welch$parameter),
      "; |difference| = ", fmt(tdif)
    )
    solution <- paste0(
      "Pooled test: t = ", fmt(pooled$statistic),
      ", df = ", fmt(pooled$parameter), ".\n",
      "Welch test: t = ", fmt(welch$statistic),
      ", df = ", fmt(welch$parameter), ".\n",
      "Absolute t difference = ", fmt(tdif), "."
    )
    
  } else if (task == "power_precision_change") {
    
    supp <- pick(levels(TG$supp))
    g <- subset(TG, supp == supp)$len
    s <- sd(g)
    n1 <- length(g)
    n2 <- floor(n1 / 2)
    se1 <- s / sqrt(n1)
    se2 <- s / sqrt(n2)
    ratio <- se2 / se1
    pct <- (ratio - 1) * 100
    
    templates <- c(
      sprintf(
        paste0(
          "A budget revision would reduce the %s supplement sample from %d observations to %d, while the SD is expected to remain near %s.\n\n",
          "Calculate the original and reduced standard errors, the ratio SE_reduced/SE_original, and the percentage increase in SE."
        ),
        supp, n1, n2, fmt(s)
      ),
      sprintf(
        paste0(
          "The project manager proposes halving the available %s observations. ",
          "Using SD = %s, compare precision at n = %d and n = %d.\n\n",
          "Report both SEs and how much larger the reduced-sample SE would be in percentage terms."
        ),
        supp, fmt(s), n1, n2
      ),
      sprintf(
        paste0(
          "A sensitivity calculation is required before the ToothGrowth analysis is finalised. ",
          "For the %s observations, use s = %s to quantify the effect of changing n from %d to %d on the standard error."
        ),
        supp, fmt(s), n1, n2
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "SE_original = ", fmt(se1),
      "; SE_reduced = ", fmt(se2),
      "; ratio = ", fmt(ratio),
      "; increase = ", fmt(pct, 1), "%"
    )
    solution <- paste0(
      "SE_original = ", fmt(se1), ".\n",
      "SE_reduced = ", fmt(se2), ".\n",
      "Ratio = ", fmt(ratio), ".\n",
      "Percentage increase = ", fmt(pct, 1), "%."
    )
    
  } else {
    
    doses <- sort(unique(TG$dose))
    d_low <- doses[1]
    d_high <- doses[length(doses)]
    
    oj_low <- mean(subset(TG, dose == d_low & supp == "OJ")$len)
    vc_low <- mean(subset(TG, dose == d_low & supp == "VC")$len)
    oj_high <- mean(subset(TG, dose == d_high & supp == "OJ")$len)
    vc_high <- mean(subset(TG, dose == d_high & supp == "VC")$len)
    
    contrast_low <- oj_low - vc_low
    contrast_high <- oj_high - vc_high
    change_in_contrast <- contrast_high - contrast_low
    
    templates <- c(
      sprintf(
        paste0(
          "The treatment team suspects that the OJ-versus-VC difference is not constant across dose. ",
          "At dose %.1f, the means are OJ %s and VC %s; at dose %.1f, they are OJ %s and VC %s.\n\n",
          "Calculate the supplement contrast at each dose and then the change in contrast from low dose to high dose."
        ),
        d_low, fmt(oj_low), fmt(vc_low),
        d_high, fmt(oj_high), fmt(vc_high)
      ),
      sprintf(
        paste0(
          "A results meeting focuses on whether the supplement gap narrows or widens across the dose range.\n\n",
          "Low dose %.1f: OJ %s, VC %s\n",
          "High dose %.1f: OJ %s, VC %s\n\n",
          "Find both OJ - VC contrasts and the high-minus-low contrast change."
        ),
        d_low, fmt(oj_low), fmt(vc_low),
        d_high, fmt(oj_high), fmt(vc_high)
      ),
      sprintf(
        paste0(
          "The manuscript currently reports only separate group means. Convert them into a more informative comparison.\n\n",
          "Using the lowest and highest doses, compute OJ - VC at each dose, then compute how much that difference changes."
        )
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "low-dose contrast = ", fmt(contrast_low),
      "; high-dose contrast = ", fmt(contrast_high),
      "; change = ", fmt(change_in_contrast)
    )
    solution <- paste0(
      "Low-dose contrast = ", fmt(oj_low), " - ",
      fmt(vc_low), " = ", fmt(contrast_low), ".\n",
      "High-dose contrast = ", fmt(oj_high), " - ",
      fmt(vc_high), " = ", fmt(contrast_high), ".\n",
      "Change in contrast = ", fmt(contrast_high), " - ",
      fmt(contrast_low), " = ", fmt(change_in_contrast), "."
    )
  }
  
  make_record(
    id = sprintf("R800_012_%03d", i),
    blueprint_id = "R800_012",
    dataset_name = "ToothGrowth",
    difficulty = "medium",
    scenario = "healthcare",
    template_id = paste0("t_test_template_", task),
    style = style,
    cognitive_skill = "multi_step_inference_and_numerical_decision",
    variables_used = "len, supp, dose",
    question = question,
    reference_answer = answer,
    solution_steps = solution,
    answer_type = "multi_value_numeric_or_decision"
  )
}

# ============================================================
# R800_014 — iris / Agriculture / Easy / Calculation
# One principal numerical operation per question
# ============================================================

easy_ir_tasks <- c(
  "mean_from_values",
  "species_mean_difference",
  "standard_error",
  "one_sample_t",
  "two_sample_t",
  "margin_of_error"
)

easy_ir_styles <- c(
  "field_station_note",
  "breeding_record",
  "greenhouse_log",
  "extension_example",
  "seed_quality_check",
  "course_assessment",
  "research_table",
  "agronomy_dialogue"
)

easy_ir_question <- function(i) {
  
  task <- easy_ir_tasks[(i - 1) %% length(easy_ir_tasks) + 1]
  style <- pick(easy_ir_styles)
  trait <- pick(c("Sepal.Length", "Petal.Length"))
  species <- pick(levels(IR$Species))
  g <- IR[IR$Species == species, trait]
  
  if (task == "mean_from_values") {
    
    shown <- sample(g, 6, replace = FALSE)
    ans <- mean(shown)
    
    templates <- c(
      sprintf(
        paste0(
          "Field station note\n\n",
          "Six %s measurements from %s plants were retained after a routine quality check: %s.\n\n",
          "Calculate the mean length for this batch."
        ),
        trait, species, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "A breeding record lists six observations but leaves the batch average blank.\n",
          "Species: %s\nTrait: %s\nMeasurements: %s\n\n",
          "Complete the missing mean."
        ),
        species, trait, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Greenhouse log: %s was measured for six %s flowers, giving %s.\n\n",
          "What average should be entered in the log?"
        ),
        trait, species, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Agronomist: \"I only need the average for this six-plant check.\"\n",
          "Assistant: \"The %s values are %s.\"\n\n",
          "Calculate the requested mean."
        ),
        trait, paste(shown, collapse = ", ")
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Mean = (", paste(shown, collapse = " + "), ")/6 = ",
      fmt(ans), "."
    )
    
  } else if (task == "species_mean_difference") {
    
    sp <- sample(levels(IR$Species), 2, replace = FALSE)
    x <- IR[IR$Species == sp[1], trait]
    y <- IR[IR$Species == sp[2], trait]
    mx <- mean(x)
    my <- mean(y)
    ans <- mx - my
    
    templates <- c(
      sprintf(
        paste0(
          "A plant-breeding report gives mean %s values of %s for %s and %s for %s.\n\n",
          "Calculate the difference using %s minus %s."
        ),
        trait, fmt(mx), sp[1], fmt(my), sp[2], sp[1], sp[2]
      ),
      sprintf(
        paste0(
          "Two iris groups are being compared before a greenhouse selection round.\n",
          "%s mean for %s: %s\n",
          "%s mean for %s: %s\n\n",
          "Find the signed difference between the two means in the order shown."
        ),
        trait, sp[1], fmt(mx), trait, sp[2], fmt(my)
      ),
      sprintf(
        paste0(
          "The extension bulletin reports the two averages but not their gap. ",
          "For %s, the means are %s (%s) and %s (%s).\n\n",
          "What is the first mean minus the second?"
        ),
        trait, fmt(mx), sp[1], fmt(my), sp[2]
      ),
      sprintf(
        paste0(
          "Seed-quality check: compare the average %s of %s with that of %s. ",
          "The verified means are %s and %s.\n\n",
          "Calculate the difference."
        ),
        trait, sp[1], sp[2], fmt(mx), fmt(my)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Difference = ", fmt(mx), " - ", fmt(my),
      " = ", fmt(ans), "."
    )
    
  } else if (task == "standard_error") {
    
    n <- length(g)
    s <- sd(g)
    ans <- s / sqrt(n)
    
    templates <- c(
      sprintf(
        paste0(
          "The greenhouse summary for %s %s reports n = %d and sample SD = %s.\n\n",
          "Calculate the standard error of the mean."
        ),
        species, trait, n, fmt(s)
      ),
      sprintf(
        paste0(
          "A seed-company quality table is missing the SE for %s in %s. ",
          "Use s = %s and n = %d."
        ),
        trait, species, fmt(s), n
      ),
      sprintf(
        paste0(
          "During an agricultural statistics class, students are given SD %s from %d %s observations.\n\n",
          "Find s/sqrt(n)."
        ),
        fmt(s), n, species
      ),
      sprintf(
        paste0(
          "The field report lists the variability and sample size but not the precision of the mean: ",
          "s = %s, n = %d.\n\n",
          "Calculate the SE."
        ),
        fmt(s), n
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(s), "/sqrt(", n, ") = ",
      fmt(ans), "."
    )
    
  } else if (task == "one_sample_t") {
    
    mu0 <- round(mean(g) + pick(c(-0.5, -0.3, 0.3, 0.5)), 1)
    parts <- one_parts(g, mu0)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "A breeding benchmark sets the expected %s for %s at %.1f. ",
          "The observed sample has mean %s, SD %s and n = %d.\n\n",
          "Calculate the one-sample t statistic."
        ),
        trait, species, mu0, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "The field station wants to compare the %s mean for %s with the reference value %.1f.\n",
          "Summary: x-bar = %s, s = %s, n = %d.\n\n",
          "Find t."
        ),
        trait, species, mu0, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "A course assessment provides the following plant summary:\n",
          "species %s, trait %s, mean %s, SD %s, n %d, benchmark %.1f.\n\n",
          "Compute the one-sample test statistic."
        ),
        species, trait, fmt(parts$m), fmt(parts$s), parts$n, mu0
      ),
      sprintf(
        paste0(
          "Agronomist: \"Our benchmark is %.1f. The sample mean is %s from %d plants, with SD %s.\"\n\n",
          "What t statistic should be reported?"
        ),
        mu0, fmt(parts$m), parts$n, fmt(parts$s)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(parts$s), "/sqrt(", parts$n,
      ") = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$m), " - ", mu0,
      ")/", fmt(parts$se), " = ", fmt(ans), "."
    )
    
  } else if (task == "two_sample_t") {
    
    sp <- sample(levels(IR$Species), 2, replace = FALSE)
    x <- IR[IR$Species == sp[1], trait]
    y <- IR[IR$Species == sp[2], trait]
    parts <- welch_parts(x, y)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "A cultivar comparison uses %s as the response. ",
          "%s has mean %s, variance %s and n = %d; ",
          "%s has mean %s, variance %s and n = %d.\n\n",
          "Calculate the Welch t statistic for %s minus %s."
        ),
        trait,
        sp[1], fmt(parts$mx), fmt(parts$vx), parts$nx,
        sp[2], fmt(parts$my), fmt(parts$vy), parts$ny,
        sp[1], sp[2]
      ),
      sprintf(
        paste0(
          "The breeding station has summarised two species but not completed the test calculation.\n\n",
          "%s: mean %s, variance %s, n %d\n",
          "%s: mean %s, variance %s, n %d\n\n",
          "Find the unequal-variance t statistic in the order shown."
        ),
        sp[1], fmt(parts$mx), fmt(parts$vx), parts$nx,
        sp[2], fmt(parts$my), fmt(parts$vy), parts$ny
      ),
      sprintf(
        paste0(
          "An extension specialist is checking whether the reported %s comparison is numerically correct. ",
          "Use the real iris summaries for %s and %s to compute the Welch t value."
        ),
        trait, sp[1], sp[2]
      ),
      sprintf(
        paste0(
          "Greenhouse analyst: \"Please reconstruct the t statistic for the %s comparison between %s and %s.\"\n\n",
          "Use the species means, variances and sample sizes from iris."
        ),
        trait, sp[1], sp[2]
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$mx), " - ",
      fmt(parts$my), ")/", fmt(parts$se),
      " = ", fmt(ans), "."
    )
    
  } else {
    
    n <- length(g)
    s <- sd(g)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    ans <- t_star * se
    
    templates <- c(
      sprintf(
        paste0(
          "The 95%% interval for mean %s in %s is being prepared. ",
          "The sample has n = %d, SD = %s and t* = %s.\n\n",
          "Calculate the margin of error."
        ),
        trait, species, n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "A greenhouse report leaves the plus-or-minus value blank. ",
          "For %s %s, use n = %d, s = %s and t* = %s."
        ),
        species, trait, n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "The agricultural extension example already gives the sample mean. ",
          "Only the 95%% confidence margin remains to be calculated from s = %s, n = %d and t* = %s."
        ),
        fmt(s), n, fmt(t_star)
      ),
      sprintf(
        paste0(
          "Before the field summary is published, compute t* x SE for the %s measurements of %s. ",
          "Use t* = %s, s = %s and n = %d."
        ),
        trait, species, fmt(t_star), fmt(s), n
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(s), "/sqrt(", n, ") = ", fmt(se), ".\n",
      "Margin = ", fmt(t_star), " x ", fmt(se),
      " = ", fmt(ans), "."
    )
  }
  
  make_record(
    id = sprintf("R800_014_%03d", i),
    blueprint_id = "R800_014",
    dataset_name = "iris",
    difficulty = "easy",
    scenario = "agriculture",
    template_id = paste0("t_test_template_", task),
    style = style,
    cognitive_skill = "single_step_numerical_calculation",
    variables_used = "Sepal.Length, Petal.Length, Species",
    question = question,
    reference_answer = fmt(ans),
    solution_steps = solution,
    answer_type = "numeric"
  )
}

# ============================================================
# Generate
# ============================================================

R800_011 <- do.call(rbind, lapply(seq_len(25), easy_tg_question))
R800_012 <- do.call(rbind, lapply(seq_len(25), medium_tg_question))
R800_014 <- do.call(rbind, lapply(seq_len(20), easy_ir_question))

ALL <- rbind(R800_011, R800_012, R800_014)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_011) == 25)
stopifnot(nrow(R800_012) == 25)
stopifnot(nrow(R800_014) == 20)
stopifnot(nrow(ALL) == 70)
stopifnot(length(unique(ALL$id)) == 70)
stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))
stopifnot(!any(ALL$question == ""))
stopifnot(!any(ALL$reference_answer == ""))

# Difficulty separation checks
stopifnot(all(R800_011$cognitive_skill == "single_step_numerical_calculation"))
stopifnot(all(R800_014$cognitive_skill == "single_step_numerical_calculation"))
stopifnot(all(R800_012$cognitive_skill == "multi_step_inference_and_numerical_decision"))

# ============================================================
# Export
# ============================================================

write.csv(R800_011, "R800_011_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(R800_012, "R800_012_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(R800_014, "R800_014_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ALL, "R800_011_012_014_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

jsonlite::write_json(
  R800_011, "R800_011_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)
jsonlite::write_json(
  R800_012, "R800_012_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)
jsonlite::write_json(
  R800_014, "R800_014_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)
jsonlite::write_json(
  ALL, "R800_011_012_014_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)

print(
  ALL[, c(
    "id", "difficulty", "scenario",
    "language_style", "template_id",
    "reference_answer"
  )]
)

cat(
  "\nV2 generated successfully:\n",
  "R800_011: 25 easy ToothGrowth questions\n",
  "R800_012: 25 medium ToothGrowth questions\n",
  "R800_014: 20 easy iris questions\n"
)

库
/
  R800_011_012_014_generator_V2.R

# ============================================================
# R800_011 + R800_012 + R800_014 — V2
#
# R800_011: t-test / ToothGrowth / Healthcare / Easy / Calculation / 25
# R800_012: t-test / ToothGrowth / Healthcare / Medium / Calculation / 25
# R800_014: t-test / iris / Agriculture / Easy / Calculation / 20
#
# V2 improvements
# 1. Full-scenario templates rather than a repeated "role + action" opening.
# 2. Different discourse forms: case note, manuscript extract, dialogue,
#    laboratory record, assessment item, audit query, field report, etc.
# 3. Easy questions use one principal numerical operation.
# 4. Medium questions require two to four linked calculations and an
#    inferential decision or comparison.
# 5. Every numerical answer is computed from a real R dataset.
# ============================================================

set.seed(8111214)

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
  if (p < 0.001) "< 0.001" else paste0("= ", fmt(p, 3))
}

pick <- function(x) sample(x, 1)

welch_parts <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- var(x)
  vy <- var(y)
  se <- sqrt(vx / nx + vy / ny)
  t <- (mx - my) / se
  df <- (vx / nx + vy / ny)^2 /
    ((vx / nx)^2 / (nx - 1) + (vy / ny)^2 / (ny - 1))
  
  list(
    nx = nx, ny = ny,
    mx = mx, my = my,
    vx = vx, vy = vy,
    se = se, t = t, df = df
  )
}

one_parts <- function(x, mu0) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  se <- s / sqrt(n)
  t <- (m - mu0) / se
  list(n = n, m = m, s = s, se = se, t = t, df = n - 1)
}

cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  sp <- sqrt(
    ((nx - 1) * var(x) + (ny - 1) * var(y)) /
      (nx + ny - 2)
  )
  (mean(x) - mean(y)) / sp
}

make_record <- function(
    id, blueprint_id, dataset_name, difficulty, scenario,
    template_id, style, cognitive_skill, variables_used,
    question, reference_answer, solution_steps, answer_type
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
    language_style = style,
    cognitive_skill = cognitive_skill,
    question_type = "calculation",
    variables_used = variables_used,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_011 — ToothGrowth / Healthcare / Easy / Calculation
# One principal numerical operation per question
# ============================================================

easy_tg_tasks <- c(
  "mean_from_values",
  "mean_difference",
  "standard_error",
  "one_sample_t",
  "two_sample_t",
  "margin_of_error"
)

easy_tg_styles <- c(
  "clinical_case_note",
  "laboratory_log",
  "manuscript_check",
  "assessment_item",
  "trial_update",
  "data_audit",
  "team_dialogue",
  "results_caption"
)

easy_tg_question <- function(i) {
  
  task <- easy_tg_tasks[(i - 1) %% length(easy_tg_tasks) + 1]
  style <- pick(easy_tg_styles)
  dose <- pick(sort(unique(TG$dose)))
  supp <- pick(levels(TG$supp))
  g <- subset(TG, dose == dose & supp == supp)$len
  
  if (task == "mean_from_values") {
    
    shown <- sample(g, min(6, length(g)), replace = FALSE)
    ans <- mean(shown)
    
    templates <- c(
      sprintf(
        paste0(
          "Clinical case note\n\n",
          "Six tooth-growth measurements from the %s supplement arm at dose %.1f were selected for a rapid verification: %s.\n\n",
          "The report needs a single descriptive value before it can be signed off. Calculate the sample mean of these six measurements."
        ),
        supp, dose, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Laboratory log, entry 17\n\n",
          "For the %s condition at dose %.1f, the technician recorded the following len values in the verification batch: %s.\n\n",
          "What average tooth length should be entered in the log?"
        ),
        supp, dose, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "A draft table in a dental-growth manuscript lists six observations but leaves the mean blank.\n\n",
          "Condition: supplement %s, dose %.1f\n",
          "Observed lengths: %s\n\n",
          "Complete the missing mean."
        ),
        supp, dose, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Research lead: \"Before the meeting, can you check the average for this small batch?\"\n",
          "Analyst: \"The measurements are %s, all from %s at dose %.1f.\"\n\n",
          "Calculate the requested mean."
        ),
        paste(shown, collapse = ", "), supp, dose
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Mean = (", paste(shown, collapse = " + "), ") / ",
      length(shown), " = ", fmt(ans), "."
    )
    
  } else if (task == "mean_difference") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    mx <- mean(x)
    my <- mean(y)
    ans <- mx - my
    
    templates <- c(
      sprintf(
        paste0(
          "An interim trial update reports a mean tooth length of %s for OJ and %s for VC at dose %.1f.\n\n",
          "The treatment contrast is defined as OJ minus VC. Calculate that contrast."
        ),
        fmt(mx), fmt(my), dose
      ),
      sprintf(
        paste0(
          "The results caption for dose %.1f contains the two group means below:\n",
          "OJ: %s\nVC: %s\n\n",
          "By how many units does the OJ mean differ from the VC mean? Use OJ - VC."
        ),
        dose, fmt(mx), fmt(my)
      ),
      sprintf(
        paste0(
          "A data audit has confirmed the averages but not the reported difference. ",
          "At dose %.1f, the verified means are %s for OJ and %s for VC.\n\n",
          "Recalculate the mean difference used in the report."
        ),
        dose, fmt(mx), fmt(my)
      ),
      sprintf(
        paste0(
          "Clinician: \"The OJ and VC averages are %s and %s at this dose. What is the numerical gap?\"\n\n",
          "Report OJ minus VC."
        ),
        fmt(mx), fmt(my)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Difference = ", fmt(mx), " - ", fmt(my),
      " = ", fmt(ans), "."
    )
    
  } else if (task == "standard_error") {
    
    n <- length(g)
    s <- sd(g)
    ans <- s / sqrt(n)
    
    templates <- c(
      sprintf(
        paste0(
          "The quality-control sheet for supplement %s at dose %.1f gives n = %d and sample SD = %s.\n\n",
          "Calculate the standard error of the mean tooth length."
        ),
        supp, dose, n, fmt(s)
      ),
      sprintf(
        paste0(
          "A teaching slide omits one number from the summary of a ToothGrowth subgroup:\n",
          "n = %d, s = %s.\n\n",
          "Find s/sqrt(n)."
        ),
        n, fmt(s)
      ),
      sprintf(
        paste0(
          "Before a confidence interval is produced, the analyst needs the uncertainty attached to the group mean. ",
          "For %s at dose %.1f, the sample contains %d observations with SD %s.\n\n",
          "What is the standard error?"
        ),
        supp, dose, n, fmt(s)
      ),
      sprintf(
        paste0(
          "The laboratory summary reports a standard deviation of %s from %d observations. ",
          "The standard-error field is blank.\n\n",
          "Complete it."
        ),
        fmt(s), n
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = s/sqrt(n) = ", fmt(s), "/sqrt(", n,
      ") = ", fmt(ans), "."
    )
    
  } else if (task == "one_sample_t") {
    
    mu0 <- round(mean(g) + pick(c(-3, -2, 2, 3)), 1)
    parts <- one_parts(g, mu0)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "A historical benchmark states that mean tooth length under this condition is %.1f. ",
          "For supplement %s at dose %.1f, the current study reports x-bar = %s, s = %s and n = %d.\n\n",
          "Calculate the one-sample t statistic."
        ),
        mu0, supp, dose, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "The trial monitor asks for a numerical check against the benchmark mu0 = %.1f.\n",
          "Observed summary: mean %s, SD %s, n %d.\n\n",
          "Using t = (x-bar - mu0)/(s/sqrt(n)), calculate t."
        ),
        mu0, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "A one-sample comparison appears in the appendix, but the test statistic is missing.\n\n",
          "Condition: %s, dose %.1f\n",
          "x-bar = %s, s = %s, n = %d, mu0 = %.1f\n\n",
          "Fill in the missing t value."
        ),
        supp, dose, fmt(parts$m), fmt(parts$s), parts$n, mu0
      ),
      sprintf(
        paste0(
          "Statistician: \"The benchmark is %.1f. The subgroup mean is %s from %d observations, with SD %s.\"\n\n",
          "What one-sample t statistic follows from these values?"
        ),
        mu0, fmt(parts$m), parts$n, fmt(parts$s)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(parts$s), "/sqrt(", parts$n,
      ") = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$m), " - ", mu0,
      ")/", fmt(parts$se), " = ", fmt(ans), "."
    )
    
  } else if (task == "two_sample_t") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    parts <- welch_parts(x, y)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "At dose %.1f, the OJ group has mean %s, variance %s and n = %d; ",
          "the VC group has mean %s, variance %s and n = %d.\n\n",
          "Calculate the Welch t statistic for OJ minus VC."
        ),
        dose, fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny
      ),
      sprintf(
        paste0(
          "The manuscript gives enough information to reconstruct the unequal-variance comparison:\n",
          "OJ: x-bar = %s, s^2 = %s, n = %d\n",
          "VC: x-bar = %s, s^2 = %s, n = %d\n\n",
          "What test statistic should appear in the results table?"
        ),
        fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny
      ),
      sprintf(
        paste0(
          "A dental researcher wants a quick numerical comparison of the two supplements at dose %.1f. ",
          "Use the summary statistics below to compute the Welch t value.\n\n",
          "OJ (%d observations): mean %s, variance %s\n",
          "VC (%d observations): mean %s, variance %s"
        ),
        dose, parts$nx, fmt(parts$mx), fmt(parts$vx),
        parts$ny, fmt(parts$my), fmt(parts$vy)
      ),
      sprintf(
        paste0(
          "Reviewer comment: \"Please verify the t statistic for the OJ-versus-VC contrast at dose %.1f.\"\n\n",
          "The group summaries are (%s, %s, %d) for OJ and (%s, %s, %d) for VC, ",
          "where each triple is mean, variance and sample size."
        ),
        dose,
        fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = sqrt(", fmt(parts$vx), "/", parts$nx,
      " + ", fmt(parts$vy), "/", parts$ny,
      ") = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$mx), " - ", fmt(parts$my),
      ")/", fmt(parts$se), " = ", fmt(ans), "."
    )
    
  } else {
    
    n <- length(g)
    s <- sd(g)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    ans <- t_star * se
    
    templates <- c(
      sprintf(
        paste0(
          "The 95%% confidence interval for supplement %s at dose %.1f is being assembled. ",
          "The group has n = %d, SD = %s, and the relevant t critical value is %s.\n\n",
          "Calculate the margin of error."
        ),
        supp, dose, n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "A results table shows the sample mean but leaves the plus-or-minus quantity blank.\n",
          "Use n = %d, s = %s and t* = %s.\n\n",
          "What margin should accompany the mean?"
        ),
        n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "For a small ToothGrowth subgroup, the analyst has already selected the 95%% critical value %s. ",
          "With SD %s from %d observations, find t* x SE."
        ),
        fmt(t_star), fmt(s), n
      ),
      sprintf(
        paste0(
          "Trial note: \"The interval centre is ready; only the margin remains.\"\n",
          "Group size %d, sample SD %s, t critical %s.\n\n",
          "Calculate the missing margin of error."
        ),
        n, fmt(s), fmt(t_star)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(s), "/sqrt(", n, ") = ", fmt(se), ".\n",
      "Margin = ", fmt(t_star), " x ", fmt(se),
      " = ", fmt(ans), "."
    )
  }
  
  make_record(
    id = sprintf("R800_011_%03d", i),
    blueprint_id = "R800_011",
    dataset_name = "ToothGrowth",
    difficulty = "easy",
    scenario = "healthcare",
    template_id = paste0("t_test_template_", task),
    style = style,
    cognitive_skill = "single_step_numerical_calculation",
    variables_used = "len, supp, dose",
    question = question,
    reference_answer = fmt(ans),
    solution_steps = solution,
    answer_type = "numeric"
  )
}

# ============================================================
# R800_012 — ToothGrowth / Healthcare / Medium / Calculation
# Linked calculations + inferential decision/comparison
# ============================================================

medium_tg_tasks <- c(
  "welch_test_decision",
  "confidence_interval_decision",
  "two_dose_contrast",
  "effect_size_and_test",
  "one_sample_test_decision",
  "pooled_vs_welch",
  "power_precision_change",
  "dose_response_comparison"
)

medium_tg_styles <- c(
  "trial_protocol_query",
  "peer_review_response",
  "clinical_meeting",
  "analysis_plan",
  "manuscript_revision",
  "teaching_vignette",
  "audit_investigation",
  "statistical_consultation",
  "conference_abstract",
  "results_reconciliation"
)

medium_tg_question <- function(i) {
  
  task <- medium_tg_tasks[(i - 1) %% length(medium_tg_tasks) + 1]
  style <- pick(medium_tg_styles)
  dose <- pick(sort(unique(TG$dose)))
  
  if (task == "welch_test_decision") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    parts <- welch_parts(x, y)
    tst <- t.test(x, y)
    alpha <- pick(c(0.05, 0.01))
    decision <- ifelse(tst$p.value < alpha, "reject H0", "do not reject H0")
    
    templates <- c(
      sprintf(
        paste0(
          "The interim analysis plan specifies a two-sided Welch test for the OJ-versus-VC comparison at dose %.1f. ",
          "The verified summaries are:\n",
          "OJ: n = %d, mean = %s, variance = %s\n",
          "VC: n = %d, mean = %s, variance = %s\n\n",
          "Calculate the standard error, t statistic, Welch degrees of freedom and two-sided p-value. ",
          "Using alpha = %.2f, state the decision."
        ),
        dose, parts$nx, fmt(parts$mx), fmt(parts$vx),
        parts$ny, fmt(parts$my), fmt(parts$vy), alpha
      ),
      sprintf(
        paste0(
          "Peer-review response, statistical point 3\n\n",
          "The reviewer asks whether OJ and VC differ at dose %.1f. Reconstruct the full unequal-variance test from the group summaries: ",
          "(%d, %s, %s) for OJ and (%d, %s, %s) for VC, where each triple is n, mean and variance.\n\n",
          "Report SE, t, df, p and the conclusion at alpha = %.2f."
        ),
        dose,
        parts$nx, fmt(parts$mx), fmt(parts$vx),
        parts$ny, fmt(parts$my), fmt(parts$vy),
        alpha
      ),
      sprintf(
        paste0(
          "At a clinical research meeting, two analysts disagree about the dose-%.1f supplement comparison. ",
          "One wants only the difference in means; the other requests the complete Welch calculation.\n\n",
          "OJ: mean %s, variance %s, n %d\n",
          "VC: mean %s, variance %s, n %d\n\n",
          "Resolve the disagreement by computing t, approximate df and the two-sided p-value, then make the alpha = %.2f decision."
        ),
        dose,
        fmt(parts$mx), fmt(parts$vx), parts$nx,
        fmt(parts$my), fmt(parts$vy), parts$ny,
        alpha
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "SE = ", fmt(parts$se),
      "; t = ", fmt(parts$t),
      "; df = ", fmt(parts$df),
      "; p ", p_text(tst$p.value),
      "; ", decision
    )
    solution <- paste0(
      "SE = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$mx), " - ", fmt(parts$my),
      ")/", fmt(parts$se), " = ", fmt(parts$t), ".\n",
      "Welch df = ", fmt(parts$df), ".\n",
      "Two-sided p ", p_text(tst$p.value), ".\n",
      "At alpha = ", alpha, ", ", decision, "."
    )
    
  } else if (task == "confidence_interval_decision") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    tst <- t.test(x, y, conf.level = 0.95)
    diff <- mean(x) - mean(y)
    excludes_zero <- tst$conf.int[1] > 0 || tst$conf.int[2] < 0
    
    templates <- c(
      sprintf(
        paste0(
          "A manuscript revision replaces a bare p-value with a 95%% confidence interval for the mean difference OJ - VC at dose %.1f.\n\n",
          "Using the ToothGrowth observations for that dose, calculate the estimated difference and its Welch confidence interval. ",
          "Then state whether the interval excludes zero."
        ),
        dose
      ),
      sprintf(
        paste0(
          "The conference abstract must report both magnitude and uncertainty. For dose %.1f, construct the 95%% confidence interval for mu_OJ - mu_VC from the real data.\n\n",
          "Give the point estimate, lower and upper limits, and the corresponding inference about equality of means."
        ),
        dose
      ),
      sprintf(
        paste0(
          "A clinical collaborator asks, \"How large might the supplement difference plausibly be at dose %.1f?\"\n\n",
          "Answer with the Welch 95%% interval for OJ minus VC, and indicate whether zero is a plausible value."
        ),
        dose
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "difference = ", fmt(diff),
      "; 95% CI [", fmt(tst$conf.int[1]), ", ",
      fmt(tst$conf.int[2]), "]; ",
      ifelse(excludes_zero, "excludes zero", "includes zero")
    )
    solution <- paste0(
      "Estimated difference = ", fmt(diff), ".\n",
      "Welch 95% CI = [", fmt(tst$conf.int[1]), ", ",
      fmt(tst$conf.int[2]), "].\n",
      ifelse(
        excludes_zero,
        "Because zero is outside the interval, the two-sided 5% test rejects equality of means.",
        "Because zero lies inside the interval, the two-sided 5% test does not reject equality of means."
      )
    )
    
  } else if (task == "two_dose_contrast") {
    
    doses <- sort(sample(sort(unique(TG$dose)), 2, replace = FALSE))
    supp <- pick(levels(TG$supp))
    x <- subset(TG, supp == supp & dose == doses[1])$len
    y <- subset(TG, supp == supp & dose == doses[2])$len
    parts <- welch_parts(x, y)
    tst <- t.test(x, y)
    diff <- mean(y) - mean(x)
    
    templates <- c(
      sprintf(
        paste0(
          "The dose-escalation section of a healthcare report compares %s at doses %.1f and %.1f. ",
          "The question is not merely whether the means differ, but by how much the higher-dose group exceeds the lower-dose group.\n\n",
          "Calculate the mean increase, the Welch t statistic for lower dose minus higher dose, and the two-sided p-value."
        ),
        supp, doses[1], doses[2]
      ),
      sprintf(
        paste0(
          "During a dose-review meeting, the %s arm is examined at %.1f and %.1f. ",
          "Use the real ToothGrowth values to quantify the increase in mean len and to test the difference with Welch's method.\n\n",
          "Report the increase, t and p."
        ),
        supp, doses[1], doses[2]
      ),
      sprintf(
        paste0(
          "A results-reconciliation check focuses on the %s supplement. ",
          "The lower and higher dose groups are %.1f and %.1f.\n\n",
          "Compute: (1) higher-dose mean minus lower-dose mean, ",
          "(2) the Welch statistic using lower minus higher, and (3) the two-sided p-value."
        ),
        supp, doses[1], doses[2]
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "increase = ", fmt(diff),
      "; t(lower-higher) = ", fmt(parts$t),
      "; p ", p_text(tst$p.value)
    )
    solution <- paste0(
      "Higher-dose increase = ", fmt(mean(y)), " - ",
      fmt(mean(x)), " = ", fmt(diff), ".\n",
      "Welch t for lower - higher = ", fmt(parts$t), ".\n",
      "Two-sided p ", p_text(tst$p.value), "."
    )
    
  } else if (task == "effect_size_and_test") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    d <- cohens_d(x, y)
    tst <- t.test(x, y)
    diff <- mean(x) - mean(y)
    
    templates <- c(
      sprintf(
        paste0(
          "A reviewer argues that the dose-%.1f comparison should include an effect size, not only a significance test.\n\n",
          "Using OJ minus VC, calculate the raw mean difference, Cohen's d based on the pooled SD, and the Welch two-sided p-value."
        ),
        dose
      ),
      sprintf(
        paste0(
          "The statistical consultation notes that a p-value alone does not express the size of the treatment contrast. ",
          "For dose %.1f, report the OJ - VC mean difference, pooled-SD Cohen's d and Welch p-value."
        ),
        dose
      ),
      sprintf(
        paste0(
          "To complete a conference table, three numbers are required for the dose-%.1f supplement comparison: ",
          "the difference in means, the standardised difference and the two-sided p-value.\n\n",
          "Calculate all three from ToothGrowth."
        ),
        dose
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "difference = ", fmt(diff),
      "; Cohen's d = ", fmt(d),
      "; p ", p_text(tst$p.value)
    )
    solution <- paste0(
      "Mean difference = ", fmt(diff), ".\n",
      "Cohen's d = ", fmt(d), ".\n",
      "Welch two-sided p ", p_text(tst$p.value), "."
    )
    
  } else if (task == "one_sample_test_decision") {
    
    supp <- pick(levels(TG$supp))
    g <- subset(TG, supp == supp & dose == dose)$len
    mu0 <- round(mean(g) + pick(c(-4, -3, 3, 4)), 1)
    parts <- one_parts(g, mu0)
    tst <- t.test(g, mu = mu0)
    alpha <- 0.05
    decision <- ifelse(tst$p.value < alpha, "reject H0", "do not reject H0")
    
    templates <- c(
      sprintf(
        paste0(
          "A historical control value of %.1f is used to benchmark the %s group at dose %.1f. ",
          "The current subgroup has mean %s, SD %s and n = %d.\n\n",
          "Calculate SE, t and the two-sided p-value, then make the 5%% decision."
        ),
        mu0, supp, dose, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "The trial protocol defines H0: mu = %.1f for %s at dose %.1f. ",
          "Using the current data, carry out the complete one-sample calculation and state whether the benchmark is rejected at alpha = 0.05."
        ),
        mu0, supp, dose
      ),
      sprintf(
        paste0(
          "A monitoring committee asks whether the observed mean for %s at dose %.1f is compatible with the benchmark %.1f.\n\n",
          "Report t, df, p and the decision."
        ),
        supp, dose, mu0
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "SE = ", fmt(parts$se),
      "; t = ", fmt(parts$t),
      "; df = ", parts$df,
      "; p ", p_text(tst$p.value),
      "; ", decision
    )
    solution <- paste0(
      "SE = ", fmt(parts$se), ".\n",
      "t = ", fmt(parts$t), ", df = ", parts$df, ".\n",
      "Two-sided p ", p_text(tst$p.value), ".\n",
      decision, " at alpha = 0.05."
    )
    
  } else if (task == "pooled_vs_welch") {
    
    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    welch <- t.test(x, y, var.equal = FALSE)
    pooled <- t.test(x, y, var.equal = TRUE)
    
    templates <- c(
      sprintf(
        paste0(
          "Two analysts have produced different test outputs for the supplement comparison at dose %.1f. ",
          "One assumed equal variances; the other used Welch's method.\n\n",
          "Calculate both t statistics and both degrees of freedom, then report the absolute difference between the two t values."
        ),
        dose
      ),
      sprintf(
        paste0(
          "A statistical audit asks whether the equal-variance assumption materially changes the dose-%.1f comparison.\n\n",
          "From the ToothGrowth data, obtain the pooled t and df, the Welch t and df, and |t_pooled - t_Welch|."
        ),
        dose
      ),
      sprintf(
        paste0(
          "The analysis plan is being revised. Before choosing a default test, compare the numerical outputs of pooled and Welch two-sample t-tests at dose %.1f.\n\n",
          "Report both t values, both dfs and their absolute t difference."
        ),
        dose
      )
    )
    
    question <- pick(templates)
    tdif <- abs(unname(pooled$statistic) - unname(welch$statistic))
    answer <- paste0(
      "pooled t = ", fmt(pooled$statistic),
      ", df = ", fmt(pooled$parameter),
      "; Welch t = ", fmt(welch$statistic),
      ", df = ", fmt(welch$parameter),
      "; |difference| = ", fmt(tdif)
    )
    solution <- paste0(
      "Pooled test: t = ", fmt(pooled$statistic),
      ", df = ", fmt(pooled$parameter), ".\n",
      "Welch test: t = ", fmt(welch$statistic),
      ", df = ", fmt(welch$parameter), ".\n",
      "Absolute t difference = ", fmt(tdif), "."
    )
    
  } else if (task == "power_precision_change") {
    
    supp <- pick(levels(TG$supp))
    g <- subset(TG, supp == supp)$len
    s <- sd(g)
    n1 <- length(g)
    n2 <- floor(n1 / 2)
    se1 <- s / sqrt(n1)
    se2 <- s / sqrt(n2)
    ratio <- se2 / se1
    pct <- (ratio - 1) * 100
    
    templates <- c(
      sprintf(
        paste0(
          "A budget revision would reduce the %s supplement sample from %d observations to %d, while the SD is expected to remain near %s.\n\n",
          "Calculate the original and reduced standard errors, the ratio SE_reduced/SE_original, and the percentage increase in SE."
        ),
        supp, n1, n2, fmt(s)
      ),
      sprintf(
        paste0(
          "The project manager proposes halving the available %s observations. ",
          "Using SD = %s, compare precision at n = %d and n = %d.\n\n",
          "Report both SEs and how much larger the reduced-sample SE would be in percentage terms."
        ),
        supp, fmt(s), n1, n2
      ),
      sprintf(
        paste0(
          "A sensitivity calculation is required before the ToothGrowth analysis is finalised. ",
          "For the %s observations, use s = %s to quantify the effect of changing n from %d to %d on the standard error."
        ),
        supp, fmt(s), n1, n2
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "SE_original = ", fmt(se1),
      "; SE_reduced = ", fmt(se2),
      "; ratio = ", fmt(ratio),
      "; increase = ", fmt(pct, 1), "%"
    )
    solution <- paste0(
      "SE_original = ", fmt(se1), ".\n",
      "SE_reduced = ", fmt(se2), ".\n",
      "Ratio = ", fmt(ratio), ".\n",
      "Percentage increase = ", fmt(pct, 1), "%."
    )
    
  } else {
    
    doses <- sort(unique(TG$dose))
    d_low <- doses[1]
    d_high <- doses[length(doses)]
    
    oj_low <- mean(subset(TG, dose == d_low & supp == "OJ")$len)
    vc_low <- mean(subset(TG, dose == d_low & supp == "VC")$len)
    oj_high <- mean(subset(TG, dose == d_high & supp == "OJ")$len)
    vc_high <- mean(subset(TG, dose == d_high & supp == "VC")$len)
    
    contrast_low <- oj_low - vc_low
    contrast_high <- oj_high - vc_high
    change_in_contrast <- contrast_high - contrast_low
    
    templates <- c(
      sprintf(
        paste0(
          "The treatment team suspects that the OJ-versus-VC difference is not constant across dose. ",
          "At dose %.1f, the means are OJ %s and VC %s; at dose %.1f, they are OJ %s and VC %s.\n\n",
          "Calculate the supplement contrast at each dose and then the change in contrast from low dose to high dose."
        ),
        d_low, fmt(oj_low), fmt(vc_low),
        d_high, fmt(oj_high), fmt(vc_high)
      ),
      sprintf(
        paste0(
          "A results meeting focuses on whether the supplement gap narrows or widens across the dose range.\n\n",
          "Low dose %.1f: OJ %s, VC %s\n",
          "High dose %.1f: OJ %s, VC %s\n\n",
          "Find both OJ - VC contrasts and the high-minus-low contrast change."
        ),
        d_low, fmt(oj_low), fmt(vc_low),
        d_high, fmt(oj_high), fmt(vc_high)
      ),
      sprintf(
        paste0(
          "The manuscript currently reports only separate group means. Convert them into a more informative comparison.\n\n",
          "Using the lowest and highest doses, compute OJ - VC at each dose, then compute how much that difference changes."
        )
      )
    )
    
    question <- pick(templates)
    answer <- paste0(
      "low-dose contrast = ", fmt(contrast_low),
      "; high-dose contrast = ", fmt(contrast_high),
      "; change = ", fmt(change_in_contrast)
    )
    solution <- paste0(
      "Low-dose contrast = ", fmt(oj_low), " - ",
      fmt(vc_low), " = ", fmt(contrast_low), ".\n",
      "High-dose contrast = ", fmt(oj_high), " - ",
      fmt(vc_high), " = ", fmt(contrast_high), ".\n",
      "Change in contrast = ", fmt(contrast_high), " - ",
      fmt(contrast_low), " = ", fmt(change_in_contrast), "."
    )
  }
  
  make_record(
    id = sprintf("R800_012_%03d", i),
    blueprint_id = "R800_012",
    dataset_name = "ToothGrowth",
    difficulty = "medium",
    scenario = "healthcare",
    template_id = paste0("t_test_template_", task),
    style = style,
    cognitive_skill = "multi_step_inference_and_numerical_decision",
    variables_used = "len, supp, dose",
    question = question,
    reference_answer = answer,
    solution_steps = solution,
    answer_type = "multi_value_numeric_or_decision"
  )
}

# ============================================================
# R800_014 — iris / Agriculture / Easy / Calculation
# One principal numerical operation per question
# ============================================================

easy_ir_tasks <- c(
  "mean_from_values",
  "species_mean_difference",
  "standard_error",
  "one_sample_t",
  "two_sample_t",
  "margin_of_error"
)

easy_ir_styles <- c(
  "field_station_note",
  "breeding_record",
  "greenhouse_log",
  "extension_example",
  "seed_quality_check",
  "course_assessment",
  "research_table",
  "agronomy_dialogue"
)

easy_ir_question <- function(i) {
  
  task <- easy_ir_tasks[(i - 1) %% length(easy_ir_tasks) + 1]
  style <- pick(easy_ir_styles)
  trait <- pick(c("Sepal.Length", "Petal.Length"))
  species <- pick(levels(IR$Species))
  g <- IR[IR$Species == species, trait]
  
  if (task == "mean_from_values") {
    
    shown <- sample(g, 6, replace = FALSE)
    ans <- mean(shown)
    
    templates <- c(
      sprintf(
        paste0(
          "Field station note\n\n",
          "Six %s measurements from %s plants were retained after a routine quality check: %s.\n\n",
          "Calculate the mean length for this batch."
        ),
        trait, species, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "A breeding record lists six observations but leaves the batch average blank.\n",
          "Species: %s\nTrait: %s\nMeasurements: %s\n\n",
          "Complete the missing mean."
        ),
        species, trait, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Greenhouse log: %s was measured for six %s flowers, giving %s.\n\n",
          "What average should be entered in the log?"
        ),
        trait, species, paste(shown, collapse = ", ")
      ),
      sprintf(
        paste0(
          "Agronomist: \"I only need the average for this six-plant check.\"\n",
          "Assistant: \"The %s values are %s.\"\n\n",
          "Calculate the requested mean."
        ),
        trait, paste(shown, collapse = ", ")
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Mean = (", paste(shown, collapse = " + "), ")/6 = ",
      fmt(ans), "."
    )
    
  } else if (task == "species_mean_difference") {
    
    sp <- sample(levels(IR$Species), 2, replace = FALSE)
    x <- IR[IR$Species == sp[1], trait]
    y <- IR[IR$Species == sp[2], trait]
    mx <- mean(x)
    my <- mean(y)
    ans <- mx - my
    
    templates <- c(
      sprintf(
        paste0(
          "A plant-breeding report gives mean %s values of %s for %s and %s for %s.\n\n",
          "Calculate the difference using %s minus %s."
        ),
        trait, fmt(mx), sp[1], fmt(my), sp[2], sp[1], sp[2]
      ),
      sprintf(
        paste0(
          "Two iris groups are being compared before a greenhouse selection round.\n",
          "%s mean for %s: %s\n",
          "%s mean for %s: %s\n\n",
          "Find the signed difference between the two means in the order shown."
        ),
        trait, sp[1], fmt(mx), trait, sp[2], fmt(my)
      ),
      sprintf(
        paste0(
          "The extension bulletin reports the two averages but not their gap. ",
          "For %s, the means are %s (%s) and %s (%s).\n\n",
          "What is the first mean minus the second?"
        ),
        trait, fmt(mx), sp[1], fmt(my), sp[2]
      ),
      sprintf(
        paste0(
          "Seed-quality check: compare the average %s of %s with that of %s. ",
          "The verified means are %s and %s.\n\n",
          "Calculate the difference."
        ),
        trait, sp[1], sp[2], fmt(mx), fmt(my)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "Difference = ", fmt(mx), " - ", fmt(my),
      " = ", fmt(ans), "."
    )
    
  } else if (task == "standard_error") {
    
    n <- length(g)
    s <- sd(g)
    ans <- s / sqrt(n)
    
    templates <- c(
      sprintf(
        paste0(
          "The greenhouse summary for %s %s reports n = %d and sample SD = %s.\n\n",
          "Calculate the standard error of the mean."
        ),
        species, trait, n, fmt(s)
      ),
      sprintf(
        paste0(
          "A seed-company quality table is missing the SE for %s in %s. ",
          "Use s = %s and n = %d."
        ),
        trait, species, fmt(s), n
      ),
      sprintf(
        paste0(
          "During an agricultural statistics class, students are given SD %s from %d %s observations.\n\n",
          "Find s/sqrt(n)."
        ),
        fmt(s), n, species
      ),
      sprintf(
        paste0(
          "The field report lists the variability and sample size but not the precision of the mean: ",
          "s = %s, n = %d.\n\n",
          "Calculate the SE."
        ),
        fmt(s), n
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(s), "/sqrt(", n, ") = ",
      fmt(ans), "."
    )
    
  } else if (task == "one_sample_t") {
    
    mu0 <- round(mean(g) + pick(c(-0.5, -0.3, 0.3, 0.5)), 1)
    parts <- one_parts(g, mu0)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "A breeding benchmark sets the expected %s for %s at %.1f. ",
          "The observed sample has mean %s, SD %s and n = %d.\n\n",
          "Calculate the one-sample t statistic."
        ),
        trait, species, mu0, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "The field station wants to compare the %s mean for %s with the reference value %.1f.\n",
          "Summary: x-bar = %s, s = %s, n = %d.\n\n",
          "Find t."
        ),
        trait, species, mu0, fmt(parts$m), fmt(parts$s), parts$n
      ),
      sprintf(
        paste0(
          "A course assessment provides the following plant summary:\n",
          "species %s, trait %s, mean %s, SD %s, n %d, benchmark %.1f.\n\n",
          "Compute the one-sample test statistic."
        ),
        species, trait, fmt(parts$m), fmt(parts$s), parts$n, mu0
      ),
      sprintf(
        paste0(
          "Agronomist: \"Our benchmark is %.1f. The sample mean is %s from %d plants, with SD %s.\"\n\n",
          "What t statistic should be reported?"
        ),
        mu0, fmt(parts$m), parts$n, fmt(parts$s)
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(parts$s), "/sqrt(", parts$n,
      ") = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$m), " - ", mu0,
      ")/", fmt(parts$se), " = ", fmt(ans), "."
    )
    
  } else if (task == "two_sample_t") {
    
    sp <- sample(levels(IR$Species), 2, replace = FALSE)
    x <- IR[IR$Species == sp[1], trait]
    y <- IR[IR$Species == sp[2], trait]
    parts <- welch_parts(x, y)
    ans <- parts$t
    
    templates <- c(
      sprintf(
        paste0(
          "A cultivar comparison uses %s as the response. ",
          "%s has mean %s, variance %s and n = %d; ",
          "%s has mean %s, variance %s and n = %d.\n\n",
          "Calculate the Welch t statistic for %s minus %s."
        ),
        trait,
        sp[1], fmt(parts$mx), fmt(parts$vx), parts$nx,
        sp[2], fmt(parts$my), fmt(parts$vy), parts$ny,
        sp[1], sp[2]
      ),
      sprintf(
        paste0(
          "The breeding station has summarised two species but not completed the test calculation.\n\n",
          "%s: mean %s, variance %s, n %d\n",
          "%s: mean %s, variance %s, n %d\n\n",
          "Find the unequal-variance t statistic in the order shown."
        ),
        sp[1], fmt(parts$mx), fmt(parts$vx), parts$nx,
        sp[2], fmt(parts$my), fmt(parts$vy), parts$ny
      ),
      sprintf(
        paste0(
          "An extension specialist is checking whether the reported %s comparison is numerically correct. ",
          "Use the real iris summaries for %s and %s to compute the Welch t value."
        ),
        trait, sp[1], sp[2]
      ),
      sprintf(
        paste0(
          "Greenhouse analyst: \"Please reconstruct the t statistic for the %s comparison between %s and %s.\"\n\n",
          "Use the species means, variances and sample sizes from iris."
        ),
        trait, sp[1], sp[2]
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(parts$se), ".\n",
      "t = (", fmt(parts$mx), " - ",
      fmt(parts$my), ")/", fmt(parts$se),
      " = ", fmt(ans), "."
    )
    
  } else {
    
    n <- length(g)
    s <- sd(g)
    se <- s / sqrt(n)
    t_star <- qt(0.975, df = n - 1)
    ans <- t_star * se
    
    templates <- c(
      sprintf(
        paste0(
          "The 95%% interval for mean %s in %s is being prepared. ",
          "The sample has n = %d, SD = %s and t* = %s.\n\n",
          "Calculate the margin of error."
        ),
        trait, species, n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "A greenhouse report leaves the plus-or-minus value blank. ",
          "For %s %s, use n = %d, s = %s and t* = %s."
        ),
        species, trait, n, fmt(s), fmt(t_star)
      ),
      sprintf(
        paste0(
          "The agricultural extension example already gives the sample mean. ",
          "Only the 95%% confidence margin remains to be calculated from s = %s, n = %d and t* = %s."
        ),
        fmt(s), n, fmt(t_star)
      ),
      sprintf(
        paste0(
          "Before the field summary is published, compute t* x SE for the %s measurements of %s. ",
          "Use t* = %s, s = %s and n = %d."
        ),
        trait, species, fmt(t_star), fmt(s), n
      )
    )
    
    question <- pick(templates)
    solution <- paste0(
      "SE = ", fmt(s), "/sqrt(", n, ") = ", fmt(se), ".\n",
      "Margin = ", fmt(t_star), " x ", fmt(se),
      " = ", fmt(ans), "."
    )
  }
  
  make_record(
    id = sprintf("R800_014_%03d", i),
    blueprint_id = "R800_014",
    dataset_name = "iris",
    difficulty = "easy",
    scenario = "agriculture",
    template_id = paste0("t_test_template_", task),
    style = style,
    cognitive_skill = "single_step_numerical_calculation",
    variables_used = "Sepal.Length, Petal.Length, Species",
    question = question,
    reference_answer = fmt(ans),
    solution_steps = solution,
    answer_type = "numeric"
  )
}

# ============================================================
# Generate
# ============================================================

R800_011 <- do.call(rbind, lapply(seq_len(25), easy_tg_question))
R800_012 <- do.call(rbind, lapply(seq_len(25), medium_tg_question))
R800_014 <- do.call(rbind, lapply(seq_len(20), easy_ir_question))

ALL <- rbind(R800_011, R800_012, R800_014)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_011) == 25)
stopifnot(nrow(R800_012) == 25)
stopifnot(nrow(R800_014) == 20)
stopifnot(nrow(ALL) == 70)
stopifnot(length(unique(ALL$id)) == 70)
stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))
stopifnot(!any(ALL$question == ""))
stopifnot(!any(ALL$reference_answer == ""))

# Difficulty separation checks
stopifnot(all(R800_011$cognitive_skill == "single_step_numerical_calculation"))
stopifnot(all(R800_014$cognitive_skill == "single_step_numerical_calculation"))
stopifnot(all(R800_012$cognitive_skill == "multi_step_inference_and_numerical_decision"))

# ============================================================
# Export
# ============================================================

write.csv(R800_011, "R800_011_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(R800_012, "R800_012_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(R800_014, "R800_014_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ALL, "R800_011_012_014_questions_v2.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

jsonlite::write_json(
  R800_011, "R800_011_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)
jsonlite::write_json(
  R800_012, "R800_012_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)
jsonlite::write_json(
  R800_014, "R800_014_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)
jsonlite::write_json(
  ALL, "R800_011_012_014_questions_v2.json",
  pretty = TRUE, auto_unbox = TRUE, na = "null"
)

print(
  ALL[, c(
    "id", "difficulty", "scenario",
    "language_style", "template_id",
    "reference_answer"
  )]
)

cat(
  "\nV2 generated successfully:\n",
  "R800_011: 25 easy ToothGrowth questions\n",
  "R800_012: 25 medium ToothGrowth questions\n",
  "R800_014: 20 easy iris questions\n"
)