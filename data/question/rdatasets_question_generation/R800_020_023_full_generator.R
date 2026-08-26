# ============================================================
# R800_020 + R800_023
#
# R800_020
# Confidence Interval / ToothGrowth / Finance
# Hard / Short Answer / 15
#
# R800_023
# Confidence Interval / ToothGrowth / Transportation
# Hard / Interpretation / 15
#
# Output:
#   R800_020_023_questions.csv
#   R800_020_023_questions.json
#
# Design principles:
# - Real R data from ToothGrowth
# - Strongly varied discourse forms and sentence structures
# - Hard difficulty through interpretation, critique, justification,
#   comparison of intervals, precision, assumptions and decision risk
# - One combined CSV and one combined JSON only
# ============================================================

set.seed(80002023)

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

make_record <- function(
    id,
    blueprint_id,
    scenario,
    template_id,
    language_style,
    presentation_layout,
    cognitive_skill,
    statistical_output,
    question,
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
    difficulty = "hard",
    scenario = scenario,
    language_style = language_style,
    presentation_layout = presentation_layout,
    cognitive_skill = cognitive_skill,
    question_type = ifelse(
      blueprint_id == "R800_020",
      "short_answer",
      "interpretation"
    ),
    variables_used = "len, supp, dose",
    statistical_output = statistical_output,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v1.0",
    stringsAsFactors = FALSE
  )
}

compose_prompt <- function(context, output_text, task_text, layout_id) {
  if (layout_id == 1) {
    paste0(
      context,
      "\n\nStatistical evidence:\n",
      output_text,
      "\n\n",
      task_text
    )
  } else if (layout_id == 2) {
    paste0(
      task_text,
      "\n\nUse the following output:\n",
      output_text,
      "\n\nContext:\n",
      context
    )
  } else if (layout_id == 3) {
    paste0(
      context,
      "\n\n",
      task_text,
      "\n\nRelevant figures:\n",
      output_text
    )
  } else {
    paste0(
      "Relevant figures:\n",
      output_text,
      "\n\n",
      context,
      "\n\n",
      task_text
    )
  }
}

# ============================================================
# R800_020 — Finance / Hard / Short Answer
#
# ToothGrowth values are treated as anonymised financial scores:
# len  -> outcome score / return proxy / risk-adjusted performance measure
# supp -> strategy type
# dose -> exposure level
# ============================================================

finance_styles <- c(
  "investment_committee_note",
  "risk_memo",
  "analyst_call",
  "fund_review",
  "audit_query",
  "client_letter_draft",
  "valuation_debate",
  "portfolio_research_brief",
  "compliance_review",
  "market_commentary",
  "decision_log",
  "board_pack_extract"
)

finance_openings <- list(

  investment_committee_note = c(
    "An investment committee is reviewing two strategy variants using anonymised performance scores derived from the ToothGrowth dataset.",
    "A committee paper compares average strategy outcomes at a fixed exposure level and asks how much confidence should be placed in the estimated difference.",
    "Before a capital-allocation decision is made, the committee wants the interval estimate interpreted rather than merely quoted."
  ),

  risk_memo = c(
    "A risk memo reports a mean difference but does not discuss the uncertainty around that estimate.",
    "The risk team is checking whether the interval is narrow enough to support a practical decision.",
    "A model-risk review asks whether the reported confidence interval has been interpreted too aggressively."
  ),

  analyst_call = c(
    "Portfolio manager: \"The point estimate looks attractive. How stable is it?\"\nAnalyst: \"The interval gives a better sense of the uncertainty.\"",
    "Senior analyst: \"Does the confidence interval rule out a zero advantage?\"\nAssociate: \"We need to inspect both limits before answering.\"",
    "Client strategist: \"Can we say one strategy is definitely superior?\"\nQuant analyst: \"Not without qualifying the interval and design.\""
  ),

  fund_review = c(
    "A quarterly fund review uses the two supplement groups as anonymised strategy labels.",
    "The research team compares average outcome scores across two hypothetical investment approaches.",
    "A fund-selection exercise asks whether the estimated advantage is both statistically and economically meaningful."
  ),

  audit_query = c(
    "An internal audit query challenges a statement that treats the confidence interval as a guarantee.",
    "The audit team asks whether the interval was constructed and communicated appropriately.",
    "A control review identifies a mismatch between the confidence limits and the conclusion in the report."
  ),

  client_letter_draft = c(
    "A draft client letter contains a strong claim based on a confidence interval for a mean difference.",
    "The communication team wants a statistically accurate version of the performance comparison.",
    "A client-facing note reports an estimated advantage but omits the width and uncertainty of the interval."
  ),

  valuation_debate = c(
    "Two analysts disagree about whether a wide interval should support a high-conviction view.",
    "A valuation debate focuses on whether the estimated mean advantage is precise enough to matter.",
    "The discussion turns from the midpoint of the interval to the range of plausible values."
  ),

  portfolio_research_brief = c(
    "A portfolio research brief compares two anonymised strategies at the same exposure level.",
    "The quantitative research team is assessing both effect size and confidence-interval precision.",
    "A strategy note asks how the conclusion changes when the confidence level is increased."
  ),

  compliance_review = c(
    "Compliance asks whether the phrase 'statistically proven' is justified by the interval shown.",
    "A marketing-compliance review checks that the uncertainty has not been hidden behind a point estimate.",
    "The legal review requests a more defensible explanation of the estimated strategy difference."
  ),

  market_commentary = c(
    "A market commentary turns a sample mean difference into a broad performance claim.",
    "An analyst note needs to distinguish a plausible range from a certain outcome.",
    "A commentary draft highlights the positive point estimate while ignoring the lower confidence bound."
  ),

  decision_log = c(
    "A decision log records the evidence available at the time of a strategy choice.",
    "The committee wants the reasoning documented, including what the interval can and cannot rule out.",
    "A formal decision note must state whether the evidence is precise enough for action."
  ),

  board_pack_extract = c(
    "A board pack contains a compact table of point estimates and confidence limits.",
    "Directors ask whether the uncertainty range supports the wording used in the recommendation.",
    "A governance review focuses on whether the statistical conclusion matches the interval."
  )
)

finance_tasks <- c(
  "evaluate_positive_interval_claim",
  "evaluate_zero_inclusion",
  "practical_vs_statistical_precision",
  "compare_90_95_decision",
  "interval_width_and_risk",
  "assumption_limitations",
  "sample_size_and_precision",
  "client_statement_rewrite",
  "economic_threshold_reasoning",
  "subgroup_aggregation_warning",
  "asymmetric_decision_loss",
  "one_mean_benchmark_reasoning",
  "compare_two_intervals",
  "replication_and_stability",
  "decision_under_interval_uncertainty"
)

generate_r800_020 <- function(i) {

  task_type <- finance_tasks[i]
  style <- pick(finance_styles)
  context <- pick(finance_openings[[style]])
  layout_id <- sample(1:4, 1)

  dose <- pick(sort(unique(TG$dose)))

  x <- subset(TG, dose == dose & supp == "OJ")$len
  y <- subset(TG, dose == dose & supp == "VC")$len

  ci95 <- welch_diff_ci(x, y, 0.95)
  ci90 <- welch_diff_ci(x, y, 0.90)
  ci99 <- welch_diff_ci(x, y, 0.99)

  if (task_type == "evaluate_positive_interval_claim") {

    output_text <- paste0(
      "Estimated strategy difference (OJ - VC) = ",
      fmt(ci95$diff),
      "\n95% CI = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "A committee member says, \"Because the point estimate is positive, OJ is clearly superior.\" Evaluate this claim using the confidence interval and explain what a defensible conclusion would look like.",
      "Assess whether the positive midpoint alone is sufficient evidence of an advantage. Your answer should use both confidence limits.",
      "Explain why a positive estimate does not automatically imply a reliable positive population difference."
    ))

    reference_answer <- paste0(
      "The point estimate is positive, but the conclusion depends on the full interval. ",
      ifelse(
        contains_zero(ci95$lower, ci95$upper),
        "Because the 95% interval includes zero, the data remain compatible with no population mean advantage.",
        "Because the 95% interval excludes zero and is entirely positive, the data support a positive population mean difference."
      ),
      " The interval should be reported as a range of plausible values rather than as certainty."
    )

    solution_steps <- paste0(
      "1. Inspect the signs of both limits.\n",
      "2. Check whether zero is included.\n",
      "3. Distinguish a sample point estimate from evidence about the population mean difference.\n",
      "4. State the conclusion with uncertainty."
    )

  } else if (task_type == "evaluate_zero_inclusion") {

    output_text <- paste0(
      "Mean difference = ", fmt(ci95$diff),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain the decision implications of zero being inside or outside the interval. Avoid reducing the answer to a mechanical significance label.",
      "What does the interval say about the possibility of no average advantage, and how should that affect confidence in the strategy choice?",
      "Interpret the interval from the perspective of a cautious investment decision."
    ))

    reference_answer <- if (contains_zero(ci95$lower, ci95$upper)) {
      paste0(
        "Zero lies inside the interval, so no average strategy difference remains plausible. ",
        "The evidence is therefore insufficient for a high-conviction claim of superiority, although economically relevant positive or negative effects may still fall inside the interval."
      )
    } else {
      paste0(
        "Zero lies outside the interval, so the data provide evidence of a non-zero population mean difference. ",
        "However, the committee should still consider whether the range of plausible effects is large enough to matter economically."
      )
    }

    solution_steps <- paste0(
      "Use zero for the statistical question, then use the size and range of the interval for the practical decision."
    )

  } else if (task_type == "practical_vs_statistical_precision") {

    threshold <- pick(c(1, 2, 3))

    output_text <- paste0(
      "Estimated difference = ", fmt(ci95$diff),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nMinimum economically meaningful advantage = ",
      threshold
    )

    task_text <- pick(c(
      "Discuss whether the interval supports an economically meaningful advantage, not merely a non-zero one.",
      "The committee requires at least the stated threshold before reallocating capital. Evaluate whether the interval supports that decision.",
      "Separate the statistical question from the economic threshold and give a justified recommendation."
    ))

    reference_answer <- paste0(
      "A non-zero interval is not enough for the economic decision. ",
      "The relevant question is whether the plausible range lies above the threshold of ",
      threshold, ". ",
      ifelse(
        ci95$lower > threshold,
        "Because the entire interval exceeds the threshold, the evidence supports an economically meaningful advantage.",
        ifelse(
          ci95$upper < threshold,
          "Because the interval does not reach the threshold, the evidence does not support the required economic advantage.",
          "Because the interval crosses the threshold, the evidence is too uncertain to confirm that the required economic advantage has been achieved."
        )
      )
    )

    solution_steps <- paste0(
      "Compare the lower and upper bounds with the economic threshold, not only with zero."
    )

  } else if (task_type == "compare_90_95_decision") {

    output_text <- paste0(
      "90% CI = [", fmt(ci90$lower),
      ", ", fmt(ci90$upper), "]",
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain why the 95% interval is wider and how the choice of confidence level changes the strength of the conclusion.",
      "Compare the two intervals from a risk-management perspective. Why might a decision appear clearer at 90% than at 95%?",
      "Discuss the trade-off between confidence and precision shown by these two intervals."
    ))

    reference_answer <- paste0(
      "The 95% interval is wider because greater confidence requires a larger critical value. ",
      "The 90% interval is more precise but carries a higher long-run miss rate. ",
      "A conclusion that depends on using the narrower 90% interval should be treated more cautiously because it is less conservative."
    )

    solution_steps <- paste0(
      "Explain critical value, width and the confidence-precision trade-off."
    )

  } else if (task_type == "interval_width_and_risk") {

    output_text <- paste0(
      "Point estimate = ", fmt(ci95$diff),
      "\n95% interval width = ", fmt(ci95$width),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain what the interval width says about estimation risk and why a wide interval weakens a high-conviction recommendation.",
      "How should the width influence the committee's confidence in the point estimate?",
      "Interpret the interval width as a measure of uncertainty in the strategy comparison."
    ))

    reference_answer <- paste0(
      "The width of ", fmt(ci95$width),
      " reflects uncertainty around the estimated mean difference. ",
      "A wider interval means a broader range of plausible population effects and therefore greater estimation risk. ",
      "The midpoint should not be treated as stable when the interval remains wide."
    )

    solution_steps <- paste0(
      "Relate interval width to uncertainty, precision and decision confidence."
    )

  } else if (task_type == "assumption_limitations") {

    output_text <- paste0(
      "OJ: n = ", length(x),
      ", mean = ", fmt(mean(x)),
      ", SD = ", fmt(sd(x)),
      "\nVC: n = ", length(y),
      ", mean = ", fmt(mean(y)),
      ", SD = ", fmt(sd(y)),
      "\nMethod: Welch t interval"
    )

    task_text <- pick(c(
      "Identify the assumptions and limitations that should be discussed before treating this interval as reliable financial evidence.",
      "Explain why independence, outliers and distribution shape matter for the interval.",
      "What features of the data-generating process could make the reported confidence interval misleading?"
    ))

    reference_answer <- paste0(
      "The observations should be independent within and between groups, and neither group should contain severe outliers or extreme non-normality that would undermine the t approximation. ",
      "The interval also assumes that the sample represents the target population and that the comparison is not distorted by omitted structure such as dose composition or repeated measurements. ",
      "Welch's method relaxes equal-variance assumptions but does not solve design or representativeness problems."
    )

    solution_steps <- paste0(
      "Discuss independence, distributional shape, outliers, representativeness and the scope of Welch's robustness."
    )

  } else if (task_type == "sample_size_and_precision") {

    n_current <- length(x)
    n_new <- n_current * 4
    se_current <- ci95$se
    se_new <- se_current / 2

    output_text <- paste0(
      "Current group sizes = ", n_current,
      " and ", length(y),
      "\nCurrent SE of difference = ",
      fmt(se_current),
      "\nHypothetical plan: quadruple both group sizes while variances remain similar"
    )

    task_text <- pick(c(
      "Explain approximately how the standard error and interval width would change if both group sizes were quadrupled.",
      "The committee proposes a much larger study. Quantify the expected precision gain and explain the square-root relationship.",
      "Why does quadrupling sample size not reduce uncertainty by a factor of four?"
    ))

    reference_answer <- paste0(
      "Standard error scales approximately with 1/sqrt(n). ",
      "Quadrupling both group sizes would therefore reduce the SE from about ",
      fmt(se_current), " to about ", fmt(se_new),
      ", roughly halving the confidence-interval width rather than reducing it by a factor of four."
    )

    solution_steps <- paste0(
      "Use SE proportional to 1/sqrt(n): sqrt(4) = 2, so SE and width are approximately halved."
    )

  } else if (task_type == "client_statement_rewrite") {

    output_text <- paste0(
      "Draft wording:\n",
      "\"OJ is proven to outperform VC by ",
      fmt(ci95$diff), " units.\"\n\n",
      "95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Rewrite the sentence so that it is suitable for a client report and accurately reflects uncertainty.",
      "Identify the statistical problems in the draft and provide a compliant replacement.",
      "Produce a concise statement that reports direction, magnitude and confidence limits without using proof language."
    ))

    reference_answer <- paste0(
      "At dose ", dose,
      ", the estimated mean difference between OJ and VC is ",
      fmt(ci95$diff),
      " units, with a 95% confidence interval from ",
      fmt(ci95$lower), " to ", fmt(ci95$upper),
      ". This wording reports the estimate and uncertainty without claiming certainty or universal superiority."
    )

    solution_steps <- paste0(
      "Remove 'proven', identify the comparison and dose, report the point estimate and interval, and avoid individual-level guarantees."
    )

  } else if (task_type == "economic_threshold_reasoning") {

    threshold <- pick(c(-1, 0, 1, 2))

    output_text <- paste0(
      "95% CI for OJ - VC = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]",
      "\nDecision threshold = ", threshold
    )

    task_text <- pick(c(
      "Explain whether the interval supports clearing the stated decision threshold.",
      "How should the committee classify the evidence relative to this hurdle?",
      "Give a threshold-based interpretation rather than a zero-based one."
    ))

    reference_answer <- paste0(
      ifelse(
        ci95$lower > threshold,
        "The entire interval lies above the threshold, so the evidence supports clearing it.",
        ifelse(
          ci95$upper < threshold,
          "The entire interval lies below the threshold, so the evidence does not support clearing it.",
          "The interval crosses the threshold, so the evidence is inconclusive relative to the hurdle."
        )
      ),
      " The relevant benchmark for this decision is ", threshold,
      ", not automatically zero."
    )

    solution_steps <- paste0(
      "Compare both interval limits with the decision threshold."
    )

  } else if (task_type == "subgroup_aggregation_warning") {

    x_all <- subset(TG, supp == "OJ")$len
    y_all <- subset(TG, supp == "VC")$len
    overall_ci <- welch_diff_ci(x_all, y_all, 0.95)

    output_text <- paste0(
      "Dose-specific 95% CI at dose ", dose,
      " = [", fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]",
      "\nOverall 95% CI across all doses = [",
      fmt(overall_ci$lower), ", ",
      fmt(overall_ci$upper), "]"
    )

    task_text <- pick(c(
      "Explain why the overall interval may differ from the dose-specific interval and why aggregation can mislead the decision.",
      "A manager prefers the pooled interval because it uses more observations. Critique that reasoning.",
      "Why should dose composition be considered before using the overall comparison?"
    ))

    reference_answer <- paste0(
      "The overall interval combines observations from different dose levels, and dose strongly affects len. ",
      "Pooling may change the estimated difference and its uncertainty because the supplement groups can have different dose compositions or because the supplement contrast may vary by dose. ",
      "A larger pooled sample does not correct confounding or effect heterogeneity."
    )

    solution_steps <- paste0(
      "Identify dose as an important structural variable and distinguish more data from better adjustment."
    )

  } else if (task_type == "asymmetric_decision_loss") {

    output_text <- paste0(
      "95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nDecision context: downside errors are considered more costly than missed upside"
    )

    task_text <- pick(c(
      "Explain why the lower confidence bound may matter more than the point estimate in this decision.",
      "How does asymmetric loss change the way the interval should be used?",
      "A committee is more concerned about downside than missed opportunity. Interpret the interval accordingly."
    ))

    reference_answer <- paste0(
      "When downside errors are more costly, the lower bound is especially important because it represents a conservative estimate of the plausible effect. ",
      "A positive midpoint may be insufficient if the lower bound allows a materially negative or inadequate outcome. ",
      "Decision rules should reflect the asymmetric consequences rather than treating all estimation errors equally."
    )

    solution_steps <- paste0(
      "Use the lower bound as a conservative decision input and connect it to asymmetric loss."
    )

  } else if (task_type == "one_mean_benchmark_reasoning") {

    group <- subset(TG, dose == dose & supp == pick(levels(TG$supp)))$len
    ci_mean <- one_mean_ci(group, 0.95)
    benchmark <- round(mean(group) + pick(c(-2, -1, 1, 2)), 1)

    output_text <- paste0(
      "Sample mean = ", fmt(ci_mean$mean),
      "\n95% CI for population mean = [",
      fmt(ci_mean$lower), ", ",
      fmt(ci_mean$upper), "]",
      "\nBenchmark = ", benchmark
    )

    task_text <- pick(c(
      "Evaluate whether the benchmark is compatible with the interval and explain what that means.",
      "Can the benchmark be ruled out at the 5% level? Justify using the interval.",
      "Interpret the interval relative to the benchmark without saying the benchmark is 'true' or 'false'."
    ))

    benchmark_inside <- benchmark >= ci_mean$lower &&
      benchmark <= ci_mean$upper

    reference_answer <- paste0(
      "The benchmark is ",
      ifelse(benchmark_inside, "inside", "outside"),
      " the 95% confidence interval. ",
      ifelse(
        benchmark_inside,
        "Therefore, it remains compatible with the data at the 5% two-sided level.",
        "Therefore, it is not compatible with the interval and would be rejected by the corresponding two-sided 5% test."
      ),
      " This does not assign a probability that the benchmark itself is true."
    )

    solution_steps <- paste0(
      "Compare the benchmark with both limits and connect interval inclusion to the corresponding hypothesis test."
    )

  } else if (task_type == "compare_two_intervals") {

    g1 <- subset(TG, dose == dose & supp == "OJ")$len
    g2 <- subset(TG, dose == dose & supp == "VC")$len

    a <- one_mean_ci(g1, 0.95)
    b <- one_mean_ci(g2, 0.95)

    output_text <- paste0(
      "OJ mean CI = [", fmt(a$lower),
      ", ", fmt(a$upper), "]",
      "\nVC mean CI = [", fmt(b$lower),
      ", ", fmt(b$upper), "]",
      "\nWelch CI for OJ - VC = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain why judging significance by visually checking whether the two separate mean intervals overlap is inferior to using the direct interval for the mean difference.",
      "A manager looks only at overlap between the two group intervals. Critique that method.",
      "Why is the confidence interval for OJ - VC the correct object for the comparison?"
    ))

    reference_answer <- paste0(
      "Separate confidence intervals estimate two population means, not their difference. ",
      "Their overlap is not equivalent to a formal 5% test of equality. ",
      "The direct Welch interval for OJ - VC incorporates the covariance structure implied by independent samples and answers the comparison question directly."
    )

    solution_steps <- paste0(
      "Distinguish intervals for individual means from an interval for the contrast."
    )

  } else if (task_type == "replication_and_stability") {

    output_text <- paste0(
      "Observed 95% CI = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]",
      "\nGroup sizes = ", length(x),
      " and ", length(y)
    )

    task_text <- pick(c(
      "Explain why an independent replication could produce a different point estimate and interval even if the underlying population effect is unchanged.",
      "How should the committee think about stability across repeated studies?",
      "Why is one confidence interval not a guarantee that a future interval will have the same centre or limits?"
    ))

    reference_answer <- paste0(
      "Sampling variability causes point estimates, standard errors and confidence limits to change from sample to sample. ",
      "Even with the same population effect, a replication may produce a different centre and width. ",
      "The confidence procedure has long-run coverage properties; a single realised interval is not fixed across studies."
    )

    solution_steps <- paste0(
      "Explain sampling variability and long-run coverage rather than deterministic replication."
    )

  } else {

    threshold <- pick(c(0, 1, 2))

    output_text <- paste0(
      "Estimated difference = ", fmt(ci95$diff),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nDecision threshold = ", threshold,
      "\nInterval width = ", fmt(ci95$width)
    )

    task_text <- pick(c(
      "Write a short decision note that balances expected advantage, threshold uncertainty and interval width.",
      "Recommend whether to act now, wait for more evidence or reject the strategy comparison. Justify the recommendation.",
      "Give a high-quality committee response using the full interval rather than the midpoint alone."
    ))

    reference_answer <- paste0(
      "The decision should depend on where the full interval lies relative to the threshold of ",
      threshold, ". ",
      ifelse(
        ci95$lower > threshold,
        "Because the entire interval exceeds the threshold, acting is supported by the current evidence.",
        ifelse(
          ci95$upper < threshold,
          "Because the entire interval remains below the threshold, the evidence does not support acting.",
          "Because the interval crosses the threshold, the evidence is inconclusive and further data would reduce decision risk."
        )
      ),
      " The interval width of ", fmt(ci95$width),
      " should also be recognised as a measure of estimation uncertainty."
    )

    solution_steps <- paste0(
      "Compare the interval with the decision threshold, then incorporate width and the value of additional information."
    )
  }

  full_question <- compose_prompt(
    context,
    output_text,
    task_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_020_%03d", i),
    blueprint_id = "R800_020",
    scenario = "finance",
    template_id = paste0(
      "ci_t_interval_template_",
      task_type
    ),
    language_style = style,
    presentation_layout = paste0(
      "layout_",
      layout_id
    ),
    cognitive_skill = "critical_interval_reasoning_and_decision_justification",
    statistical_output = output_text,
    question = full_question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_short_answer"
  )
}

# ============================================================
# R800_023 — Transportation / Hard / Interpretation
#
# ToothGrowth values are treated as anonymised transport metrics:
# len  -> efficiency / response metric
# supp -> vehicle or operating strategy
# dose -> operating intensity
# ============================================================

transport_styles <- c(
  "fleet_performance_report",
  "transport_authority_brief",
  "route_planning_note",
  "engineering_debrief",
  "operations_control_memo",
  "consultant_review",
  "safety_board_discussion",
  "depot_comparison",
  "policy_appraisal",
  "rail_or_bus_case",
  "analyst_dialogue",
  "infrastructure_review"
)

transport_openings <- list(

  fleet_performance_report = c(
    "A fleet-performance report uses anonymised ToothGrowth values as an operating-efficiency metric.",
    "The analytics team compares two vehicle strategies at a fixed operating intensity.",
    "A fleet review focuses on the uncertainty around the average difference between two operating modes."
  ),

  transport_authority_brief = c(
    "A transport authority briefing includes a confidence interval for a mean performance metric.",
    "The authority wants the interval translated into operational language.",
    "A public-sector transport report asks whether the observed difference is precise enough for policy use."
  ),

  route_planning_note = c(
    "A route-planning note compares average outcomes under two scheduling strategies.",
    "Planners want to know whether the estimated advantage remains plausible after accounting for uncertainty.",
    "The route team is reviewing confidence limits before changing operating practice."
  ),

  engineering_debrief = c(
    "An engineering debrief reports a point estimate and confidence interval for a performance contrast.",
    "The technical team is checking whether the interval supports a meaningful operational difference.",
    "Engineers are discussing why the lower and upper bounds matter more than the midpoint alone."
  ),

  operations_control_memo = c(
    "An operations-control memo summarises the estimated mean difference between two modes.",
    "Control-room analysts want a clear interpretation of interval width and zero inclusion.",
    "The operations team is deciding whether the evidence is strong enough to standardise one approach."
  ),

  consultant_review = c(
    "A transport consultant reviews the interval calculation before it enters a client presentation.",
    "The consulting team is asked to explain the result without overstating certainty.",
    "A methodological review focuses on whether the confidence interval has been interpreted in context."
  ),

  safety_board_discussion = c(
    "Board member: \"The estimate is positive. Can we switch immediately?\"\nAnalyst: \"We should examine the full interval and decision threshold first.\"",
    "Engineer: \"The interval crosses zero. Does that mean the strategies are identical?\"\nStatistician: \"No, it means the evidence is not decisive.\"",
    "Operations director: \"Why is the 99% interval much wider?\"\nConsultant: \"Higher confidence comes with lower precision.\""
  ),

  depot_comparison = c(
    "A depot-comparison study uses two operating groups as anonymised transport conditions.",
    "The performance unit compares average outcomes across two depots at the same workload.",
    "A technical note asks whether the estimated depot difference is operationally important."
  ),

  policy_appraisal = c(
    "A transport policy appraisal includes a confidence interval for a mean difference.",
    "The policy team needs an interpretation that separates evidence from practical importance.",
    "A decision paper evaluates whether the confidence interval clears a minimum policy threshold."
  ),

  rail_or_bus_case = c(
    "A transit case study compares two operating strategies using an anonymised performance measure.",
    "A service-planning exercise interprets a confidence interval under different confidence levels.",
    "A vehicle-management example asks whether the uncertainty is small enough for deployment."
  ),

  analyst_dialogue = c(
    "Planner: \"The confidence interval is wide. What does that imply for the decision?\"\nAnalyst: \"It means the plausible range of effects is broad.\"",
    "Manager: \"If zero is included, can we say there is no difference?\"\nStatistician: \"No, only that no difference remains plausible.\"",
    "Engineer: \"Would collecting more observations help?\"\nAnalyst: \"Yes, because larger samples generally reduce standard error.\""
  ),

  infrastructure_review = c(
    "An infrastructure review uses the comparison as a training example in uncertainty-aware decision-making.",
    "A governance panel wants the confidence interval explained before approving a change.",
    "The transport programme board is reviewing the statistical basis for an operational recommendation."
  )
)

transport_tasks <- c(
  "interpret_interval_direction",
  "interpret_zero_and_decision",
  "interpret_width_precision",
  "interpret_90_vs_99",
  "interpret_threshold_crossing",
  "interpret_mean_interval",
  "interpret_group_interval_overlap",
  "interpret_sample_size_effect",
  "interpret_welch_interval",
  "interpret_negative_lower_bound",
  "interpret_practical_relevance",
  "interpret_extrapolation_of_result",
  "interpret_reporting_language",
  "interpret_replication_uncertainty",
  "interpret_operational_recommendation"
)

generate_r800_023 <- function(i) {

  task_type <- transport_tasks[i]
  style <- pick(transport_styles)
  context <- pick(transport_openings[[style]])
  layout_id <- sample(1:4, 1)

  dose <- pick(sort(unique(TG$dose)))
  x <- subset(TG, dose == dose & supp == "OJ")$len
  y <- subset(TG, dose == dose & supp == "VC")$len

  ci95 <- welch_diff_ci(x, y, 0.95)
  ci90 <- welch_diff_ci(x, y, 0.90)
  ci99 <- welch_diff_ci(x, y, 0.99)

  if (task_type == "interpret_interval_direction") {

    output_text <- paste0(
      "Difference defined as OJ - VC",
      "\nEstimate = ", fmt(ci95$diff),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Interpret the direction and plausible size of the operating-mode difference.",
      "Explain what the signs of the estimate and interval limits imply.",
      "Translate this interval into an operational comparison."
    ))

    reference_answer <- paste0(
      "The point estimate indicates that OJ is ",
      ifelse(ci95$diff > 0, "higher", "lower"),
      " on average than VC by ", fmt(abs(ci95$diff)),
      " units. ",
      ifelse(
        ci95$lower > 0,
        "Because the full interval is positive, the population mean difference is supported as positive.",
        ifelse(
          ci95$upper < 0,
          "Because the full interval is negative, the population mean difference is supported as negative.",
          "Because the interval crosses zero, the direction of the population mean difference remains uncertain."
        )
      )
    )

    solution_steps <- paste0(
      "Interpret the point estimate first, then inspect the signs of both limits."
    )

  } else if (task_type == "interpret_zero_and_decision") {

    output_text <- paste0(
      "95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain what zero inclusion or exclusion means for the operational decision.",
      "Can the authority conclude that the two operating strategies differ on average?",
      "Interpret the interval without saying that inclusion of zero proves equality."
    ))

    reference_answer <- if (contains_zero(ci95$lower, ci95$upper)) {
      paste0(
        "Zero is included, so no population mean difference remains compatible with the data. ",
        "The authority cannot claim a statistically clear average difference at the 5% two-sided level, but exact equality has not been proved."
      )
    } else {
      paste0(
        "Zero is excluded, so the interval supports a non-zero population mean difference at the 5% two-sided level. ",
        "Operational relevance still depends on whether the range is large enough to matter."
      )
    }

    solution_steps <- paste0(
      "Check zero inclusion, connect it to the corresponding two-sided test, and avoid equality claims."
    )

  } else if (task_type == "interpret_width_precision") {

    output_text <- paste0(
      "95% CI width = ", fmt(ci95$width),
      "\nSE = ", fmt(ci95$se),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Interpret the width as a measure of estimation precision.",
      "What does this width imply about confidence in the operating comparison?",
      "Why might the authority request more data even if the point estimate appears favourable?"
    ))

    reference_answer <- paste0(
      "The interval width of ", fmt(ci95$width),
      " indicates the range of plausible population differences. ",
      "A broad interval means low precision and a greater risk that the true operational effect differs materially from the point estimate. ",
      "More data would generally reduce the SE and narrow the interval."
    )

    solution_steps <- paste0(
      "Connect width to precision, decision risk and the value of additional data."
    )

  } else if (task_type == "interpret_90_vs_99") {

    output_text <- paste0(
      "90% CI = [", fmt(ci90$lower),
      ", ", fmt(ci90$upper), "]",
      "\n99% CI = [", fmt(ci99$lower),
      ", ", fmt(ci99$upper), "]"
    )

    task_text <- pick(c(
      "Explain why the 99% interval is wider and how this affects the apparent decisiveness of the result.",
      "Compare the confidence-precision trade-off between these two intervals.",
      "Why might a result appear operationally clearer at 90% confidence than at 99%?"
    ))

    reference_answer <- paste0(
      "The 99% interval is wider because a higher confidence level uses a larger critical value. ",
      "It offers stronger long-run coverage but less precision. ",
      "A conclusion that is clear at 90% but not at 99% is more sensitive to the chosen confidence standard."
    )

    solution_steps <- paste0(
      "Explain the role of the critical value and the trade-off between confidence and width."
    )

  } else if (task_type == "interpret_threshold_crossing") {

    threshold <- pick(c(0, 1, 2, 3))

    output_text <- paste0(
      "95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nMinimum operationally worthwhile difference = ",
      threshold
    )

    task_text <- pick(c(
      "Assess whether the evidence supports clearing the operational threshold.",
      "Interpret the interval relative to the minimum worthwhile effect.",
      "Should the programme proceed if this threshold is required? Justify."
    ))

    reference_answer <- paste0(
      ifelse(
        ci95$lower > threshold,
        "The entire interval exceeds the threshold, so the evidence supports an operationally worthwhile difference.",
        ifelse(
          ci95$upper < threshold,
          "The interval remains below the threshold, so the required effect is not supported.",
          "The interval crosses the threshold, so the evidence is inconclusive about whether the worthwhile effect has been achieved."
        )
      )
    )

    solution_steps <- paste0(
      "Compare both confidence limits with the operational threshold."
    )

  } else if (task_type == "interpret_mean_interval") {

    mode <- pick(levels(TG$supp))
    group <- subset(TG, dose == dose & supp == mode)$len
    mean_ci <- one_mean_ci(group, 0.95)

    output_text <- paste0(
      "Mode = ", mode,
      "\nOperating intensity = ", dose,
      "\nMean metric = ", fmt(mean_ci$mean),
      "\n95% CI = [", fmt(mean_ci$lower),
      ", ", fmt(mean_ci$upper), "]"
    )

    task_text <- pick(c(
      "Interpret this interval as an estimate of the population mean operating metric.",
      "What does the interval say, and what does it not say about individual vehicles or journeys?",
      "Translate the interval into a cautious operational statement."
    ))

    reference_answer <- paste0(
      "The interval gives a plausible range for the population mean metric under mode ",
      mode, " at operating intensity ", dose,
      ". It does not mean that 95% of individual vehicles or journeys fall inside these limits, nor does it guarantee the next observation."
    )

    solution_steps <- paste0(
      "Distinguish a confidence interval for a population mean from a range for individual observations."
    )

  } else if (task_type == "interpret_group_interval_overlap") {

    a <- one_mean_ci(x, 0.95)
    b <- one_mean_ci(y, 0.95)

    output_text <- paste0(
      "OJ mean CI = [", fmt(a$lower),
      ", ", fmt(a$upper), "]",
      "\nVC mean CI = [", fmt(b$lower),
      ", ", fmt(b$upper), "]",
      "\nDirect CI for OJ - VC = [",
      fmt(ci95$lower), ", ",
      fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain why the direct interval for the difference is preferable to judging significance from overlap between the two separate intervals.",
      "A planner compares only the two mean intervals. Why is that an imperfect method?",
      "Which interval directly answers the transport comparison question, and why?"
    ))

    reference_answer <- paste0(
      "The separate intervals estimate two different population means. ",
      "Their overlap is not equivalent to a formal test of equality. ",
      "The direct Welch interval for OJ - VC estimates the contrast itself and therefore answers the comparison question."
    )

    solution_steps <- paste0(
      "Distinguish individual-mean intervals from a contrast interval."
    )

  } else if (task_type == "interpret_sample_size_effect") {

    output_text <- paste0(
      "Current SE = ", fmt(ci95$se),
      "\nCurrent group sizes = ", length(x),
      " and ", length(y),
      "\nHypothetical plan: double both sample sizes"
    )

    task_text <- pick(c(
      "Explain approximately how the SE and interval width would change if both sample sizes doubled.",
      "Why would doubling the data not halve the uncertainty?",
      "Quantify the expected precision gain using the square-root rule."
    ))

    new_se <- ci95$se / sqrt(2)

    reference_answer <- paste0(
      "SE scales approximately with 1/sqrt(n). ",
      "Doubling both group sizes would reduce the SE from ",
      fmt(ci95$se), " to about ", fmt(new_se),
      ", a reduction by a factor of 1/sqrt(2), not by one half. ",
      "The interval width would shrink by roughly the same factor."
    )

    solution_steps <- paste0(
      "Use the square-root sample-size relationship."
    )

  } else if (task_type == "interpret_welch_interval") {

    output_text <- paste0(
      "Welch df = ", fmt(ci95$df),
      "\nSE = ", fmt(ci95$se),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain why Welch's method is appropriate and why the df may be non-integer.",
      "What feature of the two-group comparison is reflected in the Welch degrees of freedom?",
      "Interpret the role of Welch's adjustment in the interval."
    ))

    reference_answer <- paste0(
      "Welch's interval does not assume equal population variances. ",
      "Its degrees of freedom are estimated from the two sample variances and sample sizes using an approximation, so the value may be non-integer. ",
      "This adjustment produces a more reliable interval when variability differs between groups."
    )

    solution_steps <- paste0(
      "Explain unequal-variance robustness and approximate df."
    )

  } else if (task_type == "interpret_negative_lower_bound") {

    output_text <- paste0(
      "Point estimate = ", fmt(ci95$diff),
      "\nLower bound = ", fmt(ci95$lower),
      "\nUpper bound = ", fmt(ci95$upper)
    )

    task_text <- pick(c(
      "The midpoint is positive, but the lower bound may be negative. Explain the operational meaning.",
      "Why should the planner not focus only on the favourable point estimate?",
      "Interpret the downside represented by the lower confidence bound."
    ))

    reference_answer <- paste0(
      "A positive point estimate represents the sample's best estimate, but a negative lower bound means the population effect could plausibly favour the opposite direction. ",
      "For operational decisions, this downside uncertainty should be considered rather than hidden by the midpoint."
    )

    solution_steps <- paste0(
      "Contrast the point estimate with the conservative lower bound."
    )

  } else if (task_type == "interpret_practical_relevance") {

    output_text <- paste0(
      "Estimated mean difference = ", fmt(ci95$diff),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Explain why statistical evidence and operational importance are separate issues.",
      "What additional information is needed before deciding whether this difference matters for transport performance?",
      "Interpret the interval from both statistical and practical perspectives."
    ))

    reference_answer <- paste0(
      "The interval addresses uncertainty about the population mean difference, but practical relevance depends on the measurement units, operating costs, safety implications and the minimum effect worth acting on. ",
      "A statistically non-zero difference may still be operationally trivial, while a potentially important effect may remain imprecisely estimated."
    )

    solution_steps <- paste0(
      "Separate evidence from practical magnitude and identify context-specific thresholds."
    )

  } else if (task_type == "interpret_extrapolation_of_result") {

    output_text <- paste0(
      "Interval estimated at operating intensity = ", dose,
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nProposed use: apply the result to a much higher operating intensity not present in the data"
    )

    task_text <- pick(c(
      "Explain why this confidence interval should not automatically be transferred to the new operating intensity.",
      "What is the extrapolation problem in using this interval outside the observed condition?",
      "Why may the estimated difference change under a different operating intensity?"
    ))

    reference_answer <- paste0(
      "The interval applies to the population mean difference under the observed operating intensity of ",
      dose, ". ",
      "Applying it to a much higher unobserved intensity assumes the same group contrast and variability continue unchanged, which has not been established. ",
      "A separate model including operating intensity and possible interaction would be more appropriate."
    )

    solution_steps <- paste0(
      "Identify the condition-specific scope and the unverified assumption required for extrapolation."
    )

  } else if (task_type == "interpret_reporting_language") {

    output_text <- paste0(
      "Draft statement:\n",
      "\"Mode OJ guarantees an improvement of ",
      fmt(ci95$diff), " units.\"\n",
      "95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]"
    )

    task_text <- pick(c(
      "Rewrite the statement so that it accurately reports an estimated mean difference with uncertainty.",
      "Identify the problems with the word 'guarantees' and provide a transport-report version.",
      "Produce a concise, statistically defensible sentence."
    ))

    reference_answer <- paste0(
      "At operating intensity ", dose,
      ", the estimated mean difference between OJ and VC is ",
      fmt(ci95$diff),
      " units, with a 95% confidence interval from ",
      fmt(ci95$lower), " to ", fmt(ci95$upper),
      ". This wording describes an average estimated difference and does not guarantee an outcome for every vehicle or journey."
    )

    solution_steps <- paste0(
      "Replace certainty language with estimation language and report the full interval."
    )

  } else if (task_type == "interpret_replication_uncertainty") {

    output_text <- paste0(
      "Current 95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nCurrent group sizes = ", length(x),
      " and ", length(y)
    )

    task_text <- pick(c(
      "Explain why a repeat study could produce different confidence limits even if the true population difference were unchanged.",
      "How should the transport authority understand sampling variability across replications?",
      "Why is this realised interval not a fixed property of the operating strategies?"
    ))

    reference_answer <- paste0(
      "Different random samples produce different means, variances, standard errors and confidence limits. ",
      "Even if the underlying population difference is stable, a replication will not necessarily reproduce the same interval. ",
      "The 95% procedure refers to long-run coverage across repeated samples."
    )

    solution_steps <- paste0(
      "Explain sample-to-sample variability and long-run coverage."
    )

  } else {

    threshold <- pick(c(0, 1, 2))

    output_text <- paste0(
      "Estimated difference = ", fmt(ci95$diff),
      "\n95% CI = [", fmt(ci95$lower),
      ", ", fmt(ci95$upper), "]",
      "\nOperational threshold = ", threshold,
      "\nInterval width = ", fmt(ci95$width)
    )

    task_text <- pick(c(
      "Give an operational recommendation based on the full interval, threshold and precision.",
      "Should the authority adopt OJ, retain VC or collect more evidence? Justify.",
      "Write a short interpretation suitable for a transport decision paper."
    ))

    reference_answer <- paste0(
      ifelse(
        ci95$lower > threshold,
        "The full interval exceeds the operational threshold, so adoption of OJ is supported by the current evidence.",
        ifelse(
          ci95$upper < threshold,
          "The interval remains below the operational threshold, so adoption is not supported.",
          "The interval crosses the operational threshold, so the result is inconclusive and additional evidence would be valuable."
        )
      ),
      " The interval width of ", fmt(ci95$width),
      " should be acknowledged as the remaining estimation uncertainty."
    )

    solution_steps <- paste0(
      "Compare the full interval with the threshold, then incorporate interval width and decision risk."
    )
  }

  full_question <- compose_prompt(
    context,
    output_text,
    task_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_023_%03d", i),
    blueprint_id = "R800_023",
    scenario = "transportation",
    template_id = paste0(
      "ci_t_interval_template_",
      task_type
    ),
    language_style = style,
    presentation_layout = paste0(
      "layout_",
      layout_id
    ),
    cognitive_skill = "advanced_contextual_interval_interpretation",
    statistical_output = output_text,
    question = full_question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_interpretation"
  )
}

# ============================================================
# Generate both blueprints
# ============================================================

R800_020 <- do.call(
  rbind,
  lapply(seq_len(15), generate_r800_020)
)

R800_023 <- do.call(
  rbind,
  lapply(seq_len(15), generate_r800_023)
)

ALL <- rbind(
  R800_020,
  R800_023
)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_020) == 15)
stopifnot(nrow(R800_023) == 15)
stopifnot(nrow(ALL) == 30)
stopifnot(length(unique(ALL$id)) == 30)

stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))
stopifnot(!any(is.na(ALL$statistical_output)))

stopifnot(!any(ALL$question == ""))
stopifnot(!any(ALL$reference_answer == ""))
stopifnot(!any(ALL$solution_steps == ""))

stopifnot(length(unique(R800_020$template_id)) == 15)
stopifnot(length(unique(R800_023$template_id)) == 15)

# ============================================================
# Export only one combined CSV and one combined JSON
# ============================================================

write.csv(
  ALL,
  "R800_020_023_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

jsonlite::write_json(
  ALL,
  "R800_020_023_questions.json",
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
    "scenario",
    "language_style",
    "template_id",
    "answer_type"
  )]
)

cat(
  "\nGenerated successfully:\n",
  "- R800_020: 15 Finance Hard Short Answer questions\n",
  "- R800_023: 15 Transportation Hard Interpretation questions\n",
  "- Combined CSV: R800_020_023_questions.csv\n",
  "- Combined JSON: R800_020_023_questions.json\n"
)
