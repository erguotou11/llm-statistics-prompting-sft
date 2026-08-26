# ============================================================
# R800_016 + R800_017
#
# R800_016
# t-test / ToothGrowth / General Everyday / Hard / Short Answer / 15
# Focus: written reasoning, justification, assumptions, limitations
#
# R800_017
# t-test / iris / Sports Analytics / Medium / Interpretation / 20
# Focus: interpretation of statistical output in context
#
# Design principles
# - Real R datasets and computed outputs
# - Full-scenario prompts rather than a repeated role-action template
# - Strong separation between Hard and Medium
# - Rich variation in discourse form, sentence structure and information order
# ============================================================

set.seed(80001617)

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

cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  
  pooled_sd <- sqrt(
    ((nx - 1) * var(x) + (ny - 1) * var(y)) /
      (nx + ny - 2)
  )
  
  (mean(x) - mean(y)) / pooled_sd
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
    question_type = ifelse(
      difficulty == "hard",
      "short_answer",
      "interpretation"
    ),
    variables_used = variables_used,
    statistical_output = statistical_output,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v1.1",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_016
# ToothGrowth / General Everyday / Hard / Short Answer
# ============================================================

r800_016_tasks <- c(
  "evaluate_claim_from_p_value",
  "confidence_interval_reasoning",
  "practical_vs_statistical_significance",
  "assumption_and_robustness",
  "multiple_comparisons_warning",
  "causal_claim_critique",
  "subgroup_conclusion",
  "non_significant_result_reasoning",
  "effect_size_and_decision",
  "sample_size_limitation",
  "one_sided_test_justification",
  "welch_vs_pooled_choice",
  "benchmark_claim_evaluation",
  "reporting_quality_review",
  "decision_under_uncertainty"
)

r800_016_styles <- c(
  "consumer_health_article",
  "community_forum_post",
  "personal_decision_note",
  "fact_check",
  "product_comparison_blog",
  "clinic_leaflet_review",
  "neighbourhood_discussion",
  "public_information_brief",
  "editorial_query",
  "household_choice_case",
  "podcast_transcript",
  "online_advice_column"
)

r800_016_openings <- list(
  
  consumer_health_article = c(
    "A consumer-health article summarises the ToothGrowth study for a general audience, but its statistical wording is being checked before publication.",
    "A popular health website has turned a treatment comparison into a short news item and asks for a more careful statistical explanation.",
    "An article aimed at non-specialists reports a difference between supplement groups and invites a judgement about how strong the evidence really is."
  ),
  
  community_forum_post = c(
    "A community forum post quotes a t-test result and then makes a broad claim about which supplement is better.",
    "Someone in an online discussion has copied a p-value from a data analysis without explaining what was actually tested.",
    "A public discussion about everyday supplement choices includes a statistical claim based on the ToothGrowth data."
  ),
  
  personal_decision_note = c(
    "A reader is using the ToothGrowth comparison as a simple example when deciding how much confidence to place in two competing options.",
    "A personal decision note weighs a numerical difference against the uncertainty around that difference.",
    "A non-specialist wants to know whether the observed treatment gap is large enough and reliable enough to influence a practical choice."
  ),
  
  fact_check = c(
    "A fact-checker is reviewing a claim made from the ToothGrowth dataset.",
    "A statistical fact-check is needed because a public-facing statement appears stronger than the evidence shown.",
    "An editor asks whether the reported conclusion follows from the t-test output or overstates it."
  ),
  
  product_comparison_blog = c(
    "A comparison blog presents two treatment groups as if one must always outperform the other.",
    "A product-comparison post uses the ToothGrowth data to rank two options but gives little attention to uncertainty.",
    "A consumer blog cites a difference in average tooth length and asks readers to treat it as decisive."
  ),
  
  clinic_leaflet_review = c(
    "A draft clinic leaflet uses a simple group comparison to explain evidence to patients.",
    "A public-information leaflet includes a numerical treatment contrast, but the reasoning behind the conclusion needs revision.",
    "A clinic communication team wants to ensure that the language used around the t-test is accurate and not misleading."
  ),
  
  neighbourhood_discussion = c(
    "Two neighbours disagree about whether a small observed difference should be treated as meaningful evidence.",
    "A casual discussion turns into a question about what a non-significant t-test does and does not imply.",
    "An everyday conversation about comparing two options raises a deeper question about uncertainty and sample size."
  ),
  
  public_information_brief = c(
    "A short public-information brief reports group means, a confidence interval and a p-value.",
    "A general-audience briefing note needs a balanced explanation of a two-sample comparison.",
    "A public summary includes correct numbers but an incomplete interpretation."
  ),
  
  editorial_query = c(
    "An editor has sent the statistical paragraph back with a request for justification rather than a bare conclusion.",
    "The editorial note asks whether the wording 'proves better' can be supported by the analysis.",
    "A copy editor wants the statistical statement rewritten so that it matches the actual evidence."
  ),
  
  household_choice_case = c(
    "A household-choice example uses two treatment groups to illustrate why an average difference alone may not settle a decision.",
    "A practical decision case asks whether a statistically detectable difference is also large enough to matter.",
    "An everyday choice is being framed around a t-test, but the limitations of the comparison must be made explicit."
  ),
  
  podcast_transcript = c(
    "Host: \"The p-value is below 0.05, so that settles it, right?\"\nGuest statistician: \"Not quite. We still need to discuss magnitude, uncertainty and design.\"",
    "Presenter: \"The means are different. Can we say one option causes better growth?\"\nAnalyst: \"The answer depends on what the study design and test actually support.\"",
    "Host: \"No significant result means no difference at all?\"\nStatistician: \"That is a common misunderstanding.\""
  ),
  
  online_advice_column = c(
    "An advice column receives a question about how to interpret a treatment comparison reported online.",
    "A reader asks whether a p-value alone is enough to make an everyday recommendation.",
    "An online advice response needs to distinguish statistical evidence from a guaranteed practical outcome."
  )
)

compose_hard_prompt <- function(opening, output_text, task_text, layout_id) {
  if (layout_id == 1) {
    paste0(
      opening,
      "\n\nStatistical evidence:\n",
      output_text,
      "\n\n",
      task_text
    )
  } else if (layout_id == 2) {
    paste0(
      task_text,
      "\n\nThe relevant evidence is:\n",
      output_text,
      "\n\nBackground:\n",
      opening
    )
  } else if (layout_id == 3) {
    paste0(
      opening,
      "\n\n",
      task_text,
      "\n\nUse the following statistical output in your answer:\n",
      output_text
    )
  } else {
    paste0(
      "Statistical output:\n",
      output_text,
      "\n\n",
      opening,
      "\n\n",
      task_text
    )
  }
}

generate_r800_016 <- function(i) {
  
  task_type <- r800_016_tasks[i]
  style <- pick(r800_016_styles)
  opening <- pick(r800_016_openings[[style]])
  layout_id <- sample(1:4, 1)
  
  dose <- pick(sort(unique(TG$dose)))
  
  x <- subset(TG, dose == dose & supp == "OJ")$len
  y <- subset(TG, dose == dose & supp == "VC")$len
  
  tst <- t.test(x, y, var.equal = FALSE)
  parts <- welch_parts(x, y)
  diff <- mean(x) - mean(y)
  d <- cohens_d(x, y)
  ci <- tst$conf.int
  
  if (task_type == "evaluate_claim_from_p_value") {
    
    output_text <- paste0(
      "Dose = ", dose,
      "\nMean(OJ) = ", fmt(mean(x)),
      "\nMean(VC) = ", fmt(mean(y)),
      "\nWelch t = ", fmt(tst$statistic),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "A public post concludes, \"OJ is definitely better for everyone.\" Evaluate this statement. Your answer should explain what the p-value supports, what it does not establish, and how the conclusion should be rewritten.",
      "Assess whether the evidence justifies saying that OJ will outperform VC in every case. Give a statistically careful replacement statement.",
      "The numerical result has been interpreted as a universal guarantee. Explain why that interpretation is too strong and state a defensible conclusion."
    ))
    
    reference_answer <- paste0(
      "The test provides evidence about a difference in population mean tooth length at this dose, not a guarantee for every individual observation. ",
      "A suitable conclusion is that the data provide ",
      ifelse(tst$p.value < 0.05, "evidence", "insufficient evidence"),
      " of a difference in mean tooth length between OJ and VC at dose ",
      dose,
      ". The result does not prove universal superiority or causation beyond the study conditions."
    )
    
    solution_steps <- paste0(
      "1. Compare the p-value with the chosen significance level.\n",
      "2. Interpret the test as a statement about population means.\n",
      "3. Avoid individual-level guarantees.\n",
      "4. Avoid claims extending beyond the observed treatment conditions."
    )
    
  } else if (task_type == "confidence_interval_reasoning") {
    
    output_text <- paste0(
      "Estimated mean difference (OJ - VC) = ", fmt(diff),
      "\n95% confidence interval = [",
      fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )
    
    task_text <- pick(c(
      "Explain what this interval says about the size and direction of the supplement difference. Include whether zero is compatible with the data and why the interval is more informative than reporting only the p-value.",
      "Interpret the confidence interval for a general reader. Your response should discuss direction, plausible effect sizes and the corresponding two-sided test decision.",
      "Use the interval to assess both statistical evidence and uncertainty. Do not reduce the answer to 'significant' or 'not significant'."
    ))
    
    reference_answer <- paste0(
      "The interval gives plausible values for the population mean difference OJ - VC. ",
      ifelse(
        ci[1] > 0,
        "Because the entire interval is positive, it supports a higher mean for OJ.",
        ifelse(
          ci[2] < 0,
          "Because the entire interval is negative, it supports a lower mean for OJ.",
          "Because the interval includes zero, the data are compatible with no population mean difference."
        )
      ),
      " It also shows the range of effect sizes consistent with the data, which a p-value alone does not provide."
    )
    
    solution_steps <- paste0(
      "Interpret the sign of both confidence limits, check whether zero lies inside the interval, ",
      "and describe the interval as uncertainty around the population mean difference."
    )
    
  } else if (task_type == "practical_vs_statistical_significance") {
    
    output_text <- paste0(
      "Mean difference (OJ - VC) = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Discuss separately whether the result is statistically convincing and whether the observed difference appears practically important. Explain why these are not the same question.",
      "A blog labels the result 'important' solely because the p-value is small. Evaluate that reasoning using the mean difference and Cohen's d.",
      "Write a short judgement that distinguishes evidence against equal means from the real-world magnitude of the contrast."
    ))
    
    reference_answer <- paste0(
      "Statistical significance is assessed through the p-value, whereas practical importance depends on the size of the mean difference, the standardised effect and the application context. ",
      "Here the observed difference is ", fmt(diff),
      " and Cohen's d is ", fmt(d),
      ". A small p-value does not by itself show that the effect is large enough to matter in practice."
    )
    
    solution_steps <- paste0(
      "Use the p-value for evidence against equal means, then use the raw and standardised differences for magnitude. ",
      "Conclude that statistical and practical significance require separate judgements."
    )
    
  } else if (task_type == "assumption_and_robustness") {
    
    output_text <- paste0(
      "OJ: n = ", length(x),
      ", mean = ", fmt(mean(x)),
      ", SD = ", fmt(sd(x)),
      "\nVC: n = ", length(y),
      ", mean = ", fmt(mean(y)),
      ", SD = ", fmt(sd(y)),
      "\nMethod: Welch two-sample t-test"
    )
    
    task_text <- pick(c(
      "Identify the main assumptions behind this comparison and explain why Welch's test is preferable to the pooled test when equal variances are uncertain. Also comment on what would make the conclusion less reliable.",
      "Before accepting the result, what should be checked about independence, distribution shape and unusual observations? Explain the role of Welch's method in this setting.",
      "Give a reasoned assessment of the test's robustness. Your answer should address independence, approximate normality, outliers and unequal variability."
    ))
    
    reference_answer <- paste0(
      "The observations should be independent within and between groups, and each group should not contain severe outliers or extreme non-normality, especially with modest sample sizes. ",
      "Welch's test does not require equal population variances and is therefore safer when group variability differs. ",
      "Dependence, strong outliers or major distributional irregularities could weaken the validity of the result."
    )
    
    solution_steps <- paste0(
      "Discuss independence first, then shape and outliers, then explain that Welch adjusts the standard error and degrees of freedom rather than imposing equal variances."
    )
    
  } else if (task_type == "multiple_comparisons_warning") {
    
    all_doses <- sort(unique(TG$dose))
    pvals <- sapply(
      all_doses,
      function(z) {
        a <- subset(TG, dose == z & supp == "OJ")$len
        b <- subset(TG, dose == z & supp == "VC")$len
        t.test(a, b)$p.value
      }
    )
    
    output_text <- paste0(
      "Separate OJ-versus-VC tests were run at three doses.\n",
      paste0(
        "Dose ", all_doses,
        ": p ", vapply(pvals, p_text, character(1)),
        collapse = "\n"
      )
    )
    
    task_text <- pick(c(
      "Explain why interpreting all three tests at the 5% level without adjustment increases the chance of at least one false positive. Suggest one reasonable response.",
      "A report treats each dose-specific p-value as if it were the only test performed. Critique this approach and propose an adjustment or a better modelling strategy.",
      "Why does running several separate t-tests change the error-control problem? Give a justified recommendation."
    ))
    
    reference_answer <- paste0(
      "Testing several dose-specific hypotheses inflates the family-wise probability of at least one Type I error if each test uses 0.05 independently. ",
      "Possible responses include Bonferroni or Holm adjustment, or fitting a model that analyses supplement, dose and their interaction jointly."
    )
    
    solution_steps <- paste0(
      "Recognise the family of three tests, explain accumulated false-positive risk, and recommend multiplicity control or a unified model."
    )
    
  } else if (task_type == "causal_claim_critique") {
    
    output_text <- paste0(
      "At dose ", dose,
      ", mean(OJ) - mean(VC) = ", fmt(diff),
      "\nWelch p-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "An online article says, \"Changing from VC to OJ causes tooth length to increase by exactly this amount.\" Critique both the causal wording and the use of an exact individual effect.",
      "Does this t-test output by itself justify a causal statement? Explain what additional information about the study design would be needed.",
      "Rewrite the claim so that it reflects an average group comparison rather than a guaranteed causal effect for each subject."
    ))
    
    reference_answer <- paste0(
      "The output establishes an estimated difference between group means under the observed study conditions. ",
      "A causal conclusion depends on how treatments were assigned and whether confounding was controlled. ",
      "The mean difference is not an exact effect for every subject. A careful statement is that the OJ and VC groups showed an estimated average difference of ",
      fmt(diff), " at dose ", dose, "."
    )
    
    solution_steps <- paste0(
      "Separate association from causation, note the need for random assignment and control, and distinguish an average treatment contrast from an individual response."
    )
    
  } else if (task_type == "subgroup_conclusion") {
    
    all_x <- subset(TG, supp == "OJ")$len
    all_y <- subset(TG, supp == "VC")$len
    overall_tst <- t.test(all_x, all_y)
    
    output_text <- paste0(
      "Dose-specific comparison at dose ", dose,
      ": p ", p_text(tst$p.value),
      "\nOverall comparison across all doses: p ",
      p_text(overall_tst$p.value)
    )
    
    task_text <- pick(c(
      "Explain why the dose-specific and overall conclusions may differ. Discuss the role of dose composition and why an overall comparison can obscure subgroup patterns.",
      "A reader is confused because the pooled comparison and the selected dose comparison do not tell the same story. Give a reasoned explanation.",
      "Why is it risky to ignore dose when comparing the two supplements? Relate your answer to aggregation and subgroup structure."
    ))
    
    reference_answer <- paste0(
      "The overall comparison mixes observations from different dose levels, and dose has a strong relationship with tooth length. ",
      "If the supplement groups are distributed differently across doses, or if the supplement effect changes with dose, the pooled comparison can hide or distort dose-specific patterns. ",
      "The conclusion should therefore condition on dose or use a model including dose and a supplement-by-dose interaction."
    )
    
    solution_steps <- paste0(
      "Identify dose as an important stratifying variable, explain why aggregation changes the comparison, and recommend adjusted or interaction-based analysis."
    )
    
  } else if (task_type == "non_significant_result_reasoning") {
    
    # Choose the dose whose p-value is largest to make the prompt coherent.
    all_doses <- sort(unique(TG$dose))
    dose_p <- sapply(
      all_doses,
      function(z) {
        a <- subset(TG, dose == z & supp == "OJ")$len
        b <- subset(TG, dose == z & supp == "VC")$len
        t.test(a, b)$p.value
      }
    )
    
    chosen_dose <- all_doses[which.max(dose_p)]
    x2 <- subset(TG, dose == chosen_dose & supp == "OJ")$len
    y2 <- subset(TG, dose == chosen_dose & supp == "VC")$len
    tst2 <- t.test(x2, y2)
    
    output_text <- paste0(
      "Dose = ", chosen_dose,
      "\nMean difference (OJ - VC) = ",
      fmt(mean(x2) - mean(y2)),
      "\n95% CI = [",
      fmt(tst2$conf.int[1]), ", ",
      fmt(tst2$conf.int[2]), "]",
      "\np-value ", p_text(tst2$p.value)
    )
    
    task_text <- pick(c(
      "A discussion post says, \"There is no difference at all.\" Explain why a non-significant result does not prove equality and what the confidence interval contributes.",
      "Interpret this result without using the phrase 'the treatments are the same'. Address uncertainty, sample size and the range of effects still compatible with the data.",
      "Why is 'failure to reject' not equivalent to evidence of no effect? Give a careful short answer based on the output."
    ))
    
    reference_answer <- paste0(
      "A non-significant result means the data do not provide sufficiently strong evidence against equal population means at the chosen level; it does not prove exact equality. ",
      "The confidence interval shows the range of differences still compatible with the data and may include effects that are not negligible. ",
      "Limited sample size and variability may also reduce power."
    )
    
    solution_steps <- paste0(
      "State the correct meaning of failure to reject, interpret the interval, and mention that low precision or power can leave meaningful effects unresolved."
    )
    
  } else if (task_type == "effect_size_and_decision") {
    
    output_text <- paste0(
      "Dose = ", dose,
      "\nMean difference (OJ - VC) = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Give a balanced recommendation for a general reader. Your answer should integrate the direction of the effect, its standardised magnitude, uncertainty and statistical evidence.",
      "Write a short evidence summary that uses all four quantities rather than relying on a single threshold.",
      "How should someone weigh the observed effect when making a practical choice? Justify your answer from the complete output."
    ))
    
    reference_answer <- paste0(
      "The OJ - VC difference is ", fmt(diff),
      " with Cohen's d = ", fmt(d),
      ". The confidence interval [", fmt(ci[1]), ", ",
      fmt(ci[2]), "] shows the uncertainty around the population mean difference, and the p-value is ",
      p_text(tst$p.value),
      ". A practical recommendation should consider both the likely magnitude and uncertainty, not statistical significance alone."
    )
    
    solution_steps <- paste0(
      "Combine direction, raw effect size, standardised effect size, interval width and p-value. Avoid absolute claims and state that practical relevance depends on context."
    )
    
  } else if (task_type == "sample_size_limitation") {
    
    output_text <- paste0(
      "Each supplement group at dose ", dose,
      " contains ", length(x), " observations.",
      "\nObserved SDs: OJ = ", fmt(sd(x)),
      ", VC = ", fmt(sd(y)),
      "\n95% CI width = ", fmt(diff(ci))
    )
    
    task_text <- pick(c(
      "Explain how the modest group sizes affect precision and the strength of any everyday recommendation. What would a larger sample change?",
      "A reader treats the point estimate as very stable. Critique this view using sample size, variability and interval width.",
      "Why should the conclusion remain cautious even when the observed mean difference looks noticeable?"
    ))
    
    reference_answer <- paste0(
      "With only ", length(x),
      " observations per group, the estimate is sensitive to sampling variability, especially when within-group SDs are substantial. ",
      "The confidence interval width reflects this uncertainty. A larger sample would generally reduce the standard error, narrow the interval and provide greater power to detect a true difference."
    )
    
    solution_steps <- paste0(
      "Link sample size to standard error, interval width and power, then explain why a point estimate alone overstates certainty."
    )
    
  } else if (task_type == "one_sided_test_justification") {
    
    one_sided_p <- if (unname(tst$statistic) > 0) {
      tst$p.value / 2
    } else {
      1 - tst$p.value / 2
    }
    
    output_text <- paste0(
      "Research claim: mean(OJ) > mean(VC) at dose ", dose,
      "\nObserved t = ", fmt(tst$statistic),
      "\nTwo-sided p ", p_text(tst$p.value),
      "\nCorresponding one-sided p ",
      p_text(one_sided_p)
    )
    
    task_text <- pick(c(
      "Explain when a one-sided test would be justified and why choosing it only after seeing the direction of the data is inappropriate.",
      "A writer prefers the smaller one-sided p-value. Evaluate whether that choice is defensible.",
      "What conditions must be satisfied before replacing the two-sided test with a one-sided alternative?"
    ))
    
    reference_answer <- paste0(
      "A one-sided test is justified only when the directional alternative was specified before examining the data and an effect in the opposite direction would not lead to the same substantive claim. ",
      "Choosing a one-sided test after observing the sign of the result inflates the false-positive risk and is not valid."
    )
    
    solution_steps <- paste0(
      "Discuss pre-specification, the scientific relevance of the opposite direction, and the problem of post-hoc selection."
    )
    
  } else if (task_type == "welch_vs_pooled_choice") {
    
    pooled <- t.test(x, y, var.equal = TRUE)
    
    output_text <- paste0(
      "OJ SD = ", fmt(sd(x)),
      "\nVC SD = ", fmt(sd(y)),
      "\nWelch: t = ", fmt(tst$statistic),
      ", df = ", fmt(tst$parameter),
      ", p ", p_text(tst$p.value),
      "\nPooled: t = ", fmt(pooled$statistic),
      ", df = ", fmt(pooled$parameter),
      ", p ", p_text(pooled$p.value)
    )
    
    task_text <- pick(c(
      "Which test should be preferred here, and why? Your answer should discuss the equal-variance assumption rather than simply choosing the smaller p-value.",
      "Compare the logic of Welch and pooled t-tests and justify a default choice for this analysis.",
      "A report uses the pooled result without comment. Explain whether that is adequately justified."
    ))
    
    reference_answer <- paste0(
      "Welch's test is generally preferable unless equal population variances are substantively and empirically justified. ",
      "It remains valid under unequal variances and usually performs well when variances are equal. ",
      "The choice should be based on assumptions and design, not on which method gives the more favourable p-value."
    )
    
    solution_steps <- paste0(
      "Compare the variance assumption, explain Welch's robustness, and reject method selection based on outcome."
    )
    
  } else if (task_type == "benchmark_claim_evaluation") {
    
    benchmark <- round(mean(x) + pick(c(-3, -2, 2, 3)), 1)
    one_tst <- t.test(x, mu = benchmark)
    
    output_text <- paste0(
      "Group: OJ at dose ", dose,
      "\nSample mean = ", fmt(mean(x)),
      "\nBenchmark = ", benchmark,
      "\nOne-sample t = ", fmt(one_tst$statistic),
      "\n95% CI for mean = [",
      fmt(one_tst$conf.int[1]), ", ",
      fmt(one_tst$conf.int[2]), "]",
      "\np-value ", p_text(one_tst$p.value)
    )
    
    task_text <- pick(c(
      "Evaluate the claim that the group mean is meaningfully different from the benchmark. Distinguish the test result from the size of the departure.",
      "A public summary says the benchmark is 'wrong'. Explain what the one-sample test can legitimately conclude.",
      "Interpret the one-sample result and identify one limitation of using a single benchmark value in an everyday recommendation."
    ))
    
    reference_answer <- paste0(
      "The one-sample test evaluates whether the population mean is compatible with the benchmark under the model assumptions. ",
      "The observed departure is ", fmt(mean(x) - benchmark),
      ", while the confidence interval shows plausible values for the population mean. ",
      "Rejecting the benchmark statistically does not automatically show that the difference is practically important or that the benchmark is universally inappropriate."
    )
    
    solution_steps <- paste0(
      "Interpret the hypothesis, quantify the departure, use the interval for uncertainty and separate statistical from practical conclusions."
    )
    
  } else if (task_type == "reporting_quality_review") {
    
    output_text <- paste0(
      "Draft sentence:\n",
      "\"OJ produced significantly greater growth than VC (p ",
      p_text(tst$p.value),
      ").\"\n\n",
      "Supporting values:\n",
      "Mean difference = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )
    
    task_text <- pick(c(
      "Rewrite the draft sentence so that it reports the comparison more completely and avoids implying certainty beyond the data.",
      "Identify what is missing from the sentence and provide an improved results statement.",
      "Edit the statement for statistical accuracy, including magnitude, uncertainty and the relevant treatment condition."
    ))
    
    reference_answer <- paste0(
      "At dose ", dose,
      ", the estimated mean tooth length was ", fmt(diff),
      " units higher for OJ than for VC, with a 95% confidence interval from ",
      fmt(ci[1]), " to ", fmt(ci[2]),
      " and a Welch two-sided p-value ", p_text(tst$p.value),
      ". This wording reports magnitude and uncertainty without claiming universal superiority."
    )
    
    solution_steps <- paste0(
      "Include the dose, direction and size of the contrast, the confidence interval, test method and p-value, while avoiding causal or individual-level wording."
    )
    
  } else {
    
    output_text <- paste0(
      "Dose = ", dose,
      "\nMean difference (OJ - VC) = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\np-value ", p_text(tst$p.value),
      "\nCohen's d = ", fmt(d)
    )
    
    task_text <- pick(c(
      "Imagine that a decision must be made now, despite uncertainty. Give a justified recommendation that states what the evidence favours, how uncertain it remains and what additional information would improve the decision.",
      "Write a short decision note for a non-specialist. It should neither ignore the evidence nor pretend that the result is certain.",
      "Based on this output, what would be a responsible next step? Support your answer with the estimated effect, uncertainty and limitations."
    ))
    
    reference_answer <- paste0(
      "The evidence may favour one supplement on average at dose ", dose,
      ", but the recommendation should reflect the estimated difference of ",
      fmt(diff), ", the interval [", fmt(ci[1]), ", ",
      fmt(ci[2]), "] and Cohen's d = ", fmt(d),
      ". A responsible decision would also consider replication, sample size, possible adverse outcomes and whether the observed effect is practically important."
    )
    
    solution_steps <- paste0(
      "Summarise the direction and magnitude, acknowledge uncertainty, avoid certainty claims and identify what further evidence would reduce decision risk."
    )
  }
  
  full_question <- compose_hard_prompt(
    opening,
    output_text,
    task_text,
    layout_id
  )
  
  make_record(
    id = sprintf("R800_016_%03d", i),
    blueprint_id = "R800_016",
    dataset_name = "ToothGrowth",
    difficulty = "hard",
    scenario = "general_everyday",
    template_id = paste0("t_test_template_", task_type),
    language_style = style,
    presentation_layout = paste0("layout_", layout_id),
    cognitive_skill = "written_reasoning_justification_and_critical_evaluation",
    variables_used = "len, supp, dose",
    statistical_output = output_text,
    question = full_question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_reasoning"
  )
}

# ============================================================
# R800_017
# iris / Sports Analytics / Medium / Interpretation
# ============================================================

r800_017_tasks <- c(
  "interpret_mean_difference",
  "interpret_t_and_p",
  "interpret_confidence_interval",
  "interpret_residual_variation",
  "compare_two_traits",
  "interpret_non_significant_result",
  "interpret_effect_direction",
  "interpret_standard_error",
  "interpret_sample_size",
  "interpret_species_as_groups",
  "translate_output_for_coach",
  "compare_statistical_and_practical",
  "interpret_one_sample_test",
  "interpret_welch_df",
  "interpret_interval_width",
  "interpret_association_not_causation",
  "evaluate_prediction_claim",
  "interpret_multiple_testing",
  "interpret_group_overlap",
  "summarise_complete_result"
)

r800_017_styles <- c(
  "performance_lab_note",
  "team_selection_brief",
  "sports_science_class",
  "scouting_report",
  "coach_analyst_dialogue",
  "competition_review",
  "training_centre_memo",
  "broadcast_graphic",
  "equipment_testing_note",
  "athlete_profile_comparison"
)

r800_017_openings <- list(
  
  performance_lab_note = c(
    "A sports-performance laboratory is using iris flower measurements as a neutral training dataset for learning how to interpret group comparisons.",
    "Analysts in a performance lab practise reading t-test output before applying the same skills to athlete data.",
    "A statistical training exercise in a sports science laboratory uses plant measurements to simulate comparisons between performance groups."
  ),
  
  team_selection_brief = c(
    "A team-selection workshop uses the iris data as an anonymised example of comparing two squads on a continuous performance measure.",
    "The selection panel is practising how to interpret differences between two groups without overclaiming.",
    "A mock selection brief treats two iris species as stand-ins for two training groups."
  ),
  
  sports_science_class = c(
    "Students in a sports analytics module are interpreting t-test output from a real R dataset.",
    "A sports-science class uses iris measurements to practise turning statistical output into plain-language conclusions.",
    "The lecturer presents an iris comparison as a model for interpreting athlete-group differences."
  ),
  
  scouting_report = c(
    "A scouting report exercise asks analysts to compare two groups using a continuous measurement.",
    "The recruitment analytics team is practising how to report uncertainty around group differences.",
    "A simulated scouting task uses iris traits as placeholders for measurable athlete characteristics."
  ),
  
  coach_analyst_dialogue = c(
    "Coach: \"The two group means are different. Is that enough to make a decision?\"\nAnalyst: \"We need to interpret the test statistic, interval and uncertainty together.\"",
    "Coach: \"What does this p-value actually tell me?\"\nPerformance analyst: \"It addresses evidence about the group means, not certainty about every individual.\"",
    "Head coach: \"Can I rank the groups from this result alone?\"\nStatistician: \"Only if we interpret the output carefully.\""
  ),
  
  competition_review = c(
    "A post-competition analytics review includes a training example on group mean comparison.",
    "The review team is checking whether a reported difference is both statistically supported and meaningful.",
    "A competition debrief uses a t-test example to practise evidence-based interpretation."
  ),
  
  training_centre_memo = c(
    "A training-centre memo explains how to read a two-sample t-test before the method is used on athlete data.",
    "The analytics unit prepares an internal note on interpreting group differences and confidence intervals.",
    "A methods memo uses iris measurements to illustrate cautious statistical communication."
  ),
  
  broadcast_graphic = c(
    "A broadcast graphics team is learning how to turn statistical output into a short but accurate comparison.",
    "A television analyst wants a one-sentence interpretation that does not misuse the p-value.",
    "A sports data graphic includes two means, a confidence interval and a test result."
  ),
  
  equipment_testing_note = c(
    "An equipment-testing unit uses the iris data as a practice dataset for comparing two batches.",
    "A testing note focuses on how to interpret variation and uncertainty across two groups.",
    "The sports engineering team is reviewing a sample comparison before analysing equipment measurements."
  ),
  
  athlete_profile_comparison = c(
    "An athlete-profile comparison exercise treats iris species as anonymised group labels.",
    "A development programme uses the dataset to practise comparing average measurements across groups.",
    "The analytics team is rehearsing how to explain a group contrast to coaches."
  )
)

compose_medium_prompt <- function(opening, output_text, task_text, layout_id) {
  if (layout_id == 1) {
    paste0(
      opening,
      "\n\nOutput supplied to the analyst:\n",
      output_text,
      "\n\n",
      task_text
    )
  } else if (layout_id == 2) {
    paste0(
      task_text,
      "\n\nStatistical output:\n",
      output_text,
      "\n\nScenario:\n",
      opening
    )
  } else if (layout_id == 3) {
    paste0(
      opening,
      "\n\n",
      task_text,
      "\n\nBase your interpretation on:\n",
      output_text
    )
  } else {
    paste0(
      "Statistical summary:\n",
      output_text,
      "\n\n",
      opening,
      "\n\n",
      task_text
    )
  }
}

generate_r800_017 <- function(i) {
  
  task_type <- r800_017_tasks[i]
  style <- pick(r800_017_styles)
  opening <- pick(r800_017_openings[[style]])
  layout_id <- sample(1:4, 1)
  
  trait <- pick(c("Sepal.Length", "Petal.Length"))
  species_pair <- sample(levels(IR$Species), 2, replace = FALSE)
  
  x <- IR[IR$Species == species_pair[1], trait]
  y <- IR[IR$Species == species_pair[2], trait]
  
  tst <- t.test(x, y)
  parts <- welch_parts(x, y)
  diff <- mean(x) - mean(y)
  d <- cohens_d(x, y)
  ci <- tst$conf.int
  
  if (task_type == "interpret_mean_difference") {
    
    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      "\nDifference (first - second) = ", fmt(diff)
    )
    
    task_text <- pick(c(
      "Interpret the signed mean difference in context. State which group has the larger average and by how much.",
      "Translate the numerical contrast into a clear comparison suitable for a coach.",
      "Explain the direction and size of the difference without making a claim about every individual observation."
    ))
    
    reference_answer <- paste0(
      "The average ", trait, " for ", species_pair[1],
      " is ", fmt(abs(diff)), " units ",
      ifelse(diff > 0, "higher", "lower"),
      " than the average for ", species_pair[2],
      ". This is a comparison of group means, not a statement that every member of one group exceeds every member of the other."
    )
    
    solution_steps <- paste0(
      "Use the sign of first minus second to identify direction, then express the absolute size in the measurement units."
    )
    
  } else if (task_type == "interpret_t_and_p") {
    
    output_text <- paste0(
      "Welch t = ", fmt(tst$statistic),
      "\nApproximate df = ", fmt(tst$parameter),
      "\nTwo-sided p-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Interpret the test result at the 5% level. Explain what the sign of t indicates and what the p-value says about the equality of group means.",
      "Give a concise but complete interpretation of t and p for the group comparison.",
      "How should a performance analyst describe this output without saying that the result proves the groups are fundamentally different?"
    ))
    
    reference_answer <- paste0(
      "The sign of t reflects the direction of the first-group-minus-second-group difference. ",
      "The p-value is ", p_text(tst$p.value),
      ", so at the 5% level the analysis ",
      ifelse(tst$p.value < 0.05, "rejects", "does not reject"),
      " the null hypothesis of equal population means. The result concerns average ",
      trait, " values in the two groups."
    )
    
    solution_steps <- paste0(
      "Interpret sign, compare p with 0.05, and state the conclusion about population means."
    )
    
  } else if (task_type == "interpret_confidence_interval") {
    
    output_text <- paste0(
      "Estimated difference (", species_pair[1], " - ",
      species_pair[2], ") = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )
    
    task_text <- pick(c(
      "Explain the confidence interval, including direction, plausible effect sizes and whether zero is included.",
      "What does this interval contribute beyond the point estimate?",
      "Interpret the interval in language appropriate for a sports analytics report."
    ))
    
    reference_answer <- paste0(
      "The interval gives plausible values for the population mean difference in ",
      trait, ". ",
      ifelse(
        ci[1] > 0,
        "All plausible values are positive, supporting a higher mean for the first group.",
        ifelse(
          ci[2] < 0,
          "All plausible values are negative, supporting a lower mean for the first group.",
          "The interval includes zero, so no difference remains plausible."
        )
      ),
      " Its width shows the uncertainty around the estimated effect."
    )
    
    solution_steps <- paste0(
      "Check the signs of both limits, identify whether zero lies inside and describe the interval as uncertainty around the population mean difference."
    )
    
  } else if (task_type == "interpret_residual_variation") {
    
    output_text <- paste0(
      species_pair[1], " SD = ", fmt(sd(x)),
      "\n", species_pair[2], " SD = ", fmt(sd(y)),
      "\nMean difference = ", fmt(diff)
    )
    
    task_text <- pick(c(
      "Explain why the difference in means does not imply complete separation between the groups.",
      "How do the within-group SDs affect the interpretation of the mean contrast?",
      "A coach sees different averages and assumes every observation follows the same pattern. Explain why the SDs matter."
    ))
    
    reference_answer <- paste0(
      "The group means differ by ", fmt(diff),
      ", but the SDs show substantial variation within each group. Individual observations may overlap even when the averages differ. ",
      "The t-test compares means relative to this within-group variability."
    )
    
    solution_steps <- paste0(
      "Contrast between-group difference with within-group spread and explain that group-level averages do not determine every individual value."
    )
    
  } else if (task_type == "compare_two_traits") {
    
    sp <- pick(levels(IR$Species))
    
    sepal <- IR[IR$Species == sp, "Sepal.Length"]
    petal <- IR[IR$Species == sp, "Petal.Length"]
    
    output_text <- paste0(
      "Species = ", sp,
      "\nMean Sepal.Length = ", fmt(mean(sepal)),
      "\nMean Petal.Length = ", fmt(mean(petal)),
      "\nDifference (Sepal - Petal) = ",
      fmt(mean(sepal) - mean(petal))
    )
    
    task_text <- pick(c(
      "Interpret the numerical difference as a descriptive comparison. Why would an independent-samples t-test not be appropriate if the two measurements come from the same flowers?",
      "Explain the difference between the two trait means and identify the dependence issue in treating them as unrelated samples.",
      "A trainee proposes an ordinary two-sample t-test for these measurements. Explain why the pairing structure matters."
    ))
    
    reference_answer <- paste0(
      "For ", sp, ", mean sepal length exceeds mean petal length by ",
      fmt(mean(sepal) - mean(petal)),
      " units. Because both measurements are taken from the same flowers, the observations are paired rather than independent. ",
      "A paired analysis would reflect that within-flower relationship."
    )
    
    solution_steps <- paste0(
      "Interpret the descriptive difference, then recognise repeated measurement on the same observational units."
    )
    
  } else if (task_type == "interpret_non_significant_result") {
    
    # Select the pair/trait with the largest p-value among available comparisons.
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
      combos$trait, combos$a, combos$b
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
    
    task_text <- pick(c(
      "Explain why a non-significant result would not prove that the groups are identical.",
      "Interpret failure to reject in terms of evidence and uncertainty.",
      "What should an analyst say instead of 'there is no difference'?"
    ))
    
    reference_answer <- paste0(
      "A non-significant result means the sample does not provide sufficiently strong evidence against equal population means at the chosen level. ",
      "It does not establish exact equality. The confidence interval shows the range of differences still compatible with the data."
    )
    
    solution_steps <- paste0(
      "State failure to reject correctly, use the interval to discuss remaining plausible effects and avoid claiming equality."
    )
    
  } else if (task_type == "interpret_effect_direction") {
    
    output_text <- paste0(
      "Difference defined as ", species_pair[1],
      " minus ", species_pair[2],
      "\nEstimated difference = ", fmt(diff),
      "\nt statistic = ", fmt(tst$statistic)
    )
    
    task_text <- pick(c(
      "Explain why the signs of the mean difference and t statistic agree, and what would change if the group order were reversed.",
      "Interpret the direction of the comparison and describe the effect of swapping the subtraction order.",
      "A report shows a negative t value. Explain what that means and whether it implies a negative trait measurement."
    ))
    
    reference_answer <- paste0(
      "The t statistic has the same sign as the estimated first-minus-second mean difference because the standard error is positive. ",
      "Reversing the group order would reverse the signs of both the difference and t, but not the two-sided p-value or the substantive evidence."
    )
    
    solution_steps <- paste0(
      "Link t to difference divided by a positive SE, then explain the effect of group-order reversal."
    )
    
  } else if (task_type == "interpret_standard_error") {
    
    output_text <- paste0(
      "Estimated mean difference = ", fmt(diff),
      "\nStandard error of the difference = ", fmt(parts$se)
    )
    
    task_text <- pick(c(
      "Interpret the standard error as a measure of uncertainty in the estimated group difference.",
      "What would a smaller standard error mean for the stability of this comparison?",
      "Explain the role of the standard error in forming t and the confidence interval."
    ))
    
    reference_answer <- paste0(
      "The standard error of ", fmt(parts$se),
      " describes the sampling variability expected in the estimated mean difference. ",
      "A smaller SE would produce a larger absolute t value for the same observed difference and a narrower confidence interval."
    )
    
    solution_steps <- paste0(
      "Describe SE as uncertainty in the estimator, then connect it to t = difference/SE and interval width."
    )
    
  } else if (task_type == "interpret_sample_size") {
    
    output_text <- paste0(
      species_pair[1], ": n = ", length(x),
      "\n", species_pair[2], ": n = ", length(y),
      "\nStandard error = ", fmt(parts$se)
    )
    
    task_text <- pick(c(
      "Explain how the sample sizes influence precision and power in this comparison.",
      "What would generally happen to the standard error and confidence interval if both group sizes were smaller?",
      "Interpret the role of n without claiming that a large sample guarantees practical importance."
    ))
    
    reference_answer <- paste0(
      "Larger group sizes generally reduce the standard error, narrow the confidence interval and increase power, assuming variability remains similar. ",
      "However, a large sample can make a very small effect statistically detectable, so sample size does not determine practical importance."
    )
    
    solution_steps <- paste0(
      "Connect n to SE, interval width and power, then distinguish precision from effect magnitude."
    )
    
  } else if (task_type == "interpret_species_as_groups") {
    
    output_text <- paste0(
      "Grouping variable: Species\n",
      "Compared levels: ", species_pair[1],
      " versus ", species_pair[2],
      "\nResponse: ", trait
    )
    
    task_text <- pick(c(
      "Explain why Species is treated as a grouping variable and the trait as a numerical response.",
      "Identify the roles of the variables in the two-sample t-test.",
      "Why would reversing the response and grouping variables not represent the same analysis?"
    ))
    
    reference_answer <- paste0(
      "Species defines the two independent groups, while ", trait,
      " is the continuous outcome whose means are compared. ",
      "A two-sample t-test is designed for a numerical response across categorical groups, so reversing those roles would not answer the same question."
    )
    
    solution_steps <- paste0(
      "Identify categorical grouping variable and continuous response, then relate them to the purpose of the test."
    )
    
  } else if (task_type == "translate_output_for_coach") {
    
    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      "\n95% CI for difference = [",
      fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Turn this output into two or three sentences suitable for a coach with no statistical training.",
      "Explain the result in plain language while retaining direction, uncertainty and strength of evidence.",
      "Write a concise spoken interpretation for a team meeting."
    ))
    
    reference_answer <- paste0(
      "The average ", trait, " differs between the two groups by about ",
      fmt(abs(diff)), " units, with ", species_pair[1],
      ifelse(diff > 0, " higher", " lower"),
      " on average. The 95% interval for the first-minus-second difference is [",
      fmt(ci[1]), ", ", fmt(ci[2]),
      "], and the p-value is ", p_text(tst$p.value),
      ", which indicates ",
      ifelse(tst$p.value < 0.05, "strong evidence of a mean difference.", "limited evidence of a mean difference.")
    )
    
    solution_steps <- paste0(
      "Report group direction and size, explain the interval as uncertainty and translate the p-value without jargon."
    )
    
  } else if (task_type == "compare_statistical_and_practical") {
    
    output_text <- paste0(
      "Mean difference = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Explain how the statistical evidence and practical magnitude could lead to different judgements.",
      "Why should a coach consider Cohen's d as well as the p-value?",
      "Interpret the output without assuming that statistical significance automatically means a useful difference."
    ))
    
    reference_answer <- paste0(
      "The p-value addresses evidence against equal population means, whereas Cohen's d = ",
      fmt(d),
      " describes the difference relative to within-group variability. ",
      "A statistically detectable result may still be too small to matter in practice, and a potentially useful effect may remain uncertain in a smaller sample."
    )
    
    solution_steps <- paste0(
      "Separate evidence from magnitude and explain the distinct roles of p and d."
    )
    
  } else if (task_type == "interpret_one_sample_test") {
    
    sp <- pick(levels(IR$Species))
    tr <- pick(c("Sepal.Length", "Petal.Length"))
    z <- IR[IR$Species == sp, tr]
    benchmark <- round(mean(z) + pick(c(-0.4, -0.3, 0.3, 0.4)), 1)
    one_tst <- t.test(z, mu = benchmark)
    
    output_text <- paste0(
      "Species = ", sp,
      "\nTrait = ", tr,
      "\nBenchmark mean = ", benchmark,
      "\nSample mean = ", fmt(mean(z)),
      "\nt = ", fmt(one_tst$statistic),
      "\np-value ", p_text(one_tst$p.value)
    )
    
    task_text <- pick(c(
      "Interpret the one-sample test in context. What population quantity is being tested?",
      "Explain the conclusion at the 5% level and avoid treating the benchmark as an individual target.",
      "What does the test say about the group mean relative to the benchmark?"
    ))
    
    reference_answer <- paste0(
      "The test compares the population mean ", tr,
      " for ", sp, " with the benchmark ", benchmark,
      ". At the 5% level, the result ",
      ifelse(one_tst$p.value < 0.05, "provides", "does not provide"),
      " sufficient evidence that the population mean differs from the benchmark. ",
      "It is not a test of whether every individual observation differs from ", benchmark, "."
    )
    
    solution_steps <- paste0(
      "Identify the tested population mean, compare p with 0.05 and distinguish group mean from individual observations."
    )
    
  } else if (task_type == "interpret_welch_df") {
    
    output_text <- paste0(
      "Welch t = ", fmt(parts$t),
      "\nApproximate df = ", fmt(parts$df),
      "\nGroup sample sizes = ", parts$nx,
      " and ", parts$ny
    )
    
    task_text <- pick(c(
      "Why are the Welch degrees of freedom not necessarily an integer or equal to n1 + n2 - 2?",
      "Interpret the approximate df and explain what feature of Welch's method produces it.",
      "A trainee thinks the reported df must be a software error. Correct that misunderstanding."
    ))
    
    reference_answer <- paste0(
      "Welch's method estimates the degrees of freedom using the two sample variances and sample sizes. ",
      "The resulting Satterthwaite approximation can be non-integer and is generally smaller than or different from n1 + n2 - 2 because equal variances are not assumed."
    )
    
    solution_steps <- paste0(
      "Explain variance-based adjustment and distinguish Welch df from pooled-test df."
    )
    
  } else if (task_type == "interpret_interval_width") {
    
    output_text <- paste0(
      "95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\nInterval width = ", fmt(diff(ci)),
      "\nStandard error = ", fmt(parts$se)
    )
    
    task_text <- pick(c(
      "Interpret the width of the interval as a statement about precision.",
      "What features of the data contribute to a wider confidence interval?",
      "Explain how the standard error and confidence level determine interval width."
    ))
    
    reference_answer <- paste0(
      "The interval width of ", fmt(diff(ci)),
      " reflects the uncertainty around the estimated mean difference. ",
      "Greater within-group variability, smaller samples or a higher confidence level would widen the interval; a smaller standard error would narrow it."
    )
    
    solution_steps <- paste0(
      "Connect width to critical value times SE and identify sample size and variability as drivers of SE."
    )
    
  } else if (task_type == "interpret_association_not_causation") {
    
    output_text <- paste0(
      "Observed group difference in ", trait,
      " = ", fmt(diff),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Explain why this comparison does not show that belonging to one species causes the trait value.",
      "What can be concluded about association, and what cannot be concluded about causation?",
      "A commentator uses causal language. Rewrite the conclusion appropriately."
    ))
    
    reference_answer <- paste0(
      "The analysis shows an association between species group and average ",
      trait, " in the observed dataset. Species was not assigned as an intervention, so the t-test does not establish a manipulable causal effect. ",
      "The result should be described as a difference in group means."
    )
    
    solution_steps <- paste0(
      "State the observed association and reject causal wording unsupported by the design."
    )
    
  } else if (task_type == "evaluate_prediction_claim") {
    
    output_text <- paste0(
      "Group means differ by ", fmt(diff),
      "\nWithin-group SDs are ", fmt(sd(x)),
      " and ", fmt(sd(y))
    )
    
    task_text <- pick(c(
      "A scout claims the trait alone can perfectly identify group membership. Evaluate that claim.",
      "Why does a difference in means not imply perfect classification?",
      "Explain what additional analysis would be needed before using the trait as a predictor of group."
    ))
    
    reference_answer <- paste0(
      "Different group means do not imply perfect separation because the within-group distributions may overlap. ",
      "A classification analysis, prediction error assessment and validation data would be needed before using ",
      trait, " to identify group membership."
    )
    
    solution_steps <- paste0(
      "Use within-group variability to explain overlap and distinguish mean comparison from classification."
    )
    
  } else if (task_type == "interpret_multiple_testing") {
    
    output_text <- paste0(
      "Several pairwise tests were run across three species and two traits.",
      "\nEach unadjusted test used alpha = 0.05."
    )
    
    task_text <- pick(c(
      "Explain why the overall false-positive risk is larger than 5% and suggest one correction.",
      "Why should the analyst not interpret each pairwise p-value in isolation?",
      "Give a suitable response to the multiple-testing problem."
    ))
    
    reference_answer <- paste0(
      "Running several tests creates multiple opportunities for a false positive, so the family-wise error rate exceeds 5% when each test is judged separately at 0.05. ",
      "Possible responses include Holm or Bonferroni adjustment, or a broader model followed by planned comparisons."
    )
    
    solution_steps <- paste0(
      "Identify the family of tests, explain accumulated Type I error and propose multiplicity control."
    )
    
  } else if (task_type == "interpret_group_overlap") {
    
    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      ", SD = ", fmt(sd(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      ", SD = ", fmt(sd(y))
    )
    
    task_text <- pick(c(
      "Explain why two groups can have clearly different means and still contain overlapping observations.",
      "What do the means and SDs jointly tell you about separation between the groups?",
      "Why should an analyst avoid turning a mean comparison into a claim about every individual?"
    ))
    
    reference_answer <- paste0(
      "The means describe group centres, while the SDs describe spread around those centres. ",
      "Even if the centres differ, observations from the two groups can overlap. Therefore, the result supports an average difference, not perfect individual separation."
    )
    
    solution_steps <- paste0(
      "Interpret centre and spread together and distinguish group-level inference from individual classification."
    )
    
  } else {
    
    output_text <- paste0(
      "Trait = ", trait,
      "\n", species_pair[1], " mean = ", fmt(mean(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      "\nDifference = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\nt = ", fmt(tst$statistic),
      ", df = ", fmt(tst$parameter),
      "\np-value ", p_text(tst$p.value),
      "\nCohen's d = ", fmt(d)
    )
    
    task_text <- pick(c(
      "Produce a complete interpretation suitable for a sports analytics report. Include direction, magnitude, uncertainty, statistical evidence and one limitation.",
      "Summarise the comparison in a way that a coach could use without overstating the result.",
      "Write a concise results paragraph using the full output."
    ))
    
    reference_answer <- paste0(
      "The average ", trait, " for ", species_pair[1],
      " is ", fmt(abs(diff)), " units ",
      ifelse(diff > 0, "higher", "lower"),
      " than for ", species_pair[2],
      ". The 95% confidence interval for the first-minus-second difference is [",
      fmt(ci[1]), ", ", fmt(ci[2]),
      "], with t = ", fmt(tst$statistic),
      ", df = ", fmt(tst$parameter),
      " and p ", p_text(tst$p.value),
      ". Cohen's d = ", fmt(d),
      " describes the standardised magnitude. The conclusion concerns group means and does not imply perfect separation or causation."
    )
    
    solution_steps <- paste0(
      "Report direction and raw difference, interpret the interval, state the test evidence, mention effect size and add one limitation."
    )
  }
  
  full_question <- compose_medium_prompt(
    opening,
    output_text,
    task_text,
    layout_id
  )
  
  make_record(
    id = sprintf("R800_017_%03d", i),
    blueprint_id = "R800_017",
    dataset_name = "iris",
    difficulty = "medium",
    scenario = "sports_analytics",
    template_id = paste0("t_test_template_", task_type),
    language_style = style,
    presentation_layout = paste0("layout_", layout_id),
    cognitive_skill = "contextual_output_interpretation",
    variables_used = "Sepal.Length, Petal.Length, Species",
    statistical_output = output_text,
    question = full_question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "short_interpretation"
  )
}

# ============================================================
# Generate datasets
# ============================================================

R800_016 <- do.call(
  rbind,
  lapply(seq_len(15), generate_r800_016)
)

R800_017 <- do.call(
  rbind,
  lapply(seq_len(20), generate_r800_017)
)

ALL <- rbind(R800_016, R800_017)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_016) == 15)
stopifnot(nrow(R800_017) == 20)
stopifnot(nrow(ALL) == 35)
stopifnot(length(unique(ALL$id)) == 35)

stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))
stopifnot(!any(ALL$question == ""))
stopifnot(!any(ALL$reference_answer == ""))

stopifnot(length(unique(R800_016$template_id)) == 15)
stopifnot(length(unique(R800_017$template_id)) == 20)

stopifnot(
  all(
    R800_016$cognitive_skill ==
      "written_reasoning_justification_and_critical_evaluation"
  )
)

stopifnot(
  all(
    R800_017$cognitive_skill ==
      "contextual_output_interpretation"
  )
)

# ============================================================
# Export
# ============================================================

write.csv(
  R800_016,
  "R800_016_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  R800_017,
  "R800_017_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  ALL,
  "R800_016_017_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

jsonlite::write_json(
  R800_016,
  "R800_016_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  R800_017,
  "R800_017_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  ALL,
  "R800_016_017_questions.json",
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
    "answer_type"
  )]
)

cat(
  "\nGenerated successfully:\n",
  "- R800_016: 15 Hard short-answer questions\n",
  "- R800_017: 20 Medium interpretation questions\n",
  "- Separate and combined CSV/JSON files saved\n"
)

库
/
  R800_016_017_full_generator_fixed.R
# ============================================================
# R800_016 + R800_017
#
# R800_016
# t-test / ToothGrowth / General Everyday / Hard / Short Answer / 15
# Focus: written reasoning, justification, assumptions, limitations
#
# R800_017
# t-test / iris / Sports Analytics / Medium / Interpretation / 20
# Focus: interpretation of statistical output in context
#
# Design principles
# - Real R datasets and computed outputs
# - Full-scenario prompts rather than a repeated role-action template
# - Strong separation between Hard and Medium
# - Rich variation in discourse form, sentence structure and information order
# ============================================================

set.seed(80001617)

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

cohens_d <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  
  pooled_sd <- sqrt(
    ((nx - 1) * var(x) + (ny - 1) * var(y)) /
      (nx + ny - 2)
  )
  
  (mean(x) - mean(y)) / pooled_sd
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
    question_type = ifelse(
      difficulty == "hard",
      "short_answer",
      "interpretation"
    ),
    variables_used = variables_used,
    statistical_output = statistical_output,
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = answer_type,
    version = "v1.1",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_016
# ToothGrowth / General Everyday / Hard / Short Answer
# ============================================================

r800_016_tasks <- c(
  "evaluate_claim_from_p_value",
  "confidence_interval_reasoning",
  "practical_vs_statistical_significance",
  "assumption_and_robustness",
  "multiple_comparisons_warning",
  "causal_claim_critique",
  "subgroup_conclusion",
  "non_significant_result_reasoning",
  "effect_size_and_decision",
  "sample_size_limitation",
  "one_sided_test_justification",
  "welch_vs_pooled_choice",
  "benchmark_claim_evaluation",
  "reporting_quality_review",
  "decision_under_uncertainty"
)

r800_016_styles <- c(
  "consumer_health_article",
  "community_forum_post",
  "personal_decision_note",
  "fact_check",
  "product_comparison_blog",
  "clinic_leaflet_review",
  "neighbourhood_discussion",
  "public_information_brief",
  "editorial_query",
  "household_choice_case",
  "podcast_transcript",
  "online_advice_column"
)

r800_016_openings <- list(
  
  consumer_health_article = c(
    "A consumer-health article summarises the ToothGrowth study for a general audience, but its statistical wording is being checked before publication.",
    "A popular health website has turned a treatment comparison into a short news item and asks for a more careful statistical explanation.",
    "An article aimed at non-specialists reports a difference between supplement groups and invites a judgement about how strong the evidence really is."
  ),
  
  community_forum_post = c(
    "A community forum post quotes a t-test result and then makes a broad claim about which supplement is better.",
    "Someone in an online discussion has copied a p-value from a data analysis without explaining what was actually tested.",
    "A public discussion about everyday supplement choices includes a statistical claim based on the ToothGrowth data."
  ),
  
  personal_decision_note = c(
    "A reader is using the ToothGrowth comparison as a simple example when deciding how much confidence to place in two competing options.",
    "A personal decision note weighs a numerical difference against the uncertainty around that difference.",
    "A non-specialist wants to know whether the observed treatment gap is large enough and reliable enough to influence a practical choice."
  ),
  
  fact_check = c(
    "A fact-checker is reviewing a claim made from the ToothGrowth dataset.",
    "A statistical fact-check is needed because a public-facing statement appears stronger than the evidence shown.",
    "An editor asks whether the reported conclusion follows from the t-test output or overstates it."
  ),
  
  product_comparison_blog = c(
    "A comparison blog presents two treatment groups as if one must always outperform the other.",
    "A product-comparison post uses the ToothGrowth data to rank two options but gives little attention to uncertainty.",
    "A consumer blog cites a difference in average tooth length and asks readers to treat it as decisive."
  ),
  
  clinic_leaflet_review = c(
    "A draft clinic leaflet uses a simple group comparison to explain evidence to patients.",
    "A public-information leaflet includes a numerical treatment contrast, but the reasoning behind the conclusion needs revision.",
    "A clinic communication team wants to ensure that the language used around the t-test is accurate and not misleading."
  ),
  
  neighbourhood_discussion = c(
    "Two neighbours disagree about whether a small observed difference should be treated as meaningful evidence.",
    "A casual discussion turns into a question about what a non-significant t-test does and does not imply.",
    "An everyday conversation about comparing two options raises a deeper question about uncertainty and sample size."
  ),
  
  public_information_brief = c(
    "A short public-information brief reports group means, a confidence interval and a p-value.",
    "A general-audience briefing note needs a balanced explanation of a two-sample comparison.",
    "A public summary includes correct numbers but an incomplete interpretation."
  ),
  
  editorial_query = c(
    "An editor has sent the statistical paragraph back with a request for justification rather than a bare conclusion.",
    "The editorial note asks whether the wording 'proves better' can be supported by the analysis.",
    "A copy editor wants the statistical statement rewritten so that it matches the actual evidence."
  ),
  
  household_choice_case = c(
    "A household-choice example uses two treatment groups to illustrate why an average difference alone may not settle a decision.",
    "A practical decision case asks whether a statistically detectable difference is also large enough to matter.",
    "An everyday choice is being framed around a t-test, but the limitations of the comparison must be made explicit."
  ),
  
  podcast_transcript = c(
    "Host: \"The p-value is below 0.05, so that settles it, right?\"\nGuest statistician: \"Not quite. We still need to discuss magnitude, uncertainty and design.\"",
    "Presenter: \"The means are different. Can we say one option causes better growth?\"\nAnalyst: \"The answer depends on what the study design and test actually support.\"",
    "Host: \"No significant result means no difference at all?\"\nStatistician: \"That is a common misunderstanding.\""
  ),
  
  online_advice_column = c(
    "An advice column receives a question about how to interpret a treatment comparison reported online.",
    "A reader asks whether a p-value alone is enough to make an everyday recommendation.",
    "An online advice response needs to distinguish statistical evidence from a guaranteed practical outcome."
  )
)

compose_hard_prompt <- function(opening, output_text, task_text, layout_id) {
  if (layout_id == 1) {
    paste0(
      opening,
      "\n\nStatistical evidence:\n",
      output_text,
      "\n\n",
      task_text
    )
  } else if (layout_id == 2) {
    paste0(
      task_text,
      "\n\nThe relevant evidence is:\n",
      output_text,
      "\n\nBackground:\n",
      opening
    )
  } else if (layout_id == 3) {
    paste0(
      opening,
      "\n\n",
      task_text,
      "\n\nUse the following statistical output in your answer:\n",
      output_text
    )
  } else {
    paste0(
      "Statistical output:\n",
      output_text,
      "\n\n",
      opening,
      "\n\n",
      task_text
    )
  }
}

generate_r800_016 <- function(i) {
  
  task_type <- r800_016_tasks[i]
  style <- pick(r800_016_styles)
  opening <- pick(r800_016_openings[[style]])
  layout_id <- sample(1:4, 1)
  
  dose <- pick(sort(unique(TG$dose)))
  
  x <- subset(TG, dose == dose & supp == "OJ")$len
  y <- subset(TG, dose == dose & supp == "VC")$len
  
  tst <- t.test(x, y, var.equal = FALSE)
  parts <- welch_parts(x, y)
  diff <- mean(x) - mean(y)
  d <- cohens_d(x, y)
  ci <- tst$conf.int
  
  if (task_type == "evaluate_claim_from_p_value") {
    
    output_text <- paste0(
      "Dose = ", dose,
      "\nMean(OJ) = ", fmt(mean(x)),
      "\nMean(VC) = ", fmt(mean(y)),
      "\nWelch t = ", fmt(tst$statistic),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "A public post concludes, \"OJ is definitely better for everyone.\" Evaluate this statement. Your answer should explain what the p-value supports, what it does not establish, and how the conclusion should be rewritten.",
      "Assess whether the evidence justifies saying that OJ will outperform VC in every case. Give a statistically careful replacement statement.",
      "The numerical result has been interpreted as a universal guarantee. Explain why that interpretation is too strong and state a defensible conclusion."
    ))
    
    reference_answer <- paste0(
      "The test provides evidence about a difference in population mean tooth length at this dose, not a guarantee for every individual observation. ",
      "A suitable conclusion is that the data provide ",
      ifelse(tst$p.value < 0.05, "evidence", "insufficient evidence"),
      " of a difference in mean tooth length between OJ and VC at dose ",
      dose,
      ". The result does not prove universal superiority or causation beyond the study conditions."
    )
    
    solution_steps <- paste0(
      "1. Compare the p-value with the chosen significance level.\n",
      "2. Interpret the test as a statement about population means.\n",
      "3. Avoid individual-level guarantees.\n",
      "4. Avoid claims extending beyond the observed treatment conditions."
    )
    
  } else if (task_type == "confidence_interval_reasoning") {
    
    output_text <- paste0(
      "Estimated mean difference (OJ - VC) = ", fmt(diff),
      "\n95% confidence interval = [",
      fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )
    
    task_text <- pick(c(
      "Explain what this interval says about the size and direction of the supplement difference. Include whether zero is compatible with the data and why the interval is more informative than reporting only the p-value.",
      "Interpret the confidence interval for a general reader. Your response should discuss direction, plausible effect sizes and the corresponding two-sided test decision.",
      "Use the interval to assess both statistical evidence and uncertainty. Do not reduce the answer to 'significant' or 'not significant'."
    ))
    
    reference_answer <- paste0(
      "The interval gives plausible values for the population mean difference OJ - VC. ",
      ifelse(
        ci[1] > 0,
        "Because the entire interval is positive, it supports a higher mean for OJ.",
        ifelse(
          ci[2] < 0,
          "Because the entire interval is negative, it supports a lower mean for OJ.",
          "Because the interval includes zero, the data are compatible with no population mean difference."
        )
      ),
      " It also shows the range of effect sizes consistent with the data, which a p-value alone does not provide."
    )
    
    solution_steps <- paste0(
      "Interpret the sign of both confidence limits, check whether zero lies inside the interval, ",
      "and describe the interval as uncertainty around the population mean difference."
    )
    
  } else if (task_type == "practical_vs_statistical_significance") {
    
    output_text <- paste0(
      "Mean difference (OJ - VC) = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Discuss separately whether the result is statistically convincing and whether the observed difference appears practically important. Explain why these are not the same question.",
      "A blog labels the result 'important' solely because the p-value is small. Evaluate that reasoning using the mean difference and Cohen's d.",
      "Write a short judgement that distinguishes evidence against equal means from the real-world magnitude of the contrast."
    ))
    
    reference_answer <- paste0(
      "Statistical significance is assessed through the p-value, whereas practical importance depends on the size of the mean difference, the standardised effect and the application context. ",
      "Here the observed difference is ", fmt(diff),
      " and Cohen's d is ", fmt(d),
      ". A small p-value does not by itself show that the effect is large enough to matter in practice."
    )
    
    solution_steps <- paste0(
      "Use the p-value for evidence against equal means, then use the raw and standardised differences for magnitude. ",
      "Conclude that statistical and practical significance require separate judgements."
    )
    
  } else if (task_type == "assumption_and_robustness") {
    
    output_text <- paste0(
      "OJ: n = ", length(x),
      ", mean = ", fmt(mean(x)),
      ", SD = ", fmt(sd(x)),
      "\nVC: n = ", length(y),
      ", mean = ", fmt(mean(y)),
      ", SD = ", fmt(sd(y)),
      "\nMethod: Welch two-sample t-test"
    )
    
    task_text <- pick(c(
      "Identify the main assumptions behind this comparison and explain why Welch's test is preferable to the pooled test when equal variances are uncertain. Also comment on what would make the conclusion less reliable.",
      "Before accepting the result, what should be checked about independence, distribution shape and unusual observations? Explain the role of Welch's method in this setting.",
      "Give a reasoned assessment of the test's robustness. Your answer should address independence, approximate normality, outliers and unequal variability."
    ))
    
    reference_answer <- paste0(
      "The observations should be independent within and between groups, and each group should not contain severe outliers or extreme non-normality, especially with modest sample sizes. ",
      "Welch's test does not require equal population variances and is therefore safer when group variability differs. ",
      "Dependence, strong outliers or major distributional irregularities could weaken the validity of the result."
    )
    
    solution_steps <- paste0(
      "Discuss independence first, then shape and outliers, then explain that Welch adjusts the standard error and degrees of freedom rather than imposing equal variances."
    )
    
  } else if (task_type == "multiple_comparisons_warning") {
    
    all_doses <- sort(unique(TG$dose))
    pvals <- sapply(
      all_doses,
      function(z) {
        a <- subset(TG, dose == z & supp == "OJ")$len
        b <- subset(TG, dose == z & supp == "VC")$len
        t.test(a, b)$p.value
      }
    )
    
    output_text <- paste0(
      "Separate OJ-versus-VC tests were run at three doses.\n",
      paste0(
        "Dose ", all_doses,
        ": p ", vapply(pvals, p_text, character(1)),
        collapse = "\n"
      )
    )
    
    task_text <- pick(c(
      "Explain why interpreting all three tests at the 5% level without adjustment increases the chance of at least one false positive. Suggest one reasonable response.",
      "A report treats each dose-specific p-value as if it were the only test performed. Critique this approach and propose an adjustment or a better modelling strategy.",
      "Why does running several separate t-tests change the error-control problem? Give a justified recommendation."
    ))
    
    reference_answer <- paste0(
      "Testing several dose-specific hypotheses inflates the family-wise probability of at least one Type I error if each test uses 0.05 independently. ",
      "Possible responses include Bonferroni or Holm adjustment, or fitting a model that analyses supplement, dose and their interaction jointly."
    )
    
    solution_steps <- paste0(
      "Recognise the family of three tests, explain accumulated false-positive risk, and recommend multiplicity control or a unified model."
    )
    
  } else if (task_type == "causal_claim_critique") {
    
    output_text <- paste0(
      "At dose ", dose,
      ", mean(OJ) - mean(VC) = ", fmt(diff),
      "\nWelch p-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "An online article says, \"Changing from VC to OJ causes tooth length to increase by exactly this amount.\" Critique both the causal wording and the use of an exact individual effect.",
      "Does this t-test output by itself justify a causal statement? Explain what additional information about the study design would be needed.",
      "Rewrite the claim so that it reflects an average group comparison rather than a guaranteed causal effect for each subject."
    ))
    
    reference_answer <- paste0(
      "The output establishes an estimated difference between group means under the observed study conditions. ",
      "A causal conclusion depends on how treatments were assigned and whether confounding was controlled. ",
      "The mean difference is not an exact effect for every subject. A careful statement is that the OJ and VC groups showed an estimated average difference of ",
      fmt(diff), " at dose ", dose, "."
    )
    
    solution_steps <- paste0(
      "Separate association from causation, note the need for random assignment and control, and distinguish an average treatment contrast from an individual response."
    )
    
  } else if (task_type == "subgroup_conclusion") {
    
    all_x <- subset(TG, supp == "OJ")$len
    all_y <- subset(TG, supp == "VC")$len
    overall_tst <- t.test(all_x, all_y)
    
    output_text <- paste0(
      "Dose-specific comparison at dose ", dose,
      ": p ", p_text(tst$p.value),
      "\nOverall comparison across all doses: p ",
      p_text(overall_tst$p.value)
    )
    
    task_text <- pick(c(
      "Explain why the dose-specific and overall conclusions may differ. Discuss the role of dose composition and why an overall comparison can obscure subgroup patterns.",
      "A reader is confused because the pooled comparison and the selected dose comparison do not tell the same story. Give a reasoned explanation.",
      "Why is it risky to ignore dose when comparing the two supplements? Relate your answer to aggregation and subgroup structure."
    ))
    
    reference_answer <- paste0(
      "The overall comparison mixes observations from different dose levels, and dose has a strong relationship with tooth length. ",
      "If the supplement groups are distributed differently across doses, or if the supplement effect changes with dose, the pooled comparison can hide or distort dose-specific patterns. ",
      "The conclusion should therefore condition on dose or use a model including dose and a supplement-by-dose interaction."
    )
    
    solution_steps <- paste0(
      "Identify dose as an important stratifying variable, explain why aggregation changes the comparison, and recommend adjusted or interaction-based analysis."
    )
    
  } else if (task_type == "non_significant_result_reasoning") {
    
    # Choose the dose whose p-value is largest to make the prompt coherent.
    all_doses <- sort(unique(TG$dose))
    dose_p <- sapply(
      all_doses,
      function(z) {
        a <- subset(TG, dose == z & supp == "OJ")$len
        b <- subset(TG, dose == z & supp == "VC")$len
        t.test(a, b)$p.value
      }
    )
    
    chosen_dose <- all_doses[which.max(dose_p)]
    x2 <- subset(TG, dose == chosen_dose & supp == "OJ")$len
    y2 <- subset(TG, dose == chosen_dose & supp == "VC")$len
    tst2 <- t.test(x2, y2)
    
    output_text <- paste0(
      "Dose = ", chosen_dose,
      "\nMean difference (OJ - VC) = ",
      fmt(mean(x2) - mean(y2)),
      "\n95% CI = [",
      fmt(tst2$conf.int[1]), ", ",
      fmt(tst2$conf.int[2]), "]",
      "\np-value ", p_text(tst2$p.value)
    )
    
    task_text <- pick(c(
      "A discussion post says, \"There is no difference at all.\" Explain why a non-significant result does not prove equality and what the confidence interval contributes.",
      "Interpret this result without using the phrase 'the treatments are the same'. Address uncertainty, sample size and the range of effects still compatible with the data.",
      "Why is 'failure to reject' not equivalent to evidence of no effect? Give a careful short answer based on the output."
    ))
    
    reference_answer <- paste0(
      "A non-significant result means the data do not provide sufficiently strong evidence against equal population means at the chosen level; it does not prove exact equality. ",
      "The confidence interval shows the range of differences still compatible with the data and may include effects that are not negligible. ",
      "Limited sample size and variability may also reduce power."
    )
    
    solution_steps <- paste0(
      "State the correct meaning of failure to reject, interpret the interval, and mention that low precision or power can leave meaningful effects unresolved."
    )
    
  } else if (task_type == "effect_size_and_decision") {
    
    output_text <- paste0(
      "Dose = ", dose,
      "\nMean difference (OJ - VC) = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Give a balanced recommendation for a general reader. Your answer should integrate the direction of the effect, its standardised magnitude, uncertainty and statistical evidence.",
      "Write a short evidence summary that uses all four quantities rather than relying on a single threshold.",
      "How should someone weigh the observed effect when making a practical choice? Justify your answer from the complete output."
    ))
    
    reference_answer <- paste0(
      "The OJ - VC difference is ", fmt(diff),
      " with Cohen's d = ", fmt(d),
      ". The confidence interval [", fmt(ci[1]), ", ",
      fmt(ci[2]), "] shows the uncertainty around the population mean difference, and the p-value is ",
      p_text(tst$p.value),
      ". A practical recommendation should consider both the likely magnitude and uncertainty, not statistical significance alone."
    )
    
    solution_steps <- paste0(
      "Combine direction, raw effect size, standardised effect size, interval width and p-value. Avoid absolute claims and state that practical relevance depends on context."
    )
    
  } else if (task_type == "sample_size_limitation") {
    
    output_text <- paste0(
      "Each supplement group at dose ", dose,
      " contains ", length(x), " observations.",
      "\nObserved SDs: OJ = ", fmt(sd(x)),
      ", VC = ", fmt(sd(y)),
      "\n95% CI width = ", fmt(diff(ci))
    )
    
    task_text <- pick(c(
      "Explain how the modest group sizes affect precision and the strength of any everyday recommendation. What would a larger sample change?",
      "A reader treats the point estimate as very stable. Critique this view using sample size, variability and interval width.",
      "Why should the conclusion remain cautious even when the observed mean difference looks noticeable?"
    ))
    
    reference_answer <- paste0(
      "With only ", length(x),
      " observations per group, the estimate is sensitive to sampling variability, especially when within-group SDs are substantial. ",
      "The confidence interval width reflects this uncertainty. A larger sample would generally reduce the standard error, narrow the interval and provide greater power to detect a true difference."
    )
    
    solution_steps <- paste0(
      "Link sample size to standard error, interval width and power, then explain why a point estimate alone overstates certainty."
    )
    
  } else if (task_type == "one_sided_test_justification") {
    
    one_sided_p <- if (unname(tst$statistic) > 0) {
      tst$p.value / 2
    } else {
      1 - tst$p.value / 2
    }
    
    output_text <- paste0(
      "Research claim: mean(OJ) > mean(VC) at dose ", dose,
      "\nObserved t = ", fmt(tst$statistic),
      "\nTwo-sided p ", p_text(tst$p.value),
      "\nCorresponding one-sided p ",
      p_text(one_sided_p)
    )
    
    task_text <- pick(c(
      "Explain when a one-sided test would be justified and why choosing it only after seeing the direction of the data is inappropriate.",
      "A writer prefers the smaller one-sided p-value. Evaluate whether that choice is defensible.",
      "What conditions must be satisfied before replacing the two-sided test with a one-sided alternative?"
    ))
    
    reference_answer <- paste0(
      "A one-sided test is justified only when the directional alternative was specified before examining the data and an effect in the opposite direction would not lead to the same substantive claim. ",
      "Choosing a one-sided test after observing the sign of the result inflates the false-positive risk and is not valid."
    )
    
    solution_steps <- paste0(
      "Discuss pre-specification, the scientific relevance of the opposite direction, and the problem of post-hoc selection."
    )
    
  } else if (task_type == "welch_vs_pooled_choice") {
    
    pooled <- t.test(x, y, var.equal = TRUE)
    
    output_text <- paste0(
      "OJ SD = ", fmt(sd(x)),
      "\nVC SD = ", fmt(sd(y)),
      "\nWelch: t = ", fmt(tst$statistic),
      ", df = ", fmt(tst$parameter),
      ", p ", p_text(tst$p.value),
      "\nPooled: t = ", fmt(pooled$statistic),
      ", df = ", fmt(pooled$parameter),
      ", p ", p_text(pooled$p.value)
    )
    
    task_text <- pick(c(
      "Which test should be preferred here, and why? Your answer should discuss the equal-variance assumption rather than simply choosing the smaller p-value.",
      "Compare the logic of Welch and pooled t-tests and justify a default choice for this analysis.",
      "A report uses the pooled result without comment. Explain whether that is adequately justified."
    ))
    
    reference_answer <- paste0(
      "Welch's test is generally preferable unless equal population variances are substantively and empirically justified. ",
      "It remains valid under unequal variances and usually performs well when variances are equal. ",
      "The choice should be based on assumptions and design, not on which method gives the more favourable p-value."
    )
    
    solution_steps <- paste0(
      "Compare the variance assumption, explain Welch's robustness, and reject method selection based on outcome."
    )
    
  } else if (task_type == "benchmark_claim_evaluation") {
    
    benchmark <- round(mean(x) + pick(c(-3, -2, 2, 3)), 1)
    one_tst <- t.test(x, mu = benchmark)
    
    output_text <- paste0(
      "Group: OJ at dose ", dose,
      "\nSample mean = ", fmt(mean(x)),
      "\nBenchmark = ", benchmark,
      "\nOne-sample t = ", fmt(one_tst$statistic),
      "\n95% CI for mean = [",
      fmt(one_tst$conf.int[1]), ", ",
      fmt(one_tst$conf.int[2]), "]",
      "\np-value ", p_text(one_tst$p.value)
    )
    
    task_text <- pick(c(
      "Evaluate the claim that the group mean is meaningfully different from the benchmark. Distinguish the test result from the size of the departure.",
      "A public summary says the benchmark is 'wrong'. Explain what the one-sample test can legitimately conclude.",
      "Interpret the one-sample result and identify one limitation of using a single benchmark value in an everyday recommendation."
    ))
    
    reference_answer <- paste0(
      "The one-sample test evaluates whether the population mean is compatible with the benchmark under the model assumptions. ",
      "The observed departure is ", fmt(mean(x) - benchmark),
      ", while the confidence interval shows plausible values for the population mean. ",
      "Rejecting the benchmark statistically does not automatically show that the difference is practically important or that the benchmark is universally inappropriate."
    )
    
    solution_steps <- paste0(
      "Interpret the hypothesis, quantify the departure, use the interval for uncertainty and separate statistical from practical conclusions."
    )
    
  } else if (task_type == "reporting_quality_review") {
    
    output_text <- paste0(
      "Draft sentence:\n",
      "\"OJ produced significantly greater growth than VC (p ",
      p_text(tst$p.value),
      ").\"\n\n",
      "Supporting values:\n",
      "Mean difference = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )
    
    task_text <- pick(c(
      "Rewrite the draft sentence so that it reports the comparison more completely and avoids implying certainty beyond the data.",
      "Identify what is missing from the sentence and provide an improved results statement.",
      "Edit the statement for statistical accuracy, including magnitude, uncertainty and the relevant treatment condition."
    ))
    
    reference_answer <- paste0(
      "At dose ", dose,
      ", the estimated mean tooth length was ", fmt(diff),
      " units higher for OJ than for VC, with a 95% confidence interval from ",
      fmt(ci[1]), " to ", fmt(ci[2]),
      " and a Welch two-sided p-value ", p_text(tst$p.value),
      ". This wording reports magnitude and uncertainty without claiming universal superiority."
    )
    
    solution_steps <- paste0(
      "Include the dose, direction and size of the contrast, the confidence interval, test method and p-value, while avoiding causal or individual-level wording."
    )
    
  } else {
    
    output_text <- paste0(
      "Dose = ", dose,
      "\nMean difference (OJ - VC) = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\np-value ", p_text(tst$p.value),
      "\nCohen's d = ", fmt(d)
    )
    
    task_text <- pick(c(
      "Imagine that a decision must be made now, despite uncertainty. Give a justified recommendation that states what the evidence favours, how uncertain it remains and what additional information would improve the decision.",
      "Write a short decision note for a non-specialist. It should neither ignore the evidence nor pretend that the result is certain.",
      "Based on this output, what would be a responsible next step? Support your answer with the estimated effect, uncertainty and limitations."
    ))
    
    reference_answer <- paste0(
      "The evidence may favour one supplement on average at dose ", dose,
      ", but the recommendation should reflect the estimated difference of ",
      fmt(diff), ", the interval [", fmt(ci[1]), ", ",
      fmt(ci[2]), "] and Cohen's d = ", fmt(d),
      ". A responsible decision would also consider replication, sample size, possible adverse outcomes and whether the observed effect is practically important."
    )
    
    solution_steps <- paste0(
      "Summarise the direction and magnitude, acknowledge uncertainty, avoid certainty claims and identify what further evidence would reduce decision risk."
    )
  }
  
  full_question <- compose_hard_prompt(
    opening,
    output_text,
    task_text,
    layout_id
  )
  
  make_record(
    id = sprintf("R800_016_%03d", i),
    blueprint_id = "R800_016",
    dataset_name = "ToothGrowth",
    difficulty = "hard",
    scenario = "general_everyday",
    template_id = paste0("t_test_template_", task_type),
    language_style = style,
    presentation_layout = paste0("layout_", layout_id),
    cognitive_skill = "written_reasoning_justification_and_critical_evaluation",
    variables_used = "len, supp, dose",
    statistical_output = output_text,
    question = full_question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "open_ended_reasoning"
  )
}

# ============================================================
# R800_017
# iris / Sports Analytics / Medium / Interpretation
# ============================================================

r800_017_tasks <- c(
  "interpret_mean_difference",
  "interpret_t_and_p",
  "interpret_confidence_interval",
  "interpret_residual_variation",
  "compare_two_traits",
  "interpret_non_significant_result",
  "interpret_effect_direction",
  "interpret_standard_error",
  "interpret_sample_size",
  "interpret_species_as_groups",
  "translate_output_for_coach",
  "compare_statistical_and_practical",
  "interpret_one_sample_test",
  "interpret_welch_df",
  "interpret_interval_width",
  "interpret_association_not_causation",
  "evaluate_prediction_claim",
  "interpret_multiple_testing",
  "interpret_group_overlap",
  "summarise_complete_result"
)

r800_017_styles <- c(
  "performance_lab_note",
  "team_selection_brief",
  "sports_science_class",
  "scouting_report",
  "coach_analyst_dialogue",
  "competition_review",
  "training_centre_memo",
  "broadcast_graphic",
  "equipment_testing_note",
  "athlete_profile_comparison"
)

r800_017_openings <- list(
  
  performance_lab_note = c(
    "A sports-performance laboratory is using iris flower measurements as a neutral training dataset for learning how to interpret group comparisons.",
    "Analysts in a performance lab practise reading t-test output before applying the same skills to athlete data.",
    "A statistical training exercise in a sports science laboratory uses plant measurements to simulate comparisons between performance groups."
  ),
  
  team_selection_brief = c(
    "A team-selection workshop uses the iris data as an anonymised example of comparing two squads on a continuous performance measure.",
    "The selection panel is practising how to interpret differences between two groups without overclaiming.",
    "A mock selection brief treats two iris species as stand-ins for two training groups."
  ),
  
  sports_science_class = c(
    "Students in a sports analytics module are interpreting t-test output from a real R dataset.",
    "A sports-science class uses iris measurements to practise turning statistical output into plain-language conclusions.",
    "The lecturer presents an iris comparison as a model for interpreting athlete-group differences."
  ),
  
  scouting_report = c(
    "A scouting report exercise asks analysts to compare two groups using a continuous measurement.",
    "The recruitment analytics team is practising how to report uncertainty around group differences.",
    "A simulated scouting task uses iris traits as placeholders for measurable athlete characteristics."
  ),
  
  coach_analyst_dialogue = c(
    "Coach: \"The two group means are different. Is that enough to make a decision?\"\nAnalyst: \"We need to interpret the test statistic, interval and uncertainty together.\"",
    "Coach: \"What does this p-value actually tell me?\"\nPerformance analyst: \"It addresses evidence about the group means, not certainty about every individual.\"",
    "Head coach: \"Can I rank the groups from this result alone?\"\nStatistician: \"Only if we interpret the output carefully.\""
  ),
  
  competition_review = c(
    "A post-competition analytics review includes a training example on group mean comparison.",
    "The review team is checking whether a reported difference is both statistically supported and meaningful.",
    "A competition debrief uses a t-test example to practise evidence-based interpretation."
  ),
  
  training_centre_memo = c(
    "A training-centre memo explains how to read a two-sample t-test before the method is used on athlete data.",
    "The analytics unit prepares an internal note on interpreting group differences and confidence intervals.",
    "A methods memo uses iris measurements to illustrate cautious statistical communication."
  ),
  
  broadcast_graphic = c(
    "A broadcast graphics team is learning how to turn statistical output into a short but accurate comparison.",
    "A television analyst wants a one-sentence interpretation that does not misuse the p-value.",
    "A sports data graphic includes two means, a confidence interval and a test result."
  ),
  
  equipment_testing_note = c(
    "An equipment-testing unit uses the iris data as a practice dataset for comparing two batches.",
    "A testing note focuses on how to interpret variation and uncertainty across two groups.",
    "The sports engineering team is reviewing a sample comparison before analysing equipment measurements."
  ),
  
  athlete_profile_comparison = c(
    "An athlete-profile comparison exercise treats iris species as anonymised group labels.",
    "A development programme uses the dataset to practise comparing average measurements across groups.",
    "The analytics team is rehearsing how to explain a group contrast to coaches."
  )
)

compose_medium_prompt <- function(opening, output_text, task_text, layout_id) {
  if (layout_id == 1) {
    paste0(
      opening,
      "\n\nOutput supplied to the analyst:\n",
      output_text,
      "\n\n",
      task_text
    )
  } else if (layout_id == 2) {
    paste0(
      task_text,
      "\n\nStatistical output:\n",
      output_text,
      "\n\nScenario:\n",
      opening
    )
  } else if (layout_id == 3) {
    paste0(
      opening,
      "\n\n",
      task_text,
      "\n\nBase your interpretation on:\n",
      output_text
    )
  } else {
    paste0(
      "Statistical summary:\n",
      output_text,
      "\n\n",
      opening,
      "\n\n",
      task_text
    )
  }
}

generate_r800_017 <- function(i) {
  
  task_type <- r800_017_tasks[i]
  style <- pick(r800_017_styles)
  opening <- pick(r800_017_openings[[style]])
  layout_id <- sample(1:4, 1)
  
  trait <- pick(c("Sepal.Length", "Petal.Length"))
  species_pair <- sample(levels(IR$Species), 2, replace = FALSE)
  
  x <- IR[IR$Species == species_pair[1], trait]
  y <- IR[IR$Species == species_pair[2], trait]
  
  tst <- t.test(x, y)
  parts <- welch_parts(x, y)
  diff <- mean(x) - mean(y)
  d <- cohens_d(x, y)
  ci <- tst$conf.int
  
  if (task_type == "interpret_mean_difference") {
    
    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      "\nDifference (first - second) = ", fmt(diff)
    )
    
    task_text <- pick(c(
      "Interpret the signed mean difference in context. State which group has the larger average and by how much.",
      "Translate the numerical contrast into a clear comparison suitable for a coach.",
      "Explain the direction and size of the difference without making a claim about every individual observation."
    ))
    
    reference_answer <- paste0(
      "The average ", trait, " for ", species_pair[1],
      " is ", fmt(abs(diff)), " units ",
      ifelse(diff > 0, "higher", "lower"),
      " than the average for ", species_pair[2],
      ". This is a comparison of group means, not a statement that every member of one group exceeds every member of the other."
    )
    
    solution_steps <- paste0(
      "Use the sign of first minus second to identify direction, then express the absolute size in the measurement units."
    )
    
  } else if (task_type == "interpret_t_and_p") {
    
    output_text <- paste0(
      "Welch t = ", fmt(tst$statistic),
      "\nApproximate df = ", fmt(tst$parameter),
      "\nTwo-sided p-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Interpret the test result at the 5% level. Explain what the sign of t indicates and what the p-value says about the equality of group means.",
      "Give a concise but complete interpretation of t and p for the group comparison.",
      "How should a performance analyst describe this output without saying that the result proves the groups are fundamentally different?"
    ))
    
    reference_answer <- paste0(
      "The sign of t reflects the direction of the first-group-minus-second-group difference. ",
      "The p-value is ", p_text(tst$p.value),
      ", so at the 5% level the analysis ",
      ifelse(tst$p.value < 0.05, "rejects", "does not reject"),
      " the null hypothesis of equal population means. The result concerns average ",
      trait, " values in the two groups."
    )
    
    solution_steps <- paste0(
      "Interpret sign, compare p with 0.05, and state the conclusion about population means."
    )
    
  } else if (task_type == "interpret_confidence_interval") {
    
    output_text <- paste0(
      "Estimated difference (", species_pair[1], " - ",
      species_pair[2], ") = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]"
    )
    
    task_text <- pick(c(
      "Explain the confidence interval, including direction, plausible effect sizes and whether zero is included.",
      "What does this interval contribute beyond the point estimate?",
      "Interpret the interval in language appropriate for a sports analytics report."
    ))
    
    reference_answer <- paste0(
      "The interval gives plausible values for the population mean difference in ",
      trait, ". ",
      ifelse(
        ci[1] > 0,
        "All plausible values are positive, supporting a higher mean for the first group.",
        ifelse(
          ci[2] < 0,
          "All plausible values are negative, supporting a lower mean for the first group.",
          "The interval includes zero, so no difference remains plausible."
        )
      ),
      " Its width shows the uncertainty around the estimated effect."
    )
    
    solution_steps <- paste0(
      "Check the signs of both limits, identify whether zero lies inside and describe the interval as uncertainty around the population mean difference."
    )
    
  } else if (task_type == "interpret_residual_variation") {
    
    output_text <- paste0(
      species_pair[1], " SD = ", fmt(sd(x)),
      "\n", species_pair[2], " SD = ", fmt(sd(y)),
      "\nMean difference = ", fmt(diff)
    )
    
    task_text <- pick(c(
      "Explain why the difference in means does not imply complete separation between the groups.",
      "How do the within-group SDs affect the interpretation of the mean contrast?",
      "A coach sees different averages and assumes every observation follows the same pattern. Explain why the SDs matter."
    ))
    
    reference_answer <- paste0(
      "The group means differ by ", fmt(diff),
      ", but the SDs show substantial variation within each group. Individual observations may overlap even when the averages differ. ",
      "The t-test compares means relative to this within-group variability."
    )
    
    solution_steps <- paste0(
      "Contrast between-group difference with within-group spread and explain that group-level averages do not determine every individual value."
    )
    
  } else if (task_type == "compare_two_traits") {
    
    sp <- pick(levels(IR$Species))
    
    sepal <- IR[IR$Species == sp, "Sepal.Length"]
    petal <- IR[IR$Species == sp, "Petal.Length"]
    
    output_text <- paste0(
      "Species = ", sp,
      "\nMean Sepal.Length = ", fmt(mean(sepal)),
      "\nMean Petal.Length = ", fmt(mean(petal)),
      "\nDifference (Sepal - Petal) = ",
      fmt(mean(sepal) - mean(petal))
    )
    
    task_text <- pick(c(
      "Interpret the numerical difference as a descriptive comparison. Why would an independent-samples t-test not be appropriate if the two measurements come from the same flowers?",
      "Explain the difference between the two trait means and identify the dependence issue in treating them as unrelated samples.",
      "A trainee proposes an ordinary two-sample t-test for these measurements. Explain why the pairing structure matters."
    ))
    
    reference_answer <- paste0(
      "For ", sp, ", mean sepal length exceeds mean petal length by ",
      fmt(mean(sepal) - mean(petal)),
      " units. Because both measurements are taken from the same flowers, the observations are paired rather than independent. ",
      "A paired analysis would reflect that within-flower relationship."
    )
    
    solution_steps <- paste0(
      "Interpret the descriptive difference, then recognise repeated measurement on the same observational units."
    )
    
  } else if (task_type == "interpret_non_significant_result") {
    
    # Select the pair/trait with the largest p-value among available comparisons.
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
      combos$trait, combos$a, combos$b
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
    
    task_text <- pick(c(
      "Explain why a non-significant result would not prove that the groups are identical.",
      "Interpret failure to reject in terms of evidence and uncertainty.",
      "What should an analyst say instead of 'there is no difference'?"
    ))
    
    reference_answer <- paste0(
      "A non-significant result means the sample does not provide sufficiently strong evidence against equal population means at the chosen level. ",
      "It does not establish exact equality. The confidence interval shows the range of differences still compatible with the data."
    )
    
    solution_steps <- paste0(
      "State failure to reject correctly, use the interval to discuss remaining plausible effects and avoid claiming equality."
    )
    
  } else if (task_type == "interpret_effect_direction") {
    
    output_text <- paste0(
      "Difference defined as ", species_pair[1],
      " minus ", species_pair[2],
      "\nEstimated difference = ", fmt(diff),
      "\nt statistic = ", fmt(tst$statistic)
    )
    
    task_text <- pick(c(
      "Explain why the signs of the mean difference and t statistic agree, and what would change if the group order were reversed.",
      "Interpret the direction of the comparison and describe the effect of swapping the subtraction order.",
      "A report shows a negative t value. Explain what that means and whether it implies a negative trait measurement."
    ))
    
    reference_answer <- paste0(
      "The t statistic has the same sign as the estimated first-minus-second mean difference because the standard error is positive. ",
      "Reversing the group order would reverse the signs of both the difference and t, but not the two-sided p-value or the substantive evidence."
    )
    
    solution_steps <- paste0(
      "Link t to difference divided by a positive SE, then explain the effect of group-order reversal."
    )
    
  } else if (task_type == "interpret_standard_error") {
    
    output_text <- paste0(
      "Estimated mean difference = ", fmt(diff),
      "\nStandard error of the difference = ", fmt(parts$se)
    )
    
    task_text <- pick(c(
      "Interpret the standard error as a measure of uncertainty in the estimated group difference.",
      "What would a smaller standard error mean for the stability of this comparison?",
      "Explain the role of the standard error in forming t and the confidence interval."
    ))
    
    reference_answer <- paste0(
      "The standard error of ", fmt(parts$se),
      " describes the sampling variability expected in the estimated mean difference. ",
      "A smaller SE would produce a larger absolute t value for the same observed difference and a narrower confidence interval."
    )
    
    solution_steps <- paste0(
      "Describe SE as uncertainty in the estimator, then connect it to t = difference/SE and interval width."
    )
    
  } else if (task_type == "interpret_sample_size") {
    
    output_text <- paste0(
      species_pair[1], ": n = ", length(x),
      "\n", species_pair[2], ": n = ", length(y),
      "\nStandard error = ", fmt(parts$se)
    )
    
    task_text <- pick(c(
      "Explain how the sample sizes influence precision and power in this comparison.",
      "What would generally happen to the standard error and confidence interval if both group sizes were smaller?",
      "Interpret the role of n without claiming that a large sample guarantees practical importance."
    ))
    
    reference_answer <- paste0(
      "Larger group sizes generally reduce the standard error, narrow the confidence interval and increase power, assuming variability remains similar. ",
      "However, a large sample can make a very small effect statistically detectable, so sample size does not determine practical importance."
    )
    
    solution_steps <- paste0(
      "Connect n to SE, interval width and power, then distinguish precision from effect magnitude."
    )
    
  } else if (task_type == "interpret_species_as_groups") {
    
    output_text <- paste0(
      "Grouping variable: Species\n",
      "Compared levels: ", species_pair[1],
      " versus ", species_pair[2],
      "\nResponse: ", trait
    )
    
    task_text <- pick(c(
      "Explain why Species is treated as a grouping variable and the trait as a numerical response.",
      "Identify the roles of the variables in the two-sample t-test.",
      "Why would reversing the response and grouping variables not represent the same analysis?"
    ))
    
    reference_answer <- paste0(
      "Species defines the two independent groups, while ", trait,
      " is the continuous outcome whose means are compared. ",
      "A two-sample t-test is designed for a numerical response across categorical groups, so reversing those roles would not answer the same question."
    )
    
    solution_steps <- paste0(
      "Identify categorical grouping variable and continuous response, then relate them to the purpose of the test."
    )
    
  } else if (task_type == "translate_output_for_coach") {
    
    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      "\n95% CI for difference = [",
      fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Turn this output into two or three sentences suitable for a coach with no statistical training.",
      "Explain the result in plain language while retaining direction, uncertainty and strength of evidence.",
      "Write a concise spoken interpretation for a team meeting."
    ))
    
    reference_answer <- paste0(
      "The average ", trait, " differs between the two groups by about ",
      fmt(abs(diff)), " units, with ", species_pair[1],
      ifelse(diff > 0, " higher", " lower"),
      " on average. The 95% interval for the first-minus-second difference is [",
      fmt(ci[1]), ", ", fmt(ci[2]),
      "], and the p-value is ", p_text(tst$p.value),
      ", which indicates ",
      ifelse(tst$p.value < 0.05, "strong evidence of a mean difference.", "limited evidence of a mean difference.")
    )
    
    solution_steps <- paste0(
      "Report group direction and size, explain the interval as uncertainty and translate the p-value without jargon."
    )
    
  } else if (task_type == "compare_statistical_and_practical") {
    
    output_text <- paste0(
      "Mean difference = ", fmt(diff),
      "\nCohen's d = ", fmt(d),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Explain how the statistical evidence and practical magnitude could lead to different judgements.",
      "Why should a coach consider Cohen's d as well as the p-value?",
      "Interpret the output without assuming that statistical significance automatically means a useful difference."
    ))
    
    reference_answer <- paste0(
      "The p-value addresses evidence against equal population means, whereas Cohen's d = ",
      fmt(d),
      " describes the difference relative to within-group variability. ",
      "A statistically detectable result may still be too small to matter in practice, and a potentially useful effect may remain uncertain in a smaller sample."
    )
    
    solution_steps <- paste0(
      "Separate evidence from magnitude and explain the distinct roles of p and d."
    )
    
  } else if (task_type == "interpret_one_sample_test") {
    
    sp <- pick(levels(IR$Species))
    tr <- pick(c("Sepal.Length", "Petal.Length"))
    z <- IR[IR$Species == sp, tr]
    benchmark <- round(mean(z) + pick(c(-0.4, -0.3, 0.3, 0.4)), 1)
    one_tst <- t.test(z, mu = benchmark)
    
    output_text <- paste0(
      "Species = ", sp,
      "\nTrait = ", tr,
      "\nBenchmark mean = ", benchmark,
      "\nSample mean = ", fmt(mean(z)),
      "\nt = ", fmt(one_tst$statistic),
      "\np-value ", p_text(one_tst$p.value)
    )
    
    task_text <- pick(c(
      "Interpret the one-sample test in context. What population quantity is being tested?",
      "Explain the conclusion at the 5% level and avoid treating the benchmark as an individual target.",
      "What does the test say about the group mean relative to the benchmark?"
    ))
    
    reference_answer <- paste0(
      "The test compares the population mean ", tr,
      " for ", sp, " with the benchmark ", benchmark,
      ". At the 5% level, the result ",
      ifelse(one_tst$p.value < 0.05, "provides", "does not provide"),
      " sufficient evidence that the population mean differs from the benchmark. ",
      "It is not a test of whether every individual observation differs from ", benchmark, "."
    )
    
    solution_steps <- paste0(
      "Identify the tested population mean, compare p with 0.05 and distinguish group mean from individual observations."
    )
    
  } else if (task_type == "interpret_welch_df") {
    
    output_text <- paste0(
      "Welch t = ", fmt(parts$t),
      "\nApproximate df = ", fmt(parts$df),
      "\nGroup sample sizes = ", parts$nx,
      " and ", parts$ny
    )
    
    task_text <- pick(c(
      "Why are the Welch degrees of freedom not necessarily an integer or equal to n1 + n2 - 2?",
      "Interpret the approximate df and explain what feature of Welch's method produces it.",
      "A trainee thinks the reported df must be a software error. Correct that misunderstanding."
    ))
    
    reference_answer <- paste0(
      "Welch's method estimates the degrees of freedom using the two sample variances and sample sizes. ",
      "The resulting Satterthwaite approximation can be non-integer and is generally smaller than or different from n1 + n2 - 2 because equal variances are not assumed."
    )
    
    solution_steps <- paste0(
      "Explain variance-based adjustment and distinguish Welch df from pooled-test df."
    )
    
  } else if (task_type == "interpret_interval_width") {
    
    output_text <- paste0(
      "95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\nInterval width = ", fmt(diff(ci)),
      "\nStandard error = ", fmt(parts$se)
    )
    
    task_text <- pick(c(
      "Interpret the width of the interval as a statement about precision.",
      "What features of the data contribute to a wider confidence interval?",
      "Explain how the standard error and confidence level determine interval width."
    ))
    
    reference_answer <- paste0(
      "The interval width of ", fmt(diff(ci)),
      " reflects the uncertainty around the estimated mean difference. ",
      "Greater within-group variability, smaller samples or a higher confidence level would widen the interval; a smaller standard error would narrow it."
    )
    
    solution_steps <- paste0(
      "Connect width to critical value times SE and identify sample size and variability as drivers of SE."
    )
    
  } else if (task_type == "interpret_association_not_causation") {
    
    output_text <- paste0(
      "Observed group difference in ", trait,
      " = ", fmt(diff),
      "\np-value ", p_text(tst$p.value)
    )
    
    task_text <- pick(c(
      "Explain why this comparison does not show that belonging to one species causes the trait value.",
      "What can be concluded about association, and what cannot be concluded about causation?",
      "A commentator uses causal language. Rewrite the conclusion appropriately."
    ))
    
    reference_answer <- paste0(
      "The analysis shows an association between species group and average ",
      trait, " in the observed dataset. Species was not assigned as an intervention, so the t-test does not establish a manipulable causal effect. ",
      "The result should be described as a difference in group means."
    )
    
    solution_steps <- paste0(
      "State the observed association and reject causal wording unsupported by the design."
    )
    
  } else if (task_type == "evaluate_prediction_claim") {
    
    output_text <- paste0(
      "Group means differ by ", fmt(diff),
      "\nWithin-group SDs are ", fmt(sd(x)),
      " and ", fmt(sd(y))
    )
    
    task_text <- pick(c(
      "A scout claims the trait alone can perfectly identify group membership. Evaluate that claim.",
      "Why does a difference in means not imply perfect classification?",
      "Explain what additional analysis would be needed before using the trait as a predictor of group."
    ))
    
    reference_answer <- paste0(
      "Different group means do not imply perfect separation because the within-group distributions may overlap. ",
      "A classification analysis, prediction error assessment and validation data would be needed before using ",
      trait, " to identify group membership."
    )
    
    solution_steps <- paste0(
      "Use within-group variability to explain overlap and distinguish mean comparison from classification."
    )
    
  } else if (task_type == "interpret_multiple_testing") {
    
    output_text <- paste0(
      "Several pairwise tests were run across three species and two traits.",
      "\nEach unadjusted test used alpha = 0.05."
    )
    
    task_text <- pick(c(
      "Explain why the overall false-positive risk is larger than 5% and suggest one correction.",
      "Why should the analyst not interpret each pairwise p-value in isolation?",
      "Give a suitable response to the multiple-testing problem."
    ))
    
    reference_answer <- paste0(
      "Running several tests creates multiple opportunities for a false positive, so the family-wise error rate exceeds 5% when each test is judged separately at 0.05. ",
      "Possible responses include Holm or Bonferroni adjustment, or a broader model followed by planned comparisons."
    )
    
    solution_steps <- paste0(
      "Identify the family of tests, explain accumulated Type I error and propose multiplicity control."
    )
    
  } else if (task_type == "interpret_group_overlap") {
    
    output_text <- paste0(
      species_pair[1], " mean = ", fmt(mean(x)),
      ", SD = ", fmt(sd(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      ", SD = ", fmt(sd(y))
    )
    
    task_text <- pick(c(
      "Explain why two groups can have clearly different means and still contain overlapping observations.",
      "What do the means and SDs jointly tell you about separation between the groups?",
      "Why should an analyst avoid turning a mean comparison into a claim about every individual?"
    ))
    
    reference_answer <- paste0(
      "The means describe group centres, while the SDs describe spread around those centres. ",
      "Even if the centres differ, observations from the two groups can overlap. Therefore, the result supports an average difference, not perfect individual separation."
    )
    
    solution_steps <- paste0(
      "Interpret centre and spread together and distinguish group-level inference from individual classification."
    )
    
  } else {
    
    output_text <- paste0(
      "Trait = ", trait,
      "\n", species_pair[1], " mean = ", fmt(mean(x)),
      "\n", species_pair[2], " mean = ", fmt(mean(y)),
      "\nDifference = ", fmt(diff),
      "\n95% CI = [", fmt(ci[1]), ", ", fmt(ci[2]), "]",
      "\nt = ", fmt(tst$statistic),
      ", df = ", fmt(tst$parameter),
      "\np-value ", p_text(tst$p.value),
      "\nCohen's d = ", fmt(d)
    )
    
    task_text <- pick(c(
      "Produce a complete interpretation suitable for a sports analytics report. Include direction, magnitude, uncertainty, statistical evidence and one limitation.",
      "Summarise the comparison in a way that a coach could use without overstating the result.",
      "Write a concise results paragraph using the full output."
    ))
    
    reference_answer <- paste0(
      "The average ", trait, " for ", species_pair[1],
      " is ", fmt(abs(diff)), " units ",
      ifelse(diff > 0, "higher", "lower"),
      " than for ", species_pair[2],
      ". The 95% confidence interval for the first-minus-second difference is [",
      fmt(ci[1]), ", ", fmt(ci[2]),
      "], with t = ", fmt(tst$statistic),
      ", df = ", fmt(tst$parameter),
      " and p ", p_text(tst$p.value),
      ". Cohen's d = ", fmt(d),
      " describes the standardised magnitude. The conclusion concerns group means and does not imply perfect separation or causation."
    )
    
    solution_steps <- paste0(
      "Report direction and raw difference, interpret the interval, state the test evidence, mention effect size and add one limitation."
    )
  }
  
  full_question <- compose_medium_prompt(
    opening,
    output_text,
    task_text,
    layout_id
  )
  
  make_record(
    id = sprintf("R800_017_%03d", i),
    blueprint_id = "R800_017",
    dataset_name = "iris",
    difficulty = "medium",
    scenario = "sports_analytics",
    template_id = paste0("t_test_template_", task_type),
    language_style = style,
    presentation_layout = paste0("layout_", layout_id),
    cognitive_skill = "contextual_output_interpretation",
    variables_used = "Sepal.Length, Petal.Length, Species",
    statistical_output = output_text,
    question = full_question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "short_interpretation"
  )
}

# ============================================================
# Generate datasets
# ============================================================

R800_016 <- do.call(
  rbind,
  lapply(seq_len(15), generate_r800_016)
)

R800_017 <- do.call(
  rbind,
  lapply(seq_len(20), generate_r800_017)
)

ALL <- rbind(R800_016, R800_017)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_016) == 15)
stopifnot(nrow(R800_017) == 20)
stopifnot(nrow(ALL) == 35)
stopifnot(length(unique(ALL$id)) == 35)

stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))
stopifnot(!any(ALL$question == ""))
stopifnot(!any(ALL$reference_answer == ""))

stopifnot(length(unique(R800_016$template_id)) == 15)
stopifnot(length(unique(R800_017$template_id)) == 20)

stopifnot(
  all(
    R800_016$cognitive_skill ==
      "written_reasoning_justification_and_critical_evaluation"
  )
)

stopifnot(
  all(
    R800_017$cognitive_skill ==
      "contextual_output_interpretation"
  )
)

# ============================================================
# Export
# ============================================================

write.csv(
  R800_016,
  "R800_016_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  R800_017,
  "R800_017_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  ALL,
  "R800_016_017_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

jsonlite::write_json(
  R800_016,
  "R800_016_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  R800_017,
  "R800_017_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

jsonlite::write_json(
  ALL,
  "R800_016_017_questions.json",
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
    "answer_type"
  )]
)

cat(
  "\nGenerated successfully:\n",
  "- R800_016: 15 Hard short-answer questions\n",
  "- R800_017: 20 Medium interpretation questions\n",
  "- Separate and combined CSV/JSON files saved\n"
)