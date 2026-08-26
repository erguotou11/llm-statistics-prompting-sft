# ============================================================
# R800_018 + R800_021
#
# R800_018
# Confidence Interval / ToothGrowth / Healthcare
# Medium / Calculation / 25
#
# R800_021
# Confidence Interval / ToothGrowth / Social Survey
# Medium / Calculation / 20
#
# Output:
#   R800_018_021_questions.csv
#   R800_018_021_questions.json
#
# Design principles:
# - Real R data from ToothGrowth
# - Medium difficulty through linked numerical steps
# - Strong semantic and structural variation
# - Realistic healthcare and social-survey narratives
# - One combined CSV and one combined JSON only
# ============================================================

set.seed(80001821)

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
    upper = m + margin
  )
}

welch_diff_ci <- function(x, y, conf_level = 0.95) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- var(x)
  vy <- var(y)

  diff <- mx - my
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
    diff = diff,
    se = se,
    df = df,
    t_star = t_star,
    margin = margin,
    lower = diff - margin,
    upper = diff + margin
  )
}

build_prompt <- function(context, data_block, task_text, layout_id) {
  if (layout_id == 1) {
    paste0(
      context,
      "\n\nNumerical information:\n",
      data_block,
      "\n\n",
      task_text
    )
  } else if (layout_id == 2) {
    paste0(
      task_text,
      "\n\nUse the following values:\n",
      data_block,
      "\n\nContext:\n",
      context
    )
  } else if (layout_id == 3) {
    paste0(
      context,
      "\n\n",
      task_text,
      "\n\nRelevant summary:\n",
      data_block
    )
  } else {
    paste0(
      "Relevant summary:\n",
      data_block,
      "\n\n",
      context,
      "\n\n",
      task_text
    )
  }
}

make_record <- function(
    id,
    blueprint_id,
    scenario,
    template_id,
    language_style,
    presentation_layout,
    cognitive_skill,
    question,
    reference_answer,
    solution_steps
) {
  data.frame(
    id = id,
    source = "R-generated",
    blueprint_id = blueprint_id,
    dataset_name = "ToothGrowth",
    statistical_concept = "confidence_interval",
    task = "confidence_interval",
    template_id = template_id,
    difficulty = "medium",
    scenario = scenario,
    language_style = language_style,
    presentation_layout = presentation_layout,
    cognitive_skill = cognitive_skill,
    question_type = "calculation",
    variables_used = "len, supp, dose",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "numeric_interval_or_multi_value_numeric",
    version = "v1.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_018 — Healthcare
# ============================================================

healthcare_styles <- c(
  "clinical_trial_update",
  "laboratory_report",
  "manuscript_revision",
  "dental_consultation",
  "quality_assurance_note",
  "research_meeting",
  "methods_exam",
  "conference_abstract",
  "regulatory_summary",
  "case_discussion"
)

healthcare_openings <- list(

  clinical_trial_update = c(
    "An interim clinical update summarises tooth-growth outcomes for one treatment condition in the ToothGrowth study.",
    "A treatment-monitoring report requires a confidence interval before the next review meeting.",
    "Investigators are preparing an updated estimate of average tooth length under a selected supplement and dose."
  ),

  laboratory_report = c(
    "A laboratory report contains the sample mean and variability but leaves the interval estimate incomplete.",
    "The experimental team has verified the raw observations and now needs an uncertainty range for the reported mean.",
    "A laboratory summary is being checked before the results are transferred into the main study table."
  ),

  manuscript_revision = c(
    "A journal reviewer asks the authors to replace a point estimate with a confidence interval.",
    "During manuscript revision, the research team is asked to report both effect size and uncertainty.",
    "The draft results section gives an average difference but not the corresponding interval."
  ),

  dental_consultation = c(
    "A dental researcher asks a statistician to quantify the precision of a treatment estimate.",
    "During a consultation, the clinician wants to know the plausible range for the population mean rather than only the sample mean.",
    "A collaborator requests an interval estimate that can be explained in a clinical meeting."
  ),

  quality_assurance_note = c(
    "A quality-assurance check compares the reported confidence limits with values recalculated from the dataset.",
    "The audit team is verifying whether the margin of error was computed correctly.",
    "A data-quality note flags a missing interval width in the treatment summary."
  ),

  research_meeting = c(
    "Research lead: \"We have the mean, but how precise is it?\"\nStatistician: \"We can answer that with a t-based interval.\"",
    "Clinician: \"The two groups differ in average length. What range of population differences is plausible?\"\nAnalyst: \"Let us construct the Welch interval.\"",
    "Project manager: \"Can we compare the precision of two estimates?\"\nBiostatistician: \"Yes, but we need both standard errors and interval widths.\""
  ),

  methods_exam = c(
    "A healthcare statistics assessment asks students to complete a t-based confidence interval from real data.",
    "A methods examination presents summary values from ToothGrowth and requires a multi-step interval calculation.",
    "A quantitative health-science question focuses on constructing and comparing confidence intervals."
  ),

  conference_abstract = c(
    "A conference abstract must report a concise interval estimate for the selected treatment group.",
    "The abstract currently contains only a point estimate and needs a confidence interval added.",
    "A poster draft requires the treatment difference and its 95% interval."
  ),

  regulatory_summary = c(
    "A regulatory-style summary requests a transparent calculation of uncertainty around the mean response.",
    "A technical review document requires the lower and upper confidence limits to be shown explicitly.",
    "The statistical appendix must document the critical value, standard error and interval."
  ),

  case_discussion = c(
    "A case discussion compares two supplement conditions and asks whether the interval includes zero.",
    "A teaching case asks how the confidence level affects interval width.",
    "A group review focuses on the practical meaning of a confidence interval for tooth-growth outcomes."
  )
)

healthcare_tasks <- c(
  "one_mean_95",
  "one_mean_90",
  "welch_diff_95",
  "welch_diff_99",
  "compare_two_group_widths",
  "compare_confidence_levels",
  "recover_margin_and_interval",
  "required_n_for_margin",
  "dose_mean_interval",
  "supplement_overall_interval"
)

generate_r800_018 <- function(i) {

  task_type <- healthcare_tasks[(i - 1) %% length(healthcare_tasks) + 1]
  style <- pick(healthcare_styles)
  context <- pick(healthcare_openings[[style]])
  layout_id <- sample(1:4, 1)

  dose <- pick(sort(unique(TG$dose)))
  supp <- pick(levels(TG$supp))
  group <- subset(TG, dose == dose & supp == supp)$len

  if (task_type == "one_mean_95") {

    ci <- one_mean_ci(group, 0.95)

    data_block <- paste0(
      "Supplement = ", supp,
      "\nDose = ", dose,
      "\nn = ", ci$n,
      "\nSample mean = ", fmt(ci$mean),
      "\nSample SD = ", fmt(ci$sd)
    )

    task_text <- pick(c(
      "Construct the 95% confidence interval for the population mean tooth length. Show the standard error, critical value, margin of error and final limits.",
      "Using a t interval, calculate the 95% range of plausible values for the population mean.",
      "Complete the full interval calculation rather than reporting only the endpoints."
    ))

    answer <- paste0(
      "SE = ", fmt(ci$se),
      "; t* = ", fmt(ci$t_star),
      "; margin = ", fmt(ci$margin),
      "; 95% CI [", fmt(ci$lower), ", ", fmt(ci$upper), "]"
    )

    solution <- paste0(
      "SE = ", fmt(ci$sd), "/sqrt(", ci$n,
      ") = ", fmt(ci$se), ".\n",
      "t* = ", fmt(ci$t_star), " with df = ", ci$df, ".\n",
      "Margin = ", fmt(ci$t_star), " × ",
      fmt(ci$se), " = ", fmt(ci$margin), ".\n",
      "CI = ", fmt(ci$mean), " ± ", fmt(ci$margin),
      " = [", fmt(ci$lower), ", ", fmt(ci$upper), "]."
    )

  } else if (task_type == "one_mean_90") {

    ci <- one_mean_ci(group, 0.90)

    data_block <- paste0(
      "Supplement = ", supp,
      "\nDose = ", dose,
      "\nn = ", ci$n,
      "\nMean = ", fmt(ci$mean),
      "\nSD = ", fmt(ci$sd),
      "\nConfidence level = 90%"
    )

    task_text <- pick(c(
      "Construct the 90% confidence interval for the population mean and report its width.",
      "Calculate the 90% t interval and the total distance between its lower and upper limits.",
      "Find the standard error, margin of error, endpoints and interval width."
    ))

    width <- ci$upper - ci$lower

    answer <- paste0(
      "90% CI [", fmt(ci$lower), ", ", fmt(ci$upper),
      "]; width = ", fmt(width)
    )

    solution <- paste0(
      "SE = ", fmt(ci$se), ".\n",
      "t* = ", fmt(ci$t_star), ".\n",
      "Margin = ", fmt(ci$margin), ".\n",
      "90% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "].\n",
      "Width = ", fmt(ci$upper), " - ",
      fmt(ci$lower), " = ", fmt(width), "."
    )

  } else if (task_type == "welch_diff_95") {

    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    ci <- welch_diff_ci(x, y, 0.95)

    data_block <- paste0(
      "Dose = ", dose,
      "\nOJ: n = ", ci$nx,
      ", mean = ", fmt(ci$mean_x),
      ", variance = ", fmt(ci$var_x),
      "\nVC: n = ", ci$ny,
      ", mean = ", fmt(ci$mean_y),
      ", variance = ", fmt(ci$var_y)
    )

    task_text <- pick(c(
      "Construct the 95% Welch confidence interval for the population mean difference OJ minus VC. Report the estimated difference, SE, df, margin and endpoints.",
      "Calculate the unequal-variance 95% interval for the supplement contrast.",
      "Find the full confidence interval for mu_OJ - mu_VC and determine whether zero lies inside it."
    ))

    includes_zero <- ci$lower <= 0 && ci$upper >= 0

    answer <- paste0(
      "difference = ", fmt(ci$diff),
      "; SE = ", fmt(ci$se),
      "; df = ", fmt(ci$df),
      "; 95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]; ",
      ifelse(includes_zero, "includes zero", "excludes zero")
    )

    solution <- paste0(
      "Difference = ", fmt(ci$mean_x), " - ",
      fmt(ci$mean_y), " = ", fmt(ci$diff), ".\n",
      "SE = ", fmt(ci$se), ".\n",
      "Welch df = ", fmt(ci$df), ".\n",
      "Margin = ", fmt(ci$margin), ".\n",
      "95% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "].\n",
      ifelse(
        includes_zero,
        "Zero is inside the interval.",
        "Zero is outside the interval."
      )
    )

  } else if (task_type == "welch_diff_99") {

    x <- subset(TG, dose == dose & supp == "OJ")$len
    y <- subset(TG, dose == dose & supp == "VC")$len
    ci <- welch_diff_ci(x, y, 0.99)

    data_block <- paste0(
      "Dose = ", dose,
      "\nMean(OJ) = ", fmt(ci$mean_x),
      "\nMean(VC) = ", fmt(ci$mean_y),
      "\nWelch SE = ", fmt(ci$se),
      "\nWelch df = ", fmt(ci$df),
      "\nConfidence level = 99%"
    )

    task_text <- pick(c(
      "Construct the 99% confidence interval for OJ minus VC and calculate its width.",
      "Use the supplied Welch quantities to obtain the 99% interval and total interval width.",
      "Calculate the critical value, margin, limits and width for the 99% mean-difference interval."
    ))

    width <- ci$upper - ci$lower

    answer <- paste0(
      "99% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]; width = ", fmt(width)
    )

    solution <- paste0(
      "t* = ", fmt(ci$t_star), ".\n",
      "Margin = ", fmt(ci$t_star), " × ",
      fmt(ci$se), " = ", fmt(ci$margin), ".\n",
      "99% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "].\n",
      "Width = ", fmt(width), "."
    )

  } else if (task_type == "compare_two_group_widths") {

    g1 <- subset(TG, dose == dose & supp == "OJ")$len
    g2 <- subset(TG, dose == dose & supp == "VC")$len

    ci1 <- one_mean_ci(g1, 0.95)
    ci2 <- one_mean_ci(g2, 0.95)

    width1 <- ci1$upper - ci1$lower
    width2 <- ci2$upper - ci2$lower
    wider <- ifelse(width1 > width2, "OJ", "VC")

    data_block <- paste0(
      "Dose = ", dose,
      "\nOJ: n = ", ci1$n,
      ", mean = ", fmt(ci1$mean),
      ", SD = ", fmt(ci1$sd),
      "\nVC: n = ", ci2$n,
      ", mean = ", fmt(ci2$mean),
      ", SD = ", fmt(ci2$sd)
    )

    task_text <- pick(c(
      "Construct separate 95% confidence intervals for the OJ and VC means. Compare their widths and identify which estimate is less precise.",
      "Calculate both group intervals and determine which one is wider.",
      "Find the two 95% intervals, their widths and the difference between those widths."
    ))

    answer <- paste0(
      "OJ CI [", fmt(ci1$lower), ", ", fmt(ci1$upper),
      "], width = ", fmt(width1),
      "; VC CI [", fmt(ci2$lower), ", ", fmt(ci2$upper),
      "], width = ", fmt(width2),
      "; wider = ", wider
    )

    solution <- paste0(
      "OJ margin = ", fmt(ci1$margin),
      ", interval = [", fmt(ci1$lower), ", ",
      fmt(ci1$upper), "], width = ", fmt(width1), ".\n",
      "VC margin = ", fmt(ci2$margin),
      ", interval = [", fmt(ci2$lower), ", ",
      fmt(ci2$upper), "], width = ", fmt(width2), ".\n",
      wider, " has the wider interval and therefore the less precise mean estimate."
    )

  } else if (task_type == "compare_confidence_levels") {

    ci90 <- one_mean_ci(group, 0.90)
    ci95 <- one_mean_ci(group, 0.95)
    width90 <- ci90$upper - ci90$lower
    width95 <- ci95$upper - ci95$lower
    increase <- width95 - width90

    data_block <- paste0(
      "Supplement = ", supp,
      "\nDose = ", dose,
      "\nn = ", length(group),
      "\nMean = ", fmt(mean(group)),
      "\nSD = ", fmt(sd(group))
    )

    task_text <- pick(c(
      "Construct both the 90% and 95% confidence intervals. Compare their widths and calculate how much wider the 95% interval is.",
      "Using the same sample, calculate two intervals at different confidence levels and quantify the change in width.",
      "Find both intervals, both widths and the width increase caused by moving from 90% to 95% confidence."
    ))

    answer <- paste0(
      "90% CI [", fmt(ci90$lower), ", ", fmt(ci90$upper),
      "], width = ", fmt(width90),
      "; 95% CI [", fmt(ci95$lower), ", ", fmt(ci95$upper),
      "], width = ", fmt(width95),
      "; width increase = ", fmt(increase)
    )

    solution <- paste0(
      "90% interval = [", fmt(ci90$lower), ", ",
      fmt(ci90$upper), "], width = ", fmt(width90), ".\n",
      "95% interval = [", fmt(ci95$lower), ", ",
      fmt(ci95$upper), "], width = ", fmt(width95), ".\n",
      "Increase in width = ", fmt(width95), " - ",
      fmt(width90), " = ", fmt(increase), "."
    )

  } else if (task_type == "recover_margin_and_interval") {

    ci <- one_mean_ci(group, 0.95)

    data_block <- paste0(
      "Supplement = ", supp,
      "\nDose = ", dose,
      "\nSample mean = ", fmt(ci$mean),
      "\n95% t critical value = ", fmt(ci$t_star),
      "\nStandard error = ", fmt(ci$se)
    )

    task_text <- pick(c(
      "Recover the margin of error and then construct the 95% confidence interval.",
      "Use the supplied critical value and standard error to complete the missing interval.",
      "Calculate t* × SE and use it to obtain the lower and upper limits."
    ))

    answer <- paste0(
      "margin = ", fmt(ci$margin),
      "; 95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]"
    )

    solution <- paste0(
      "Margin = ", fmt(ci$t_star), " × ",
      fmt(ci$se), " = ", fmt(ci$margin), ".\n",
      "Lower = ", fmt(ci$mean), " - ",
      fmt(ci$margin), " = ", fmt(ci$lower), ".\n",
      "Upper = ", fmt(ci$mean), " + ",
      fmt(ci$margin), " = ", fmt(ci$upper), "."
    )

  } else if (task_type == "required_n_for_margin") {

    s <- sd(group)
    target_margin <- pick(c(1.0, 1.25, 1.5, 1.75))
    z_star <- qnorm(0.975)

    required_n <- ceiling(
      (z_star * s / target_margin)^2
    )

    achieved_margin <- z_star * s / sqrt(required_n)

    data_block <- paste0(
      "Planning SD = ", fmt(s),
      "\nTarget 95% margin of error = ", target_margin,
      "\nUse z* = ", fmt(z_star)
    )

    task_text <- pick(c(
      "Estimate the minimum sample size required to achieve the target margin of error. Then verify the approximate margin obtained at that sample size.",
      "Use the planning formula n = (z*s/E)^2, round up, and check the resulting margin.",
      "Calculate the required n for the desired precision and confirm that the rounded-up sample size meets the target."
    ))

    answer <- paste0(
      "required n = ", required_n,
      "; achieved approximate margin = ",
      fmt(achieved_margin)
    )

    solution <- paste0(
      "n = (", fmt(z_star), " × ",
      fmt(s), "/", target_margin, ")^2 = ",
      fmt((z_star * s / target_margin)^2), ".\n",
      "Round up to n = ", required_n, ".\n",
      "Achieved margin ≈ ", fmt(z_star), " × ",
      fmt(s), "/sqrt(", required_n,
      ") = ", fmt(achieved_margin), "."
    )

  } else if (task_type == "dose_mean_interval") {

    dose_group <- subset(TG, dose == dose)$len
    ci <- one_mean_ci(dose_group, 0.95)

    data_block <- paste0(
      "Dose = ", dose,
      "\nBoth supplements combined",
      "\nn = ", ci$n,
      "\nMean = ", fmt(ci$mean),
      "\nSD = ", fmt(ci$sd)
    )

    task_text <- pick(c(
      "Construct the 95% confidence interval for the overall mean tooth length at this dose and report the margin of error.",
      "Calculate a combined-dose interval using all OJ and VC observations at the selected dose.",
      "Find SE, t*, margin and endpoints for the dose-level mean."
    ))

    answer <- paste0(
      "margin = ", fmt(ci$margin),
      "; 95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]"
    )

    solution <- paste0(
      "SE = ", fmt(ci$se), ".\n",
      "t* = ", fmt(ci$t_star), ".\n",
      "Margin = ", fmt(ci$margin), ".\n",
      "95% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]."
    )

  } else {

    supp_group <- subset(TG, supp == supp)$len
    ci <- one_mean_ci(supp_group, 0.95)

    data_block <- paste0(
      "Supplement = ", supp,
      "\nAll dose levels combined",
      "\nn = ", ci$n,
      "\nMean = ", fmt(ci$mean),
      "\nSD = ", fmt(ci$sd)
    )

    task_text <- pick(c(
      "Construct the 95% confidence interval for the overall mean tooth length for this supplement and calculate the interval width.",
      "Using all dose levels, find the t-based confidence interval and its total width.",
      "Calculate the margin, limits and width for the supplement-level mean."
    ))

    width <- ci$upper - ci$lower

    answer <- paste0(
      "95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]; width = ", fmt(width)
    )

    solution <- paste0(
      "SE = ", fmt(ci$se), ".\n",
      "Margin = ", fmt(ci$margin), ".\n",
      "95% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "].\n",
      "Width = ", fmt(width), "."
    )
  }

  full_question <- build_prompt(
    context,
    data_block,
    task_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_018_%03d", i),
    blueprint_id = "R800_018",
    scenario = "healthcare",
    template_id = paste0(
      "ci_t_interval_template_",
      task_type
    ),
    language_style = style,
    presentation_layout = paste0(
      "layout_",
      layout_id
    ),
    cognitive_skill = "multi_step_interval_construction_and_numerical_comparison",
    question = full_question,
    reference_answer = answer,
    solution_steps = solution
  )
}

# ============================================================
# R800_021 — Social Survey
#
# The real ToothGrowth values are used as anonymised pilot-study
# scores in a survey-methods training scenario:
#   len  -> response score
#   supp -> survey delivery format
#   dose -> contact-intensity level
# ============================================================

survey_styles <- c(
  "public_opinion_brief",
  "survey_methods_class",
  "community_consultation",
  "polling_memo",
  "questionnaire_pilot",
  "fieldwork_debrief",
  "policy_research_note",
  "respondent_experience_report",
  "editorial_fact_check",
  "analyst_dialogue"
)

survey_openings <- list(

  public_opinion_brief = c(
    "A public-opinion brief uses anonymised pilot scores to estimate the average response under one survey condition.",
    "A social research team is preparing an interval estimate for a community-response measure.",
    "A briefing note needs a confidence interval rather than a single reported average."
  ),

  survey_methods_class = c(
    "A survey-methods class uses the ToothGrowth values as anonymised response scores for a calculation exercise.",
    "Students relabel len as a continuous response score, supp as delivery format and dose as contact intensity.",
    "A methods workshop practises t-based confidence intervals using real R data under a survey scenario."
  ),

  community_consultation = c(
    "A community consultation pilot records a continuous engagement score under two contact formats.",
    "The consultation team wants a plausible range for the average response score.",
    "A local engagement study compares mean scores across two survey delivery conditions."
  ),

  polling_memo = c(
    "A polling memo reports a point estimate but omits the associated uncertainty.",
    "The polling team is checking whether two delivery formats differ in average response score.",
    "A short polling report requires a confidence interval for the mean difference."
  ),

  questionnaire_pilot = c(
    "A questionnaire pilot tests two delivery formats at several contact-intensity levels.",
    "Pilot-study scores are being used to estimate the precision of the survey mean.",
    "The questionnaire team needs a confidence interval before selecting a fieldwork design."
  ),

  fieldwork_debrief = c(
    "The fieldwork debrief includes group means, standard deviations and sample sizes.",
    "After the pilot, the team compares the precision of estimates from two survey conditions.",
    "A fieldwork review asks how interval width changes when confidence level or sample size changes."
  ),

  policy_research_note = c(
    "A policy research note summarises an anonymised response score with a t-based confidence interval.",
    "A social-policy team is comparing average scores under two modes of contact.",
    "The research note must show the estimated difference and its uncertainty."
  ),

  respondent_experience_report = c(
    "A respondent-experience report treats the numerical len values as anonymised survey scores.",
    "The research team wants to estimate the mean score for a selected delivery format.",
    "A pilot report compares average respondent scores across contact conditions."
  ),

  editorial_fact_check = c(
    "An editor asks the survey analyst to verify the reported interval limits.",
    "A fact-check identifies a missing margin of error in a public-facing survey summary.",
    "The statistical figures in a survey article are being independently recalculated."
  ),

  analyst_dialogue = c(
    "Research manager: \"The average looks different, but what range of differences is plausible?\"\nSurvey analyst: \"We need a confidence interval for the mean contrast.\"",
    "Editor: \"Can we report only the point estimate?\"\nMethodologist: \"Not if we want readers to see the uncertainty.\"",
    "Fieldwork lead: \"Would a larger sample materially narrow the interval?\"\nStatistician: \"Let us calculate the change in precision.\""
  )
)

survey_tasks <- c(
  "mode_mean_95",
  "mode_diff_95",
  "contact_level_interval",
  "compare_mode_widths",
  "compare_90_95",
  "margin_recovery",
  "sample_size_planning",
  "difference_and_relative_width"
)

generate_r800_021 <- function(i) {

  task_type <- survey_tasks[(i - 1) %% length(survey_tasks) + 1]
  style <- pick(survey_styles)
  context <- pick(survey_openings[[style]])
  layout_id <- sample(1:4, 1)

  contact_level <- pick(sort(unique(TG$dose)))
  mode <- pick(levels(TG$supp))
  group <- subset(
    TG,
    dose == contact_level & supp == mode
  )$len

  if (task_type == "mode_mean_95") {

    ci <- one_mean_ci(group, 0.95)

    data_block <- paste0(
      "Delivery format = ", mode,
      "\nContact-intensity level = ", contact_level,
      "\nn = ", ci$n,
      "\nMean response score = ", fmt(ci$mean),
      "\nSample SD = ", fmt(ci$sd)
    )

    task_text <- pick(c(
      "Construct the 95% confidence interval for the population mean response score. Include SE, t*, margin and endpoints.",
      "Calculate the full t interval for the average pilot score.",
      "Find the plausible 95% range for the population mean under this survey condition."
    ))

    answer <- paste0(
      "SE = ", fmt(ci$se),
      "; margin = ", fmt(ci$margin),
      "; 95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]"
    )

    solution <- paste0(
      "SE = ", fmt(ci$sd), "/sqrt(", ci$n,
      ") = ", fmt(ci$se), ".\n",
      "t* = ", fmt(ci$t_star), ".\n",
      "Margin = ", fmt(ci$margin), ".\n",
      "95% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]."
    )

  } else if (task_type == "mode_diff_95") {

    x <- subset(
      TG,
      dose == contact_level & supp == "OJ"
    )$len

    y <- subset(
      TG,
      dose == contact_level & supp == "VC"
    )$len

    ci <- welch_diff_ci(x, y, 0.95)
    includes_zero <- ci$lower <= 0 && ci$upper >= 0

    data_block <- paste0(
      "Contact-intensity level = ", contact_level,
      "\nFormat OJ: mean = ", fmt(ci$mean_x),
      ", variance = ", fmt(ci$var_x),
      ", n = ", ci$nx,
      "\nFormat VC: mean = ", fmt(ci$mean_y),
      ", variance = ", fmt(ci$var_y),
      ", n = ", ci$ny
    )

    task_text <- pick(c(
      "Construct the 95% Welch interval for the difference in population mean response scores, OJ minus VC. State whether zero is included.",
      "Calculate the interval estimate for the delivery-format contrast and determine whether no difference remains plausible.",
      "Find the estimated mean difference, SE, df, margin and confidence limits."
    ))

    answer <- paste0(
      "difference = ", fmt(ci$diff),
      "; 95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]; ",
      ifelse(includes_zero, "includes zero", "excludes zero")
    )

    solution <- paste0(
      "Difference = ", fmt(ci$diff), ".\n",
      "SE = ", fmt(ci$se), ".\n",
      "Welch df = ", fmt(ci$df), ".\n",
      "Margin = ", fmt(ci$margin), ".\n",
      "95% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]."
    )

  } else if (task_type == "contact_level_interval") {

    level_group <- subset(
      TG,
      dose == contact_level
    )$len

    ci <- one_mean_ci(level_group, 0.95)

    data_block <- paste0(
      "Contact-intensity level = ", contact_level,
      "\nBoth delivery formats combined",
      "\nn = ", ci$n,
      "\nMean score = ", fmt(ci$mean),
      "\nSD = ", fmt(ci$sd)
    )

    task_text <- pick(c(
      "Construct the 95% confidence interval for the overall mean response score at this contact level.",
      "Using both delivery formats together, calculate the t-based interval and margin of error.",
      "Find SE, t*, margin and endpoints for the combined contact-level mean."
    ))

    answer <- paste0(
      "margin = ", fmt(ci$margin),
      "; 95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]"
    )

    solution <- paste0(
      "SE = ", fmt(ci$se), ".\n",
      "t* = ", fmt(ci$t_star), ".\n",
      "Margin = ", fmt(ci$margin), ".\n",
      "95% CI = [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]."
    )

  } else if (task_type == "compare_mode_widths") {

    x <- subset(
      TG,
      dose == contact_level & supp == "OJ"
    )$len

    y <- subset(
      TG,
      dose == contact_level & supp == "VC"
    )$len

    ci_x <- one_mean_ci(x, 0.95)
    ci_y <- one_mean_ci(y, 0.95)

    width_x <- ci_x$upper - ci_x$lower
    width_y <- ci_y$upper - ci_y$lower

    wider <- ifelse(
      width_x > width_y,
      "OJ",
      "VC"
    )

    data_block <- paste0(
      "Contact level = ", contact_level,
      "\nOJ: n = ", ci_x$n,
      ", mean = ", fmt(ci_x$mean),
      ", SD = ", fmt(ci_x$sd),
      "\nVC: n = ", ci_y$n,
      ", mean = ", fmt(ci_y$mean),
      ", SD = ", fmt(ci_y$sd)
    )

    task_text <- pick(c(
      "Construct separate 95% intervals for the two delivery-format means and compare their widths.",
      "Calculate both intervals, identify the less precise estimate and report the width difference.",
      "Which survey-format mean is estimated less precisely? Support the answer numerically."
    ))

    answer <- paste0(
      "OJ width = ", fmt(width_x),
      "; VC width = ", fmt(width_y),
      "; wider = ", wider,
      "; width difference = ",
      fmt(abs(width_x - width_y))
    )

    solution <- paste0(
      "OJ CI = [", fmt(ci_x$lower), ", ",
      fmt(ci_x$upper), "], width = ",
      fmt(width_x), ".\n",
      "VC CI = [", fmt(ci_y$lower), ", ",
      fmt(ci_y$upper), "], width = ",
      fmt(width_y), ".\n",
      wider, " is less precise because its interval is wider."
    )

  } else if (task_type == "compare_90_95") {

    ci90 <- one_mean_ci(group, 0.90)
    ci95 <- one_mean_ci(group, 0.95)

    width90 <- ci90$upper - ci90$lower
    width95 <- ci95$upper - ci95$lower
    pct_increase <- (
      (width95 - width90) / width90
    ) * 100

    data_block <- paste0(
      "Delivery format = ", mode,
      "\nContact level = ", contact_level,
      "\nn = ", length(group),
      "\nMean = ", fmt(mean(group)),
      "\nSD = ", fmt(sd(group))
    )

    task_text <- pick(c(
      "Construct both the 90% and 95% confidence intervals and calculate the percentage increase in width.",
      "Quantify how much precision is lost when confidence rises from 90% to 95%.",
      "Find both intervals, their widths and the relative widening."
    ))

    answer <- paste0(
      "90% width = ", fmt(width90),
      "; 95% width = ", fmt(width95),
      "; percentage increase = ",
      fmt(pct_increase, 1), "%"
    )

    solution <- paste0(
      "90% CI = [", fmt(ci90$lower), ", ",
      fmt(ci90$upper), "], width = ",
      fmt(width90), ".\n",
      "95% CI = [", fmt(ci95$lower), ", ",
      fmt(ci95$upper), "], width = ",
      fmt(width95), ".\n",
      "Percentage increase = (",
      fmt(width95), " - ", fmt(width90),
      ")/", fmt(width90), " × 100 = ",
      fmt(pct_increase, 1), "%."
    )

  } else if (task_type == "margin_recovery") {

    ci <- one_mean_ci(group, 0.95)

    data_block <- paste0(
      "Mean response score = ", fmt(ci$mean),
      "\nStandard error = ", fmt(ci$se),
      "\n95% t critical value = ", fmt(ci$t_star)
    )

    task_text <- pick(c(
      "Calculate the margin of error and recover the lower and upper confidence limits.",
      "Complete the missing interval from the supplied centre, SE and t critical value.",
      "Find the plus-or-minus quantity and the final 95% interval."
    ))

    answer <- paste0(
      "margin = ", fmt(ci$margin),
      "; 95% CI [", fmt(ci$lower), ", ",
      fmt(ci$upper), "]"
    )

    solution <- paste0(
      "Margin = ", fmt(ci$t_star), " × ",
      fmt(ci$se), " = ", fmt(ci$margin), ".\n",
      "CI = ", fmt(ci$mean), " ± ",
      fmt(ci$margin), " = [",
      fmt(ci$lower), ", ", fmt(ci$upper), "]."
    )

  } else if (task_type == "sample_size_planning") {

    s <- sd(group)
    target_margin <- pick(c(1.0, 1.25, 1.5))
    z_star <- qnorm(0.975)

    n_required <- ceiling(
      (z_star * s / target_margin)^2
    )

    achieved <- z_star * s / sqrt(n_required)

    data_block <- paste0(
      "Planning SD = ", fmt(s),
      "\nTarget 95% margin = ", target_margin,
      "\nUse z* = ", fmt(z_star)
    )

    task_text <- pick(c(
      "Estimate the minimum sample size for the desired margin and verify the margin after rounding up.",
      "Use the planning formula to determine how many survey responses are required.",
      "Calculate n, round upward and check the achieved approximate precision."
    ))

    answer <- paste0(
      "required n = ", n_required,
      "; achieved margin = ", fmt(achieved)
    )

    solution <- paste0(
      "n = (", fmt(z_star), " × ",
      fmt(s), "/", target_margin,
      ")^2 = ",
      fmt((z_star * s / target_margin)^2),
      ".\nRound up to ", n_required,
      ".\nAchieved margin = ",
      fmt(achieved), "."
    )

  } else {

    x <- subset(
      TG,
      dose == contact_level & supp == "OJ"
    )$len

    y <- subset(
      TG,
      dose == contact_level & supp == "VC"
    )$len

    ci <- welch_diff_ci(x, y, 0.95)
    width <- ci$upper - ci$lower

    relative_width <- (
      width / abs(ci$diff)
    ) * 100

    data_block <- paste0(
      "Contact level = ", contact_level,
      "\nEstimated format difference = ",
      fmt(ci$diff),
      "\n95% interval = [",
      fmt(ci$lower), ", ",
      fmt(ci$upper), "]"
    )

    task_text <- pick(c(
      "Calculate the interval width and express it as a percentage of the absolute point estimate.",
      "Measure the uncertainty relative to the estimated delivery-format difference.",
      "Find the total confidence-interval width and the relative width percentage."
    ))

    answer <- paste0(
      "width = ", fmt(width),
      "; relative width = ",
      fmt(relative_width, 1), "%"
    )

    solution <- paste0(
      "Width = ", fmt(ci$upper), " - ",
      fmt(ci$lower), " = ", fmt(width), ".\n",
      "Relative width = ", fmt(width), "/|",
      fmt(ci$diff), "| × 100 = ",
      fmt(relative_width, 1), "%."
    )
  }

  full_question <- build_prompt(
    context,
    data_block,
    task_text,
    layout_id
  )

  make_record(
    id = sprintf("R800_021_%03d", i),
    blueprint_id = "R800_021",
    scenario = "social_survey",
    template_id = paste0(
      "ci_t_interval_template_",
      task_type
    ),
    language_style = style,
    presentation_layout = paste0(
      "layout_",
      layout_id
    ),
    cognitive_skill = "multi_step_interval_construction_and_precision_analysis",
    question = full_question,
    reference_answer = answer,
    solution_steps = solution
  )
}

# ============================================================
# Generate both blueprints
# ============================================================

R800_018 <- do.call(
  rbind,
  lapply(seq_len(25), generate_r800_018)
)

R800_021 <- do.call(
  rbind,
  lapply(seq_len(20), generate_r800_021)
)

ALL <- rbind(
  R800_018,
  R800_021
)

# ============================================================
# Validation
# ============================================================

stopifnot(nrow(R800_018) == 25)
stopifnot(nrow(R800_021) == 20)
stopifnot(nrow(ALL) == 45)
stopifnot(length(unique(ALL$id)) == 45)

stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))

stopifnot(!any(ALL$question == ""))
stopifnot(!any(ALL$reference_answer == ""))
stopifnot(!any(ALL$solution_steps == ""))

# ============================================================
# Export only one combined CSV and one combined JSON
# ============================================================

write.csv(
  ALL,
  "R800_018_021_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

jsonlite::write_json(
  ALL,
  "R800_018_021_questions.json",
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
    "reference_answer"
  )]
)

cat(
  "\nGenerated successfully:\n",
  "- R800_018: 25 Healthcare questions\n",
  "- R800_021: 20 Social Survey questions\n",
  "- Combined CSV: R800_018_021_questions.csv\n",
  "- Combined JSON: R800_018_021_questions.json\n"
)
