# ============================================================
# R800_025 + R800_026
# ANOVA generators using PlantGrowth and iris
#
# Output:
#   R800_025_026_questions.csv
#   R800_025_026_questions.json
# ============================================================

set.seed(800026)

data(PlantGrowth)
data(iris)

PG <- PlantGrowth
IR <- iris

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

fmt <- function(x, digits = 3) {
  format(
    round(as.numeric(x), digits),
    nsmall = digits,
    trim = TRUE,
    scientific = FALSE
  )
}

pick <- function(x) sample(x, 1)

p_text <- function(p) {
  if (p < 0.001) "< 0.001" else paste0("= ", fmt(p))
}

anova_values <- function(y, g) {
  dat <- data.frame(y = as.numeric(y), g = factor(g))
  fit <- aov(y ~ g, data = dat)
  tab <- summary(fit)[[1]]

  means <- tapply(dat$y, dat$g, mean)
  sizes <- table(dat$g)
  grand <- mean(dat$y)

  ssb <- as.numeric(tab["g", "Sum Sq"])
  sse <- as.numeric(tab["Residuals", "Sum Sq"])
  sst <- ssb + sse

  dfb <- as.numeric(tab["g", "Df"])
  dfe <- as.numeric(tab["Residuals", "Df"])

  msb <- as.numeric(tab["g", "Mean Sq"])
  mse <- as.numeric(tab["Residuals", "Mean Sq"])
  fval <- as.numeric(tab["g", "F value"])
  pval <- as.numeric(tab["g", "Pr(>F)"])

  list(
    means = means,
    sizes = sizes,
    grand = grand,
    ssb = ssb,
    sse = sse,
    sst = sst,
    dfb = dfb,
    dfe = dfe,
    msb = msb,
    mse = mse,
    fval = fval,
    pval = pval,
    eta2 = ssb / sst
  )
}

layout_question <- function(context, numbers, task, layout) {
  switch(
    as.character(layout),
    "1" = paste0(context, "\n\nNumerical information:\n", numbers, "\n\n", task),
    "2" = paste0(task, "\n\nUse these values:\n", numbers, "\n\nContext:\n", context),
    "3" = paste0(context, "\n\n", task, "\n\nRelevant summary:\n", numbers),
    paste0("Relevant summary:\n", numbers, "\n\n", context, "\n\n", task)
  )
}

make_row <- function(id, blueprint, dataset, scenario, style,
                     template, skill, variables, question,
                     answer, steps) {
  data.frame(
    id = id,
    source = "R-generated",
    blueprint_id = blueprint,
    dataset_name = dataset,
    statistical_concept = "anova",
    task = "ANOVA",
    template_id = template,
    difficulty = "medium",
    scenario = scenario,
    language_style = style,
    cognitive_skill = skill,
    question_type = "calculation",
    variables_used = variables,
    question = question,
    reference_answer = answer,
    solution_steps = steps,
    answer_type = "numeric_or_numeric_decision",
    version = "v1.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_025: PlantGrowth / Agriculture / Medium / Calculation
# ============================================================

pg_contexts <- list(
  field_trial = c(
    "A field trial compares final plant weights under a control programme and two alternative growing treatments.",
    "The agronomy team has completed a three-condition crop trial and is checking the ANOVA before presenting it to growers."
  ),
  greenhouse_log = c(
    "A greenhouse log contains three sets of final plant weights, but several ANOVA entries are still blank.",
    "The greenhouse manager wants to separate treatment variation from plant-to-plant variation."
  ),
  cooperative_brief = c(
    "A growers' cooperative is deciding whether either new treatment changes average plant weight enough to justify a larger trial.",
    "Before changing practice across its farms, the cooperative requests a complete numerical ANOVA."
  ),
  journal_review = c(
    "A journal reviewer asks the authors to show how their reported F statistic follows from the group summaries.",
    "The results section lists treatment means but omits some of the supporting ANOVA calculations."
  ),
  extension_note = c(
    "An agricultural extension service is preparing a worked example on comparing several cultivation programmes.",
    "A technical bulletin for growers needs a transparent calculation of between- and within-group variation."
  ),
  research_meeting = c(
    "Trial lead: \"The means look different, but how large is the treatment signal relative to residual variation?\"\nStatistician: \"The ANOVA table will answer that.\"",
    "Farm manager: \"Can we move straight to pairwise comparisons?\"\nAnalyst: \"Only after checking the omnibus test.\""
  ),
  audit_check = c(
    "A data-quality audit is checking whether the sums of squares and mean squares in the trial report are internally consistent.",
    "The validation team has hidden one ANOVA cell and asks the analyst to reconstruct it."
  ),
  teaching_case = c(
    "A crop-science class uses PlantGrowth for a medium-level ANOVA calculation.",
    "Students receive a partially completed agricultural ANOVA table and must recover the missing quantities."
  )
)

pg_tasks <- rep(c(
  "means",
  "ss_between",
  "ss_within_total",
  "df_ms",
  "f_p",
  "missing_cell",
  "eta2",
  "full_decision",
  "critical_value",
  "posthoc"
), 2)

generate_pg <- function(i) {
  st <- anova_values(PG$weight, PG$group)
  style <- pick(names(pg_contexts))
  context <- pick(pg_contexts[[style]])
  layout <- sample(1:4, 1)
  task <- pg_tasks[i]

  gm <- st$means
  n <- st$sizes

  if (task == "means") {
    totals <- tapply(PG$weight, PG$group, sum)
    numbers <- paste0(
      "ctrl: n = ", n["ctrl"], ", total = ", fmt(totals["ctrl"]),
      "\ntrt1: n = ", n["trt1"], ", total = ", fmt(totals["trt1"]),
      "\ntrt2: n = ", n["trt2"], ", total = ", fmt(totals["trt2"])
    )
    task_text <- pick(c(
      "Calculate all three group means, identify the largest mean and find the range across the means.",
      "Recover the treatment averages and quantify the highest-minus-lowest mean difference.",
      "Complete the descriptive stage of the analysis before the ANOVA table is interpreted."
    ))
    gap <- max(gm) - min(gm)
    answer <- paste0(
      "ctrl = ", fmt(gm["ctrl"]),
      "; trt1 = ", fmt(gm["trt1"]),
      "; trt2 = ", fmt(gm["trt2"]),
      "; highest = ", names(which.max(gm)),
      "; range = ", fmt(gap)
    )
    steps <- paste0(
      "Means are total/n: ", fmt(gm["ctrl"]), ", ",
      fmt(gm["trt1"]), ", ", fmt(gm["trt2"]), ".\n",
      "Range = ", fmt(max(gm)), " - ", fmt(min(gm)),
      " = ", fmt(gap), "."
    )

  } else if (task == "ss_between") {
    numbers <- paste0(
      "Grand mean = ", fmt(st$grand),
      "\nctrl: n = ", n["ctrl"], ", mean = ", fmt(gm["ctrl"]),
      "\ntrt1: n = ", n["trt1"], ", mean = ", fmt(gm["trt1"]),
      "\ntrt2: n = ", n["trt2"], ", mean = ", fmt(gm["trt2"])
    )
    task_text <- pick(c(
      "Calculate the between-group sum of squares, showing the contribution from each treatment.",
      "Use sum n_j(mean_j - grand mean)^2 to obtain the treatment sum of squares.",
      "Reconstruct the variation explained by treatment."
    ))
    contrib <- n * (gm - st$grand)^2
    answer <- paste0("SS_between = ", fmt(st$ssb))
    steps <- paste0(
      "Contributions: ctrl = ", fmt(contrib["ctrl"]),
      ", trt1 = ", fmt(contrib["trt1"]),
      ", trt2 = ", fmt(contrib["trt2"]), ".\n",
      "SS_between = ", fmt(sum(contrib)), "."
    )

  } else if (task == "ss_within_total") {
    parts <- tapply(
      seq_len(nrow(PG)),
      PG$group,
      function(idx) sum((PG$weight[idx] - mean(PG$weight[idx]))^2)
    )
    numbers <- paste0(
      "Within-group squared-deviation totals:",
      "\nctrl = ", fmt(parts["ctrl"]),
      "\ntrt1 = ", fmt(parts["trt1"]),
      "\ntrt2 = ", fmt(parts["trt2"]),
      "\nSS_between = ", fmt(st$ssb)
    )
    task_text <- pick(c(
      "Calculate SS_within and then recover SS_total.",
      "Add the three residual contributions, then combine them with the treatment sum of squares.",
      "Complete the within-group and total variability entries."
    ))
    answer <- paste0(
      "SS_within = ", fmt(st$sse),
      "; SS_total = ", fmt(st$sst)
    )
    steps <- paste0(
      "SS_within = ", fmt(sum(parts)), ".\n",
      "SS_total = ", fmt(st$ssb), " + ",
      fmt(st$sse), " = ", fmt(st$sst), "."
    )

  } else if (task == "df_ms") {
    numbers <- paste0(
      "Number of groups k = 3",
      "\nTotal N = ", nrow(PG),
      "\nSS_between = ", fmt(st$ssb),
      "\nSS_within = ", fmt(st$sse)
    )
    task_text <- pick(c(
      "Calculate both degrees of freedom and both mean squares.",
      "Complete the df and mean-square columns of the ANOVA table.",
      "Find df_between, df_within, MS_between and MS_within."
    ))
    answer <- paste0(
      "df_between = ", st$dfb,
      "; df_within = ", st$dfe,
      "; MS_between = ", fmt(st$msb),
      "; MS_within = ", fmt(st$mse)
    )
    steps <- paste0(
      "df_between = 3 - 1 = ", st$dfb, ".\n",
      "df_within = ", nrow(PG), " - 3 = ", st$dfe, ".\n",
      "MS_between = ", fmt(st$ssb), "/", st$dfb,
      " = ", fmt(st$msb), ".\n",
      "MS_within = ", fmt(st$sse), "/", st$dfe,
      " = ", fmt(st$mse), "."
    )

  } else if (task == "f_p") {
    numbers <- paste0(
      "MS_between = ", fmt(st$msb),
      "\nMS_within = ", fmt(st$mse),
      "\ndf = (", st$dfb, ", ", st$dfe, ")"
    )
    task_text <- pick(c(
      "Calculate the F statistic and its upper-tail p-value.",
      "Use the mean-square ratio to recover F and the ANOVA p-value.",
      "Find F = MS_between/MS_within and evaluate it with the stated degrees of freedom."
    ))
    answer <- paste0(
      "F = ", fmt(st$fval),
      "; p ", p_text(st$pval)
    )
    steps <- paste0(
      "F = ", fmt(st$msb), "/", fmt(st$mse),
      " = ", fmt(st$fval), ".\n",
      "Upper-tail p ", p_text(st$pval), "."
    )

  } else if (task == "missing_cell") {
    hidden <- pick(c("ssb", "sse", "msb", "mse"))
    if (hidden == "ssb") {
      numbers <- paste0(
        "SS_total = ", fmt(st$sst),
        "\nSS_within = ", fmt(st$sse),
        "\nSS_between is missing."
      )
      task_text <- "Recover SS_between and then calculate eta-squared."
      answer <- paste0(
        "SS_between = ", fmt(st$ssb),
        "; eta_squared = ", fmt(st$eta2)
      )
      steps <- paste0(
        "SS_between = ", fmt(st$sst), " - ", fmt(st$sse),
        " = ", fmt(st$ssb), ".\n",
        "eta_squared = ", fmt(st$ssb), "/", fmt(st$sst),
        " = ", fmt(st$eta2), "."
      )
    } else if (hidden == "sse") {
      numbers <- paste0(
        "SS_total = ", fmt(st$sst),
        "\nSS_between = ", fmt(st$ssb),
        "\nSS_within is missing."
      )
      task_text <- "Recover SS_within and then calculate MS_within."
      answer <- paste0(
        "SS_within = ", fmt(st$sse),
        "; MS_within = ", fmt(st$mse)
      )
      steps <- paste0(
        "SS_within = ", fmt(st$sst), " - ", fmt(st$ssb),
        " = ", fmt(st$sse), ".\n",
        "MS_within = ", fmt(st$sse), "/", st$dfe,
        " = ", fmt(st$mse), "."
      )
    } else if (hidden == "msb") {
      numbers <- paste0(
        "SS_between = ", fmt(st$ssb),
        "\ndf_between = ", st$dfb,
        "\nMS_within = ", fmt(st$mse),
        "\nMS_between is missing."
      )
      task_text <- "Recover MS_between and then calculate F."
      answer <- paste0(
        "MS_between = ", fmt(st$msb),
        "; F = ", fmt(st$fval)
      )
      steps <- paste0(
        "MS_between = ", fmt(st$ssb), "/", st$dfb,
        " = ", fmt(st$msb), ".\n",
        "F = ", fmt(st$msb), "/", fmt(st$mse),
        " = ", fmt(st$fval), "."
      )
    } else {
      numbers <- paste0(
        "SS_within = ", fmt(st$sse),
        "\ndf_within = ", st$dfe,
        "\nMS_between = ", fmt(st$msb),
        "\nMS_within is missing."
      )
      task_text <- "Recover MS_within and then calculate F."
      answer <- paste0(
        "MS_within = ", fmt(st$mse),
        "; F = ", fmt(st$fval)
      )
      steps <- paste0(
        "MS_within = ", fmt(st$sse), "/", st$dfe,
        " = ", fmt(st$mse), ".\n",
        "F = ", fmt(st$msb), "/", fmt(st$mse),
        " = ", fmt(st$fval), "."
      )
    }

  } else if (task == "eta2") {
    numbers <- paste0(
      "SS_between = ", fmt(st$ssb),
      "\nSS_total = ", fmt(st$sst)
    )
    task_text <- pick(c(
      "Calculate eta-squared and express it as a percentage of total variation explained by treatment.",
      "Quantify the proportion of plant-weight variation associated with group membership.",
      "Find the ANOVA effect size from the sums of squares."
    ))
    answer <- paste0(
      "eta_squared = ", fmt(st$eta2),
      "; explained variation = ", fmt(st$eta2 * 100, 1), "%"
    )
    steps <- paste0(
      "eta_squared = ", fmt(st$ssb), "/", fmt(st$sst),
      " = ", fmt(st$eta2), ".\n",
      "Percentage = ", fmt(st$eta2 * 100, 1), "%."
    )

  } else if (task == "full_decision") {
    alpha <- pick(c(0.05, 0.01))
    numbers <- paste0(
      "SS_between = ", fmt(st$ssb),
      "\nSS_within = ", fmt(st$sse),
      "\ndf_between = ", st$dfb,
      "\ndf_within = ", st$dfe,
      "\nalpha = ", alpha
    )
    task_text <- pick(c(
      "Calculate both mean squares, F and p, then state the ANOVA decision.",
      "Complete the numerical ANOVA and decide whether the three population means are equal.",
      "Recover the omnibus evidence and make the stated alpha-level decision."
    ))
    decision <- ifelse(st$pval < alpha, "reject H0", "do not reject H0")
    answer <- paste0(
      "MS_between = ", fmt(st$msb),
      "; MS_within = ", fmt(st$mse),
      "; F = ", fmt(st$fval),
      "; p ", p_text(st$pval),
      "; ", decision
    )
    steps <- paste0(
      "MS_between = ", fmt(st$msb),
      "; MS_within = ", fmt(st$mse), ".\n",
      "F = ", fmt(st$fval), "; p ", p_text(st$pval), ".\n",
      "At alpha = ", alpha, ", ", decision, "."
    )

  } else if (task == "critical_value") {
    alpha <- pick(c(0.05, 0.01))
    fcrit <- qf(1 - alpha, st$dfb, st$dfe)
    numbers <- paste0(
      "Observed F = ", fmt(st$fval),
      "\ndf = (", st$dfb, ", ", st$dfe, ")",
      "\nalpha = ", alpha
    )
    task_text <- pick(c(
      "Calculate the upper-tail critical F value and make the rejection decision.",
      "Use the critical-value approach rather than the p-value approach.",
      "Find F_critical and compare it with the observed statistic."
    ))
    decision <- ifelse(st$fval > fcrit, "reject H0", "do not reject H0")
    answer <- paste0(
      "F_critical = ", fmt(fcrit),
      "; observed F = ", fmt(st$fval),
      "; ", decision
    )
    steps <- paste0(
      "F_critical = ", fmt(fcrit), ".\n",
      "Observed F = ", fmt(st$fval),
      ifelse(st$fval > fcrit, " > ", " <= "),
      fmt(fcrit), ", so ", decision, "."
    )

  } else {
    numbers <- paste0(
      "F(", st$dfb, ", ", st$dfe, ") = ", fmt(st$fval),
      "\np-value ", p_text(st$pval),
      "\nGroup means: ctrl = ", fmt(gm["ctrl"]),
      ", trt1 = ", fmt(gm["trt1"]),
      ", trt2 = ", fmt(gm["trt2"])
    )
    task_text <- pick(c(
      "Decide whether post-hoc comparisons are warranted at 5%, and calculate the largest observed gap between group means.",
      "Use the omnibus result to decide on follow-up comparisons, then identify the widest sample-mean separation.",
      "State the follow-up decision and quantify the largest difference among the three observed means."
    ))
    gap <- max(gm) - min(gm)
    warranted <- st$pval < 0.05
    answer <- paste0(
      "post_hoc_warranted = ", ifelse(warranted, "yes", "no"),
      "; largest mean gap = ", fmt(gap)
    )
    steps <- paste0(
      "Because p ", p_text(st$pval), ", post-hoc comparisons are ",
      ifelse(warranted, "warranted", "not warranted"), ".\n",
      "Largest gap = ", fmt(max(gm)), " - ", fmt(min(gm)),
      " = ", fmt(gap), "."
    )
  }

  question <- layout_question(context, numbers, task_text, layout)

  make_row(
    sprintf("R800_025_%03d", i),
    "R800_025",
    "PlantGrowth",
    "agriculture",
    style,
    paste0("anova_aov_template_", task),
    "multi_step_anova_calculation_and_evidence_interpretation",
    "weight, group",
    question,
    answer,
    steps
  )
}

# ============================================================
# R800_026: iris / Healthcare / Medium / Calculation
#
# Sepal.Length -> Biomarker A
# Petal.Length -> Biomarker B
# Species      -> Patient cohort
# ============================================================

health_contexts <- list(
  clinical_summary = c(
    "A clinical training exercise compares a continuous biomarker across three anonymised patient cohorts.",
    "Three patient cohorts are compared before any pairwise follow-up analysis is considered."
  ),
  hospital_report = c(
    "A hospital quality report contains three cohort summaries but omits part of the ANOVA table.",
    "The quality-improvement team wants to know whether between-cohort variation is large relative to residual variation."
  ),
  biomarker_lab = c(
    "A biomedical laboratory is validating a three-cohort comparison for a continuous marker.",
    "The lab team must reconstruct the ANOVA from cohort means, sizes and variation."
  ),
  conference_abstract = c(
    "A medical conference abstract lists cohort means but still needs the omnibus ANOVA evidence.",
    "A poster submission requires F, p and a measure of explained variation."
  ),
  diagnostic_study = c(
    "A diagnostic study compares one continuous measurement across three patient categories.",
    "The team is evaluating whether cohort membership is associated with different average biomarker levels."
  ),
  pathology_meeting = c(
    "Pathologist: \"The cohort means look different, but how strong is the formal evidence?\"\nBiostatistician: \"We need to compare between- and within-cohort mean squares.\"",
    "Lab director: \"Should we run pairwise comparisons?\"\nAnalyst: \"Only if the omnibus ANOVA supports doing so.\""
  ),
  research_audit = c(
    "A clinical research audit checks whether the published sums of squares and F statistic are internally consistent.",
    "The audit team hides one ANOVA entry and asks for it to be recovered."
  ),
  teaching_hospital = c(
    "A teaching hospital uses anonymised iris measurements for a medium-level ANOVA problem.",
    "Residents in a biostatistics workshop must complete a three-group numerical analysis."
  )
)

health_tasks <- rep(c(
  "means",
  "ss_between",
  "ss_within_total",
  "df_ms",
  "f_p",
  "missing_cell",
  "eta2",
  "full_decision",
  "critical_value",
  "posthoc"
), 2)

generate_health <- function(i) {
  trait <- pick(c("Sepal.Length", "Petal.Length"))
  label <- ifelse(trait == "Sepal.Length", "Biomarker A", "Biomarker B")
  st <- anova_values(IR[[trait]], IR$Species)

  style <- pick(names(health_contexts))
  context <- pick(health_contexts[[style]])
  layout <- sample(1:4, 1)
  task <- health_tasks[i]

  gm <- st$means
  n <- st$sizes
  y <- IR[[trait]]
  g <- IR$Species

  cohort_name <- c(
    setosa = "Cohort 1",
    versicolor = "Cohort 2",
    virginica = "Cohort 3"
  )

  if (task == "means") {
    totals <- tapply(y, g, sum)
    numbers <- paste0(
      label, " totals:",
      "\nCohort 1: n = ", n["setosa"], ", total = ", fmt(totals["setosa"]),
      "\nCohort 2: n = ", n["versicolor"], ", total = ", fmt(totals["versicolor"]),
      "\nCohort 3: n = ", n["virginica"], ", total = ", fmt(totals["virginica"])
    )
    task_text <- pick(c(
      "Calculate the three cohort means, identify the largest mean and find the range across the means.",
      "Recover the average biomarker level for each cohort and quantify the widest observed mean gap.",
      "Complete the descriptive comparison required before interpreting the ANOVA."
    ))
    gap <- max(gm) - min(gm)
    answer <- paste0(
      "Cohort 1 = ", fmt(gm["setosa"]),
      "; Cohort 2 = ", fmt(gm["versicolor"]),
      "; Cohort 3 = ", fmt(gm["virginica"]),
      "; highest = ", cohort_name[names(which.max(gm))],
      "; range = ", fmt(gap)
    )
    steps <- paste0(
      "Means are total/n: ", fmt(gm["setosa"]), ", ",
      fmt(gm["versicolor"]), ", ", fmt(gm["virginica"]), ".\n",
      "Range = ", fmt(max(gm)), " - ", fmt(min(gm)),
      " = ", fmt(gap), "."
    )

  } else if (task == "ss_between") {
    numbers <- paste0(
      label, " grand mean = ", fmt(st$grand),
      "\nCohort 1: n = ", n["setosa"], ", mean = ", fmt(gm["setosa"]),
      "\nCohort 2: n = ", n["versicolor"], ", mean = ", fmt(gm["versicolor"]),
      "\nCohort 3: n = ", n["virginica"], ", mean = ", fmt(gm["virginica"])
    )
    task_text <- pick(c(
      "Calculate the between-cohort sum of squares.",
      "Use sum n_j(mean_j - grand mean)^2 to obtain SS_between.",
      "Reconstruct the biomarker variation explained by cohort membership."
    ))
    contrib <- n * (gm - st$grand)^2
    answer <- paste0("SS_between = ", fmt(st$ssb))
    steps <- paste0(
      "Cohort contributions = ", fmt(contrib["setosa"]), ", ",
      fmt(contrib["versicolor"]), ", ",
      fmt(contrib["virginica"]), ".\n",
      "SS_between = ", fmt(sum(contrib)), "."
    )

  } else if (task == "ss_within_total") {
    parts <- tapply(
      seq_len(nrow(IR)),
      g,
      function(idx) sum((y[idx] - mean(y[idx]))^2)
    )
    numbers <- paste0(
      "Within-cohort squared-deviation totals for ", label, ":",
      "\nCohort 1 = ", fmt(parts["setosa"]),
      "\nCohort 2 = ", fmt(parts["versicolor"]),
      "\nCohort 3 = ", fmt(parts["virginica"]),
      "\nSS_between = ", fmt(st$ssb)
    )
    task_text <- pick(c(
      "Calculate SS_within and then SS_total.",
      "Add the three residual contributions and combine them with SS_between.",
      "Complete the residual and total variation entries."
    ))
    answer <- paste0(
      "SS_within = ", fmt(st$sse),
      "; SS_total = ", fmt(st$sst)
    )
    steps <- paste0(
      "SS_within = ", fmt(sum(parts)), ".\n",
      "SS_total = ", fmt(st$ssb), " + ",
      fmt(st$sse), " = ", fmt(st$sst), "."
    )

  } else if (task == "df_ms") {
    numbers <- paste0(
      "Number of cohorts k = 3",
      "\nTotal N = ", length(y),
      "\nSS_between = ", fmt(st$ssb),
      "\nSS_within = ", fmt(st$sse)
    )
    task_text <- pick(c(
      "Calculate both degrees of freedom and both mean squares.",
      "Complete the df and MS columns of the ANOVA table.",
      "Find df_between, df_within, MS_between and MS_within."
    ))
    answer <- paste0(
      "df_between = ", st$dfb,
      "; df_within = ", st$dfe,
      "; MS_between = ", fmt(st$msb),
      "; MS_within = ", fmt(st$mse)
    )
    steps <- paste0(
      "df_between = 3 - 1 = ", st$dfb, ".\n",
      "df_within = ", length(y), " - 3 = ", st$dfe, ".\n",
      "MS_between = ", fmt(st$ssb), "/", st$dfb,
      " = ", fmt(st$msb), ".\n",
      "MS_within = ", fmt(st$sse), "/", st$dfe,
      " = ", fmt(st$mse), "."
    )

  } else if (task == "f_p") {
    numbers <- paste0(
      "MS_between = ", fmt(st$msb),
      "\nMS_within = ", fmt(st$mse),
      "\ndf = (", st$dfb, ", ", st$dfe, ")"
    )
    task_text <- pick(c(
      "Calculate F and the upper-tail p-value.",
      "Use the mean-square ratio to obtain the omnibus evidence.",
      "Recover the test statistic and p-value from the two mean squares."
    ))
    answer <- paste0(
      "F = ", fmt(st$fval),
      "; p ", p_text(st$pval)
    )
    steps <- paste0(
      "F = ", fmt(st$msb), "/", fmt(st$mse),
      " = ", fmt(st$fval), ".\n",
      "p ", p_text(st$pval), "."
    )

  } else if (task == "missing_cell") {
    hidden <- pick(c("ssb", "sse", "msb", "mse"))
    if (hidden == "ssb") {
      numbers <- paste0(
        "SS_total = ", fmt(st$sst),
        "\nSS_within = ", fmt(st$sse),
        "\nSS_between is missing."
      )
      task_text <- "Recover SS_between and then calculate eta-squared."
      answer <- paste0(
        "SS_between = ", fmt(st$ssb),
        "; eta_squared = ", fmt(st$eta2)
      )
      steps <- paste0(
        "SS_between = ", fmt(st$sst), " - ", fmt(st$sse),
        " = ", fmt(st$ssb), ".\n",
        "eta_squared = ", fmt(st$eta2), "."
      )
    } else if (hidden == "sse") {
      numbers <- paste0(
        "SS_total = ", fmt(st$sst),
        "\nSS_between = ", fmt(st$ssb),
        "\nSS_within is missing."
      )
      task_text <- "Recover SS_within and then calculate MS_within."
      answer <- paste0(
        "SS_within = ", fmt(st$sse),
        "; MS_within = ", fmt(st$mse)
      )
      steps <- paste0(
        "SS_within = ", fmt(st$sst), " - ", fmt(st$ssb),
        " = ", fmt(st$sse), ".\n",
        "MS_within = ", fmt(st$mse), "."
      )
    } else if (hidden == "msb") {
      numbers <- paste0(
        "SS_between = ", fmt(st$ssb),
        "\ndf_between = ", st$dfb,
        "\nMS_within = ", fmt(st$mse),
        "\nMS_between is missing."
      )
      task_text <- "Recover MS_between and then calculate F."
      answer <- paste0(
        "MS_between = ", fmt(st$msb),
        "; F = ", fmt(st$fval)
      )
      steps <- paste0(
        "MS_between = ", fmt(st$ssb), "/", st$dfb,
        " = ", fmt(st$msb), ".\n",
        "F = ", fmt(st$fval), "."
      )
    } else {
      numbers <- paste0(
        "SS_within = ", fmt(st$sse),
        "\ndf_within = ", st$dfe,
        "\nMS_between = ", fmt(st$msb),
        "\nMS_within is missing."
      )
      task_text <- "Recover MS_within and then calculate F."
      answer <- paste0(
        "MS_within = ", fmt(st$mse),
        "; F = ", fmt(st$fval)
      )
      steps <- paste0(
        "MS_within = ", fmt(st$sse), "/", st$dfe,
        " = ", fmt(st$mse), ".\n",
        "F = ", fmt(st$fval), "."
      )
    }

  } else if (task == "eta2") {
    numbers <- paste0(
      "SS_between = ", fmt(st$ssb),
      "\nSS_total = ", fmt(st$sst)
    )
    task_text <- pick(c(
      "Calculate eta-squared and express it as a percentage of total biomarker variation explained by cohort.",
      "Find the proportion of variation attributable to cohort membership.",
      "Compute the ANOVA effect size."
    ))
    answer <- paste0(
      "eta_squared = ", fmt(st$eta2),
      "; explained variation = ", fmt(st$eta2 * 100, 1), "%"
    )
    steps <- paste0(
      "eta_squared = ", fmt(st$ssb), "/", fmt(st$sst),
      " = ", fmt(st$eta2), ".\n",
      "Percentage = ", fmt(st$eta2 * 100, 1), "%."
    )

  } else if (task == "full_decision") {
    alpha <- pick(c(0.05, 0.01))
    numbers <- paste0(
      "SS_between = ", fmt(st$ssb),
      "\nSS_within = ", fmt(st$sse),
      "\ndf_between = ", st$dfb,
      "\ndf_within = ", st$dfe,
      "\nalpha = ", alpha
    )
    task_text <- pick(c(
      "Calculate both mean squares, F and p, then state the conclusion for the three cohort means.",
      "Complete the full one-way ANOVA and decision.",
      "Recover the omnibus evidence from the supplied sums of squares."
    ))
    decision <- ifelse(st$pval < alpha, "reject H0", "do not reject H0")
    answer <- paste0(
      "MS_between = ", fmt(st$msb),
      "; MS_within = ", fmt(st$mse),
      "; F = ", fmt(st$fval),
      "; p ", p_text(st$pval),
      "; ", decision
    )
    steps <- paste0(
      "MS_between = ", fmt(st$msb),
      "; MS_within = ", fmt(st$mse), ".\n",
      "F = ", fmt(st$fval), "; p ", p_text(st$pval), ".\n",
      "At alpha = ", alpha, ", ", decision, "."
    )

  } else if (task == "critical_value") {
    alpha <- pick(c(0.05, 0.01))
    fcrit <- qf(1 - alpha, st$dfb, st$dfe)
    numbers <- paste0(
      label, " ANOVA",
      "\nObserved F = ", fmt(st$fval),
      "\ndf = (", st$dfb, ", ", st$dfe, ")",
      "\nalpha = ", alpha
    )
    task_text <- pick(c(
      "Calculate F_critical and make the rejection decision.",
      "Use the critical-value approach rather than the p-value.",
      "Find the upper-tail cutoff and compare it with observed F."
    ))
    decision <- ifelse(st$fval > fcrit, "reject H0", "do not reject H0")
    answer <- paste0(
      "F_critical = ", fmt(fcrit),
      "; ", decision
    )
    steps <- paste0(
      "F_critical = ", fmt(fcrit), ".\n",
      "Observed F = ", fmt(st$fval),
      ifelse(st$fval > fcrit, " > ", " <= "),
      fmt(fcrit), ", so ", decision, "."
    )

  } else {
    numbers <- paste0(
      label, " omnibus ANOVA:",
      "\nF(", st$dfb, ", ", st$dfe, ") = ", fmt(st$fval),
      "\np-value ", p_text(st$pval),
      "\nCohort means = ", fmt(gm["setosa"]), ", ",
      fmt(gm["versicolor"]), ", ", fmt(gm["virginica"])
    )
    task_text <- pick(c(
      "Decide whether post-hoc comparisons are warranted at 5%, and calculate the largest observed difference between cohort means.",
      "Use the omnibus result to determine whether follow-up comparisons should be considered, then find the widest cohort-mean gap.",
      "State the follow-up decision and quantify the largest separation among the three cohort averages."
    ))
    gap <- max(gm) - min(gm)
    warranted <- st$pval < 0.05
    answer <- paste0(
      "post_hoc_warranted = ", ifelse(warranted, "yes", "no"),
      "; largest mean gap = ", fmt(gap)
    )
    steps <- paste0(
      "Because p ", p_text(st$pval), ", post-hoc comparisons are ",
      ifelse(warranted, "warranted", "not warranted"), ".\n",
      "Largest gap = ", fmt(max(gm)), " - ", fmt(min(gm)),
      " = ", fmt(gap), "."
    )
  }

  question <- layout_question(context, numbers, task_text, layout)

  make_row(
    sprintf("R800_026_%03d", i),
    "R800_026",
    "iris",
    "healthcare",
    style,
    paste0("anova_aov_template_", task),
    "multi_step_anova_calculation_and_clinical_evidence_interpretation",
    "Sepal.Length, Petal.Length, Species",
    question,
    answer,
    steps
  )
}

# ============================================================
# Generate, validate and export
# ============================================================

R800_025 <- do.call(rbind, lapply(seq_len(20), generate_pg))
R800_026 <- do.call(rbind, lapply(seq_len(20), generate_health))
ALL <- rbind(R800_025, R800_026)

stopifnot(nrow(R800_025) == 20)
stopifnot(nrow(R800_026) == 20)
stopifnot(nrow(ALL) == 40)
stopifnot(length(unique(ALL$id)) == 40)
stopifnot(!any(is.na(ALL$question)))
stopifnot(!any(is.na(ALL$reference_answer)))
stopifnot(!any(is.na(ALL$solution_steps)))
stopifnot(!any(ALL$question == ""))
stopifnot(!any(ALL$reference_answer == ""))

write.csv(
  ALL,
  "R800_025_026_questions.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

jsonlite::write_json(
  ALL,
  "R800_025_026_questions.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

print(
  ALL[, c(
    "id",
    "blueprint_id",
    "dataset_name",
    "scenario",
    "language_style",
    "template_id",
    "reference_answer"
  )]
)

cat(
  "\nGenerated successfully:\n",
  "- R800_025: 20 PlantGrowth Agriculture ANOVA questions\n",
  "- R800_026: 20 iris Healthcare ANOVA questions\n",
  "- CSV: R800_025_026_questions.csv\n",
  "- JSON: R800_025_026_questions.json\n"
)
