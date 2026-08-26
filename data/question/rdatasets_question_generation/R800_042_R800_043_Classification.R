# ============================================================
# Classification Question Generator
#
# R800_042
# Dataset: iris
# Domain: Education
# Difficulty: Easy
# Question type: Single Choice
# Count: 10
#
# R800_043
# Dataset: iris
# Domain: Marketing
# Difficulty: Medium
# Question type: Multiple Choice
# Count: 10
#
# Outputs:
# 1. R800_042_R800_043_Classification_v2.csv
# 2. R800_042_R800_043_Classification_v2.json
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
  formatC(x, format = "f", digits = digits)
}

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

format_options <- function(options) {
  paste(
    paste0(LETTERS[seq_along(options)], ". ", options),
    collapse = "\n"
  )
}

format_answer_letters <- function(indices) {
  paste(LETTERS[indices], collapse = ", ")
}

format_named_values <- function(x, digits = 3) {
  paste(
    paste0(names(x), " = ", fmt_num(as.numeric(x), digits)),
    collapse = ", "
  )
}

# ------------------------------------------------------------
# Train-test split
#
# First 40 observations from each species are training data.
# Last 10 observations from each species are test data.
# ------------------------------------------------------------

species_levels <- levels(iris$Species)

train_indices <- unlist(
  lapply(
    species_levels,
    function(sp) {
      which(iris$Species == sp)[1:40]
    }
  )
)

test_indices <- setdiff(seq_len(nrow(iris)), train_indices)

train_data <- iris[train_indices, ]
test_data <- iris[test_indices, ]

truth_test <- as.character(test_data$Species)

# ------------------------------------------------------------
# Classification helpers
# ------------------------------------------------------------

calculate_centroids <- function(data, features) {
  aggregate(
    data[, features, drop = FALSE],
    by = list(Species = data$Species),
    FUN = mean
  )
}

euclidean_distance <- function(observation, centroid, features) {
  sqrt(
    sum(
      (
        as.numeric(observation[features]) -
          as.numeric(centroid[features])
      )^2
    )
  )
}

nearest_centroid_predict_one <- function(observation, centroids, features) {

  distances <- sapply(
    seq_len(nrow(centroids)),
    function(i) {
      euclidean_distance(
        observation = observation,
        centroid = centroids[i, ],
        features = features
      )
    }
  )

  names(distances) <- as.character(centroids$Species)

  list(
    predicted_class = names(which.min(distances)),
    distances = distances
  )
}

nearest_centroid_predict <- function(data, centroids, features) {
  sapply(
    seq_len(nrow(data)),
    function(i) {
      nearest_centroid_predict_one(
        observation = data[i, ],
        centroids = centroids,
        features = features
      )$predicted_class
    }
  )
}

classification_metrics <- function(truth, prediction, positive_class) {

  truth_positive <- truth == positive_class
  pred_positive <- prediction == positive_class

  tp <- sum(truth_positive & pred_positive)
  tn <- sum(!truth_positive & !pred_positive)
  fp <- sum(!truth_positive & pred_positive)
  fn <- sum(truth_positive & !pred_positive)

  sensitivity <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
  specificity <- ifelse(tn + fp == 0, NA_real_, tn / (tn + fp))
  precision <- ifelse(tp + fp == 0, NA_real_, tp / (tp + fp))

  list(
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision
  )
}

# ------------------------------------------------------------
# Models and metrics
# ------------------------------------------------------------

two_features <- c("Petal.Length", "Petal.Width")
four_features <- c(
  "Sepal.Length",
  "Sepal.Width",
  "Petal.Length",
  "Petal.Width"
)

centroids_2d <- calculate_centroids(train_data, two_features)
centroids_4d <- calculate_centroids(train_data, four_features)

pred_test_2d <- nearest_centroid_predict(
  test_data,
  centroids_2d,
  two_features
)

pred_test_4d <- nearest_centroid_predict(
  test_data,
  centroids_4d,
  four_features
)

confusion_2d <- table(
  Actual = truth_test,
  Predicted = pred_test_2d
)

confusion_4d <- table(
  Actual = truth_test,
  Predicted = pred_test_4d
)

accuracy_2d <- mean(pred_test_2d == truth_test)
accuracy_4d <- mean(pred_test_4d == truth_test)

versicolor_metrics <- classification_metrics(
  truth_test,
  pred_test_4d,
  "versicolor"
)

virginica_metrics <- classification_metrics(
  truth_test,
  pred_test_4d,
  "virginica"
)

# ------------------------------------------------------------
# Simple transparent threshold rule
# ------------------------------------------------------------

threshold_predict <- function(petal_length, petal_width) {

  if (petal_length < 2.5) {
    "setosa"
  } else if (petal_width < 1.75) {
    "versicolor"
  } else {
    "virginica"
  }
}

threshold_predictions <- mapply(
  threshold_predict,
  test_data$Petal.Length,
  test_data$Petal.Width
)

threshold_accuracy <- mean(threshold_predictions == truth_test)

# Selected observations used in questions
obs_easy <- test_data[1, ]
obs_medium <- test_data[16, ]

pred_easy <- nearest_centroid_predict_one(
  obs_easy,
  centroids_2d,
  two_features
)

pred_medium <- nearest_centroid_predict_one(
  obs_medium,
  centroids_4d,
  four_features
)

# ============================================================
# R800_042
# Education + Easy + Single Choice
# ============================================================

education_scenarios <- c(

  paste(
    "During a classroom demonstration, the iris measurements are used",
    "to explain how a simple rule can assign an observation to one of",
    "three known groups."
  ),

  paste(
    "On a revision sheet, Species is the target to be predicted and the",
    "four measurements are available as inputs."
  ),

  paste(
    "A scatterplot exercise shows one observation lying closest to the",
    "setosa centroid when only petal measurements are used."
  ),

  paste(
    "Instead of asking for a full model, a quiz presents the rule:",
    "Petal.Length below 2.5 gives setosa; otherwise Petal.Width below",
    "1.75 gives versicolor; all remaining cases are virginica."
  ),

  paste(
    "A lesson on evaluation displays a confusion matrix and asks students",
    "to identify which entries count as correct classifications."
  ),

  paste(
    "For a simple test-set summary, 27 of 30 observations are classified",
    "correctly."
  ),

  paste(
    "A student claims that a nearest-centroid classifier always chooses",
    "the class with the largest feature values."
  ),

  paste(
    "While comparing two classifiers, the instructor asks what accuracy",
    "measures."
  ),

  paste(
    "One benchmark case is assigned to virginica by the threshold rule.",
    "The class label must be compared with the recorded Species."
  ),

  paste(
    "At the end of the topic, students must choose the most suitable",
    "description of a classification task."
  )
)

education_styles <- c(
  "classroom-demo",
  "variable-role",
  "distance-intuition",
  "rule-application",
  "confusion-matrix",
  "accuracy",
  "misconception-check",
  "evaluation",
  "decision-check",
  "concept-summary"
)

education_tasks <- c(
  "identify_classifier_goal",
  "identify_response",
  "nearest_centroid_logic",
  "apply_threshold",
  "identify_correct_cells",
  "calculate_accuracy",
  "correct_distance_rule",
  "interpret_accuracy",
  "check_prediction",
  "classification_definition"
)

build_education_question <- function(i) {

  task_name <- education_tasks[i]

  if (task_name == "identify_classifier_goal") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "What is the classifier trying to predict?"
    )

    options <- c(
      "Species",
      "Petal.Length",
      "The row number",
      "The sample mean"
    )

    correct <- 1
    explanation <- "Species is the categorical outcome being assigned."

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"

  } else if (task_name == "identify_response") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which variable is the response in this classification problem?"
    )

    options <- c(
      "Species",
      "Sepal.Length",
      "Petal.Width",
      "Distance"
    )

    correct <- 1
    explanation <- "Species is the class label, while the measurements are predictors."

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"

  } else if (task_name == "nearest_centroid_logic") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which rule is being used?"
    )

    options <- c(
      "Assign the observation to the class with the smallest distance",
      "Assign the observation to the class with the largest sample size",
      "Assign the observation to the alphabetically first class",
      "Assign the observation to every class"
    )

    correct <- 1
    explanation <- "Nearest-centroid classification selects the smallest Euclidean distance."

    predictor_value <- "Petal.Length, Petal.Width"

  } else if (task_name == "apply_threshold") {

    petal_length <- 4.7
    petal_width <- 1.4

    predicted <- threshold_predict(
      petal_length,
      petal_width
    )

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "For Petal.Length = ",
      fmt_num(petal_length),
      " and Petal.Width = ",
      fmt_num(petal_width),
      ", which class is predicted?"
    )

    options <- c(
      predicted,
      "setosa",
      "virginica",
      "No class can be assigned"
    )

    options <- unique(options)

    while (length(options) < 4) {
      options <- c(options, paste0("Other class ", length(options) + 1))
    }

    options <- options[1:4]
    correct <- which(options == predicted)[1]
    explanation <- paste0(
      "Petal.Length is not below 2.5, but Petal.Width is below 1.75, so the rule predicts ",
      predicted,
      "."
    )

    predictor_value <- "Petal.Length, Petal.Width"

  } else if (task_name == "identify_correct_cells") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "In a confusion matrix, where are correct classifications found?"
    )

    options <- c(
      "On the main diagonal",
      "Only in the first row",
      "Only in the last column",
      "Outside the table"
    )

    correct <- 1
    explanation <- "Correct predictions occur where Actual and Predicted match."

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"

  } else if (task_name == "calculate_accuracy") {

    correct_count <- 27
    total_count <- 30
    accuracy <- correct_count / total_count

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "What is the classification accuracy?"
    )

    options <- c(
      fmt_pct(accuracy),
      "27.0%",
      "3.0%",
      "100.0%"
    )

    correct <- 1
    explanation <- paste0(
      "Accuracy = 27 / 30 = ",
      fmt_num(accuracy),
      " = ",
      fmt_pct(accuracy),
      "."
    )

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"

  } else if (task_name == "correct_distance_rule") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which correction is accurate?"
    )

    options <- c(
      "The classifier chooses the nearest centroid, not the class with the largest values",
      "The classifier always chooses virginica",
      "The classifier ignores all measurements",
      "The classifier selects the class with the longest name"
    )

    correct <- 1
    explanation <- "The decision depends on distance to each class centre."

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"

  } else if (task_name == "interpret_accuracy") {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which statement best defines accuracy?"
    )

    options <- c(
      "The proportion of all test observations classified correctly",
      "The distance from one case to a centroid",
      "The number of predictor variables",
      "The probability that every future case is correct"
    )

    correct <- 1
    explanation <- "Accuracy is a sample-level proportion of correct test predictions."

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"

  } else if (task_name == "check_prediction") {

    actual_class <- as.character(obs_easy$Species)
    predicted_class <- threshold_predict(
      obs_easy$Petal.Length,
      obs_easy$Petal.Width
    )

    decision <- ifelse(
      predicted_class == actual_class,
      "correct",
      "incorrect"
    )

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "The rule predicts ",
      predicted_class,
      " and the recorded Species is ",
      actual_class,
      ". How should the decision be described?"
    )

    options <- c(
      paste0("The classification is ", decision),
      "The accuracy is automatically 100%",
      "The observation belongs to all three classes",
      "The result cannot be checked"
    )

    correct <- 1
    explanation <- "A prediction is checked by comparing it with the recorded class."

    predictor_value <- "Petal.Length, Petal.Width"

  } else {

    stem <- paste0(
      education_scenarios[i],
      "\n\n",
      "Which statement best describes classification?"
    )

    options <- c(
      "Using measured features to assign an observation to a categorical class",
      "Calculating the mean of one variable",
      "Estimating a continuous response only",
      "Sorting rows alphabetically"
    )

    correct <- 1
    explanation <- "Classification predicts a categorical label from observed features."

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"
  }

  question <- paste0(
    stem,
    "\n\n",
    format_options(options)
  )

  reference_answer <- paste0(
    LETTERS[correct],
    ". ",
    options[correct]
  )

  solution_steps <- paste0(
    "1. Identify whether the item concerns the target class, the classification rule or model evaluation. ",
    "2. Eliminate answers that confuse classification with estimation of a continuous value. ",
    "3. Select option ",
    LETTERS[correct],
    ". ",
    explanation
  )

  data.frame(
    id = sprintf("R800_042_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_042",
    dataset_name = "iris",
    statistical_concept = "Classification",
    task = "classification_method_selection",
    template_id = paste0("classification_single_choice_", task_name),
    difficulty = "easy",
    scenario = "education",
    language_style = education_styles[i],
    question_type = "single_choice",
    predictor = predictor_value,
    response = "Species",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "single_choice",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# R800_043
# Marketing + Medium + Multiple Choice
# ============================================================

marketing_scenarios <- c(

  paste(
    "Before a customer-segmentation workshop begins, iris is used as a",
    "non-commercial benchmark. The four measurements stand in for product",
    "attributes, and Species represents three target segments."
  ),

  paste(
    "Inside a campaign-planning deck, one profile is assigned to the nearest",
    "segment centre using all four features. The classification output is",
    "reviewed before any audience label is attached."
  ),

  paste(
    "Rather than hiding the rule inside a black box, the team considers a",
    "two-step petal threshold because it can be explained clearly to clients."
  ),

  paste(
    "A hold-out evaluation shows high overall accuracy, but the account team",
    "wants to know which claims can actually be made from that number."
  ),

  paste(
    "With versicolor treated as the target segment, the campaign team examines",
    "both missed targets and false positive flags."
  ),

  paste(
    "Two candidate segmentation rules are compared on the same test set:",
    "one uses only petal features, while the other uses all four measurements."
  ),

  paste(
    "One profile lies much closer to virginica than to the other two centroids.",
    "The team discusses whether distance should be interpreted as confidence."
  ),

  paste(
    "A presentation states that adding more predictors must always improve",
    "classification. The four-feature and two-feature rules provide a chance",
    "to evaluate that claim."
  ),

  paste(
    "For a premium-segment campaign, false positives are expensive because",
    "they waste media spend. Precision therefore matters alongside accuracy."
  ),

  paste(
    "To close the segmentation review, the team must separate what the iris",
    "benchmark shows from what would still need validation in a real market."
  )
)

marketing_styles <- c(
  "segmentation-benchmark",
  "profile-assignment",
  "transparent-rule",
  "holdout-evaluation",
  "target-segment",
  "model-comparison",
  "distance-interpretation",
  "feature-count-myth",
  "campaign-efficiency",
  "validation-summary"
)

marketing_tasks <- c(
  "variable_roles",
  "nearest_centroid_claims",
  "threshold_rule_claims",
  "accuracy_claims",
  "versicolor_metrics",
  "compare_models",
  "distance_and_confidence",
  "more_features_claim",
  "precision_and_cost",
  "balanced_marketing_conclusion"
)

build_marketing_question <- function(i) {

  task_name <- marketing_tasks[i]

  if (task_name == "variable_roles") {

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "Which statements correctly describe the benchmark setup? Select all that apply."
    )

    options <- c(
      "Species is the categorical response",
      "The four measurements are predictors",
      "The task is to assign each observation to one class",
      "Accuracy is calculated from continuous prediction errors only",
      "The benchmark itself does not prove readiness for real customer data"
    )

    correct <- c(1, 2, 3, 5)

  } else if (task_name == "nearest_centroid_claims") {

    distances <- pred_medium$distances

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "The distances are ",
      format_named_values(distances),
      ", and the rule predicts ",
      pred_medium$predicted_class,
      ". Which statements are valid? Select all that apply."
    )

    options <- c(
      "The predicted segment is the one with the smallest distance",
      "The decision is based on similarity to class centres",
      "The largest distance determines the class",
      "The distances are not automatically calibrated probabilities",
      "A single profile cannot establish overall model quality"
    )

    correct <- c(1, 2, 4, 5)

  } else if (task_name == "threshold_rule_claims") {

    example_length <- 5.2
    example_width <- 1.6
    predicted <- threshold_predict(example_length, example_width)

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "For Petal.Length = ",
      fmt_num(example_length),
      " and Petal.Width = ",
      fmt_num(example_width),
      ", the rule predicts ",
      predicted,
      ". Which statements are correct? Select all that apply."
    )

    options <- c(
      "The case is not setosa because Petal.Length is not below 2.5",
      "The case is versicolor because Petal.Width is below 1.75",
      "The rule is easy to explain",
      "The rule guarantees perfect classification",
      "Performance should still be checked on held-out data"
    )

    correct <- c(1, 2, 3, 5)

  } else if (task_name == "accuracy_claims") {

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "The four-feature test accuracy is ",
      fmt_pct(accuracy_4d),
      ". Which interpretations are justified? Select all that apply."
    )

    options <- c(
      "It is the proportion correctly classified in this test set",
      "It summarises average hold-out performance",
      "It guarantees the same result in every future market",
      "It should be considered with class-specific metrics",
      "It may change under a different train-test split"
    )

    correct <- c(1, 2, 4, 5)

  } else if (task_name == "versicolor_metrics") {

    m <- versicolor_metrics

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "For versicolor, sensitivity = ",
      fmt_num(m$sensitivity),
      " and precision = ",
      fmt_num(m$precision),
      ". Which statements are correct? Select all that apply."
    )

    options <- c(
      "Sensitivity concerns actual versicolor cases that are detected",
      "Precision concerns predicted versicolor cases that are correct",
      "False negatives reduce sensitivity",
      "False positives reduce precision",
      "Sensitivity and precision are always identical"
    )

    correct <- c(1, 2, 3, 4)

  } else if (task_name == "compare_models") {

    improvement <- accuracy_4d - accuracy_2d

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "Petal-only accuracy is ",
      fmt_pct(accuracy_2d),
      " and four-feature accuracy is ",
      fmt_pct(accuracy_4d),
      ". Which conclusions are reasonable? Select all that apply."
    )

    options <- c(
      paste0(
        "The accuracy difference is ",
        fmt_num(100 * improvement, 1),
        " percentage points"
      ),
      "Both methods should be compared on the same test cases",
      "A single split is not enough to prove universal superiority",
      "The model with more features must always be better",
      "Class-specific errors may matter even when overall accuracy is similar"
    )

    correct <- c(1, 2, 3, 5)

  } else if (task_name == "distance_and_confidence") {

    ordered <- sort(pred_medium$distances)
    margin <- ordered[2] - ordered[1]

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "The nearest distance is ",
      fmt_num(ordered[1]),
      " and the second-nearest is ",
      fmt_num(ordered[2]),
      ", giving a margin of ",
      fmt_num(margin),
      ". Which statements are valid? Select all that apply."
    )

    options <- c(
      "A larger margin generally indicates clearer geometric separation",
      "The nearest class is still chosen",
      "The margin is not automatically a probability",
      "A small margin may signal a borderline profile",
      "Distance alone proves the segment is commercially meaningful"
    )

    correct <- c(1, 2, 3, 4)

  } else if (task_name == "more_features_claim") {

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "Which statements correctly evaluate the claim? Select all that apply."
    )

    options <- c(
      "Additional features can help if they contain useful information",
      "Additional features can also add noise or redundancy",
      "Performance must be checked empirically",
      "More predictors guarantee higher accuracy",
      "Feature relevance matters more than feature count alone"
    )

    correct <- c(1, 2, 3, 5)

  } else if (task_name == "precision_and_cost") {

    m <- virginica_metrics

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "For virginica, precision = ",
      fmt_num(m$precision),
      ". Which statements are correct? Select all that apply."
    )

    options <- c(
      "Precision measures the reliability of positive segment flags",
      "False positives reduce precision",
      "Low precision can waste campaign spend",
      "Precision is the same as the proportion of all actual targets detected",
      "Accuracy alone may hide poor positive-flag quality"
    )

    correct <- c(1, 2, 3, 5)

  } else {

    stem <- paste0(
      marketing_scenarios[i],
      "\n\n",
      "Which statements belong in a balanced conclusion? Select all that apply."
    )

    options <- c(
      "The benchmark can demonstrate how a classification rule works",
      "Hold-out accuracy and class-specific metrics should both be reviewed",
      "Results from iris automatically generalise to customer segments",
      "Distance-based labels require domain validation before business use",
      "Real deployment should consider data quality, class balance and decision costs"
    )

    correct <- c(1, 2, 4, 5)
  }

  question <- paste0(
    stem,
    "\n\n",
    format_options(options)
  )

  reference_answer <- paste0(
    format_answer_letters(correct),
    ". Correct statements: ",
    paste(options[correct], collapse = " | ")
  )

  incorrect <- setdiff(seq_along(options), correct)

  solution_steps <- paste0(
    "1. Evaluate each statement independently. ",
    "2. Keep the distinction between class assignment, model evaluation and business interpretation clear. ",
    "3. Reject claims that treat distance as probability, accuracy as a guarantee or benchmark results as automatic real-world validation. ",
    "4. Select ",
    format_answer_letters(correct),
    ". Options ",
    format_answer_letters(incorrect),
    " are unsupported or overstated."
  )

  data.frame(
    id = sprintf("R800_043_%03d", i),
    source = "R-generated",
    blueprint_id = "R800_043",
    dataset_name = "iris",
    statistical_concept = "Classification",
    task = "classification_method_selection",
    template_id = paste0("classification_multiple_choice_", task_name),
    difficulty = "medium",
    scenario = "marketing",
    language_style = marketing_styles[i],
    question_type = "multiple_choice",
    predictor = "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width",
    response = "Species",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "multiple_choice",
    version = "v2.0",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Generate all questions
# ------------------------------------------------------------

education_questions <- do.call(
  rbind,
  lapply(seq_len(10), build_education_question)
)

marketing_questions <- do.call(
  rbind,
  lapply(seq_len(10), build_marketing_question)
)

classification_questions <- rbind(
  education_questions,
  marketing_questions
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

classification_questions <- classification_questions[, required_columns]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(names(classification_questions), required_columns),
  nrow(classification_questions) == 20,
  length(unique(classification_questions$id)) == 20,
  !anyDuplicated(classification_questions$question),
  sum(classification_questions$blueprint_id == "R800_042") == 10,
  sum(classification_questions$blueprint_id == "R800_043") == 10,
  all(education_questions$difficulty == "easy"),
  all(education_questions$question_type == "single_choice"),
  all(marketing_questions$difficulty == "medium"),
  all(marketing_questions$question_type == "multiple_choice"),
  all(nchar(classification_questions$question) >= 80),
  all(nchar(classification_questions$solution_steps) >= 40)
)

multiple_answer_counts <- sapply(
  strsplit(
    marketing_questions$reference_answer,
    "\\. Correct statements:"
  ),
  function(x) {
    length(strsplit(x[1], ", ")[[1]])
  }
)

stopifnot(
  all(multiple_answer_counts >= 2)
)

# ------------------------------------------------------------
# Preview
# ------------------------------------------------------------

cat("\nQuestion count by blueprint:\n")
print(table(classification_questions$blueprint_id))

cat("\nQuestion count by question type:\n")
print(table(classification_questions$question_type))

cat("\nQuestion count by difficulty:\n")
print(table(classification_questions$difficulty))

preview_columns <- c(
  "id",
  "dataset_name",
  "difficulty",
  "scenario",
  "language_style",
  "question_type",
  "predictor",
  "response",
  "template_id",
  "reference_answer"
)

print(
  classification_questions[, preview_columns],
  row.names = FALSE
)

cat("\n\n================ R800_042 example ================\n\n")
cat(
  education_questions$question[1],
  "\n\nReference answer:\n",
  education_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  education_questions$solution_steps[1],
  "\n"
)

cat("\n\n================ R800_043 example ================\n\n")
cat(
  marketing_questions$question[1],
  "\n\nReference answer:\n",
  marketing_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  marketing_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_042_R800_043_Classification_v2.csv"
json_file <- "R800_042_R800_043_Classification_v2.json"

write.csv(
  classification_questions,
  file = csv_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

write_json(
  classification_questions,
  path = json_file,
  dataframe = "rows",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat(
  "\n\nSuccessfully generated ",
  nrow(classification_questions),
  " classification questions.\n",
  sep = ""
)

cat(
  "R800_042 education single-choice questions: ",
  nrow(education_questions),
  "\n",
  sep = ""
)

cat(
  "R800_043 marketing multiple-choice questions: ",
  nrow(marketing_questions),
  "\n",
  sep = ""
)

cat(
  "CSV file: ",
  normalizePath(csv_file, mustWork = FALSE),
  "\n",
  sep = ""
)

cat(
  "JSON file: ",
  normalizePath(json_file, mustWork = FALSE),
  "\n",
  sep = ""
)
