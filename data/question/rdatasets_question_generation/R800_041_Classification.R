# ============================================================
# Classification Question Generator
#
# R800_041
# Dataset: iris
# Domain: Healthcare
# Difficulty: Medium
# Question type: Calculation
# Count: 10
#
# Outputs:
# 1. R800_041_Classification_v2.csv
# 2. R800_041_Classification_v2.json
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

format_vector <- function(x, digits = 3) {
  paste(
    paste0(
      names(x),
      " = ",
      fmt_num(
        as.numeric(x),
        digits
      )
    ),
    collapse = ", "
  )
}

# ------------------------------------------------------------
# Stratified training and test split
#
# First 40 observations from each species are used for training.
# Last 10 observations from each species are used for testing.
# ------------------------------------------------------------

species_levels <- levels(
  iris$Species
)

train_indices <- unlist(
  lapply(
    species_levels,
    function(sp) {
      which(
        iris$Species == sp
      )[1:40]
    }
  )
)

test_indices <- setdiff(
  seq_len(
    nrow(iris)
  ),
  train_indices
)

train_data <- iris[
  train_indices,
]

test_data <- iris[
  test_indices,
]

# ------------------------------------------------------------
# Classification helpers
# ------------------------------------------------------------

calculate_centroids <- function(
    data,
    features
) {

  aggregate(
    data[
      ,
      features,
      drop = FALSE
    ],
    by = list(
      Species = data$Species
    ),
    FUN = mean
  )
}

euclidean_distance <- function(
    observation,
    centroid,
    features
) {

  sqrt(
    sum(
      (
        as.numeric(
          observation[
            features
          ]
        ) -
          as.numeric(
            centroid[
              features
            ]
          )
      )^2
    )
  )
}

nearest_centroid_predict_one <- function(
    observation,
    centroids,
    features
) {

  distances <- sapply(
    seq_len(
      nrow(centroids)
    ),
    function(i) {
      euclidean_distance(
        observation = observation,
        centroid = centroids[i, ],
        features = features
      )
    }
  )

  names(distances) <- as.character(
    centroids$Species
  )

  list(
    predicted_class = names(
      which.min(
        distances
      )
    ),
    distances = distances
  )
}

nearest_centroid_predict <- function(
    data,
    centroids,
    features
) {

  sapply(
    seq_len(
      nrow(data)
    ),
    function(i) {
      nearest_centroid_predict_one(
        observation = data[i, ],
        centroids = centroids,
        features = features
      )$predicted_class
    }
  )
}

classification_metrics <- function(
    truth,
    prediction,
    positive_class
) {

  truth_positive <- truth == positive_class
  pred_positive <- prediction == positive_class

  tp <- sum(
    truth_positive &
      pred_positive
  )

  tn <- sum(
    !truth_positive &
      !pred_positive
  )

  fp <- sum(
    !truth_positive &
      pred_positive
  )

  fn <- sum(
    truth_positive &
      !pred_positive
  )

  accuracy <- (
    tp + tn
  ) / (
    tp + tn + fp + fn
  )

  sensitivity <- ifelse(
    tp + fn == 0,
    NA_real_,
    tp / (
      tp + fn
    )
  )

  specificity <- ifelse(
    tn + fp == 0,
    NA_real_,
    tn / (
      tn + fp
    )
  )

  precision <- ifelse(
    tp + fp == 0,
    NA_real_,
    tp / (
      tp + fp
    )
  )

  list(
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision
  )
}

# ------------------------------------------------------------
# Centroids and predictions
# ------------------------------------------------------------

two_features <- c(
  "Petal.Length",
  "Petal.Width"
)

four_features <- c(
  "Sepal.Length",
  "Sepal.Width",
  "Petal.Length",
  "Petal.Width"
)

centroids_2d <- calculate_centroids(
  train_data,
  two_features
)

centroids_4d <- calculate_centroids(
  train_data,
  four_features
)

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

truth_test <- as.character(
  test_data$Species
)

confusion_2d <- table(
  Actual = truth_test,
  Predicted = pred_test_2d
)

confusion_4d <- table(
  Actual = truth_test,
  Predicted = pred_test_4d
)

accuracy_2d <- mean(
  pred_test_2d ==
    truth_test
)

accuracy_4d <- mean(
  pred_test_4d ==
    truth_test
)

versicolor_metrics_4d <- classification_metrics(
  truth = truth_test,
  prediction = pred_test_4d,
  positive_class = "versicolor"
)

# ------------------------------------------------------------
# Simple threshold rule
# ------------------------------------------------------------

threshold_predict <- function(
    petal_length,
    petal_width
) {

  if (
    petal_length < 2.5
  ) {
    "setosa"
  } else if (
    petal_width < 1.75
  ) {
    "versicolor"
  } else {
    "virginica"
  }
}

threshold_predictions <- mapply(
  threshold_predict,
  petal_length = test_data$Petal.Length,
  petal_width = test_data$Petal.Width
)

threshold_accuracy <- mean(
  threshold_predictions ==
    truth_test
)

threshold_confusion <- table(
  Actual = truth_test,
  Predicted = threshold_predictions
)

# ------------------------------------------------------------
# Selected observations
# ------------------------------------------------------------

obs_a <- test_data[
  1,
]

obs_b <- test_data[
  12,
]

obs_c <- test_data[
  24,
]

obs_d <- test_data[
  30,
]

pred_a_2d <- nearest_centroid_predict_one(
  obs_a,
  centroids_2d,
  two_features
)

pred_b_4d <- nearest_centroid_predict_one(
  obs_b,
  centroids_4d,
  four_features
)

pred_c_2d <- nearest_centroid_predict_one(
  obs_c,
  centroids_2d,
  two_features
)

threshold_d <- threshold_predict(
  obs_d$Petal.Length,
  obs_d$Petal.Width
)

# ============================================================
# Scenarios and task definitions
# ============================================================

healthcare_scenarios <- c(

  paste(
    "Before a hospital analytics team works with protected imaging records,",
    "the iris data are used as a non-clinical benchmark. Petal measurements",
    "stand in for two quantitative morphology features, and Species serves",
    "as a three-class outcome."
  ),

  paste(
    "During validation of a simple diagnostic prototype, one benchmark case",
    "must be assigned to the nearest class centre. All four iris measurements",
    "are used in the distance calculation."
  ),

  paste(
    "A quality-control note compares one new benchmark observation with",
    "the setosa, versicolor and virginica centroids. The decision is made",
    "from Petal.Length and Petal.Width only."
  ),

  paste(
    "Rather than fitting a complex model, a training exercise applies",
    "a transparent two-step rule: Petal.Length below 2.5 indicates setosa;",
    "otherwise Petal.Width below 1.75 indicates versicolor; remaining cases",
    "are labelled virginica."
  ),

  paste(
    "Thirty held-out benchmark records are classified by the two-feature",
    "nearest-centroid rule. The resulting confusion matrix is used to check",
    "how often the rule makes a correct decision."
  ),

  paste(
    "The same held-out records are classified again, this time using",
    "all four measurements. The evaluation report asks for the resulting",
    "accuracy and number of errors."
  ),

  paste(
    "For one class-specific review, versicolor is treated as the positive",
    "class and the other two species are grouped together as negative.",
    "The four-feature classifier supplies the necessary counts."
  ),

  paste(
    "A model comparison meeting asks whether the four-feature nearest-centroid",
    "rule improves on the simpler petal-only version. Both methods are evaluated",
    "on the identical 30-record test set."
  ),

  paste(
    "A threshold classifier is being considered because its decisions are",
    "easy to explain. Before adoption, its overall test-set performance",
    "must be calculated from the benchmark data."
  ),

  paste(
    "For the final numerical exercise, one held-out observation is passed",
    "through the threshold rule and then checked against its recorded Species.",
    "The task is to state both the predicted class and whether the decision is correct."
  )
)

healthcare_styles <- c(
  "benchmark-introduction",
  "distance-based",
  "centroid-comparison",
  "rule-based",
  "confusion-matrix",
  "performance-audit",
  "class-specific-metrics",
  "model-comparison",
  "interpretable-rule",
  "decision-check"
)

classification_tasks <- c(
  "nearest_centroid_2d",
  "nearest_centroid_4d",
  "compare_distances_2d",
  "apply_threshold_rule",
  "accuracy_from_confusion_2d",
  "accuracy_from_confusion_4d",
  "versicolor_metrics",
  "compare_model_accuracy",
  "threshold_accuracy",
  "prediction_correctness"
)

# ------------------------------------------------------------
# Build one question
# ------------------------------------------------------------

build_classification_question <- function(i) {

  task_name <- classification_tasks[i]

  if (task_name == "nearest_centroid_2d") {

    distances <- pred_a_2d$distances

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "For the selected case, Petal.Length = ",
      fmt_num(obs_a$Petal.Length),
      " and Petal.Width = ",
      fmt_num(obs_a$Petal.Width),
      ". The training centroids are:\n",
      "setosa (",
      fmt_num(
        centroids_2d$Petal.Length[
          centroids_2d$Species == "setosa"
        ]
      ),
      ", ",
      fmt_num(
        centroids_2d$Petal.Width[
          centroids_2d$Species == "setosa"
        ]
      ),
      "), versicolor (",
      fmt_num(
        centroids_2d$Petal.Length[
          centroids_2d$Species == "versicolor"
        ]
      ),
      ", ",
      fmt_num(
        centroids_2d$Petal.Width[
          centroids_2d$Species == "versicolor"
        ]
      ),
      "), virginica (",
      fmt_num(
        centroids_2d$Petal.Length[
          centroids_2d$Species == "virginica"
        ]
      ),
      ", ",
      fmt_num(
        centroids_2d$Petal.Width[
          centroids_2d$Species == "virginica"
        ]
      ),
      "). Calculate the three Euclidean distances and assign the nearest class."
    )

    reference_answer <- paste0(
      "Distances: setosa = ",
      fmt_num(distances["setosa"]),
      ", versicolor = ",
      fmt_num(distances["versicolor"]),
      ", virginica = ",
      fmt_num(distances["virginica"]),
      "; predicted class = ",
      pred_a_2d$predicted_class,
      "."
    )

    solution_steps <- paste0(
      "1. For each class, calculate sqrt((Petal.Length - centroid length)^2 + ",
      "(Petal.Width - centroid width)^2). ",
      "2. The distances are ",
      format_vector(distances),
      ". ",
      "3. Select the smallest distance. ",
      "4. The observation is classified as ",
      pred_a_2d$predicted_class,
      "."
    )

    predictor_value <- "Petal.Length, Petal.Width"
    answer_type <- "numeric_and_class"

  } else if (task_name == "nearest_centroid_4d") {

    distances <- pred_b_4d$distances

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The case has Sepal.Length = ",
      fmt_num(obs_b$Sepal.Length),
      ", Sepal.Width = ",
      fmt_num(obs_b$Sepal.Width),
      ", Petal.Length = ",
      fmt_num(obs_b$Petal.Length),
      ", and Petal.Width = ",
      fmt_num(obs_b$Petal.Width),
      ". Calculate its four-dimensional Euclidean distance to each class centroid",
      " and identify the nearest class."
    )

    reference_answer <- paste0(
      "Distances: setosa = ",
      fmt_num(distances["setosa"]),
      ", versicolor = ",
      fmt_num(distances["versicolor"]),
      ", virginica = ",
      fmt_num(distances["virginica"]),
      "; predicted class = ",
      pred_b_4d$predicted_class,
      "."
    )

    solution_steps <- paste0(
      "1. Subtract each centroid from the observation feature by feature. ",
      "2. Square the four differences, sum them and take the square root. ",
      "3. The resulting distances are ",
      format_vector(distances),
      ". ",
      "4. Choose ",
      pred_b_4d$predicted_class,
      " because it has the smallest distance."
    )

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"
    answer_type <- "numeric_and_class"

  } else if (task_name == "compare_distances_2d") {

    distances <- pred_c_2d$distances
    ordered_classes <- names(
      sort(
        distances
      )
    )

    margin <- sort(
      distances
    )[2] -
      sort(
        distances
      )[1]

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "For the selected record, Petal.Length = ",
      fmt_num(obs_c$Petal.Length),
      " and Petal.Width = ",
      fmt_num(obs_c$Petal.Width),
      ". Calculate the distance to each petal-feature centroid, identify the",
      " two closest classes and report the distance margin between them."
    )

    reference_answer <- paste0(
      "Distances: ",
      format_vector(distances),
      "; closest class = ",
      ordered_classes[1],
      "; second closest = ",
      ordered_classes[2],
      "; margin = ",
      fmt_num(margin),
      "."
    )

    solution_steps <- paste0(
      "1. Compute the Euclidean distance to all three centroids. ",
      "2. Order the distances from smallest to largest. ",
      "3. The closest two classes are ",
      ordered_classes[1],
      " and ",
      ordered_classes[2],
      ". ",
      "4. Their distance margin is ",
      fmt_num(margin),
      "."
    )

    predictor_value <- "Petal.Length, Petal.Width"
    answer_type <- "numeric_and_class"

  } else if (task_name == "apply_threshold_rule") {

    example_length <- 4.8
    example_width <- 1.5

    predicted <- threshold_predict(
      example_length,
      example_width
    )

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "A benchmark case has Petal.Length = ",
      fmt_num(example_length),
      " and Petal.Width = ",
      fmt_num(example_width),
      ". Apply the rule step by step and state the predicted Species."
    )

    reference_answer <- paste0(
      "Predicted class = ",
      predicted,
      "."
    )

    solution_steps <- paste0(
      "1. Petal.Length is not below 2.5, so do not assign setosa. ",
      "2. Petal.Width = ",
      fmt_num(example_width),
      " is below 1.75. ",
      "3. Therefore the rule assigns ",
      predicted,
      "."
    )

    predictor_value <- "Petal.Length, Petal.Width"
    answer_type <- "class_label"

  } else if (task_name == "accuracy_from_confusion_2d") {

    correct_count <- sum(
      diag(
        confusion_2d
      )
    )

    total_count <- sum(
      confusion_2d
    )

    error_count <- total_count -
      correct_count

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The confusion matrix is:\n",
      paste(
        capture.output(
          print(
            confusion_2d
          )
        ),
        collapse = "\n"
      ),
      "\nCalculate the number correct, number incorrect and overall accuracy."
    )

    reference_answer <- paste0(
      "Correct = ",
      correct_count,
      "; incorrect = ",
      error_count,
      "; accuracy = ",
      fmt_num(accuracy_2d),
      " (",
      fmt_pct(accuracy_2d),
      ")."
    )

    solution_steps <- paste0(
      "1. Add the diagonal cells to obtain ",
      correct_count,
      " correct classifications. ",
      "2. Subtract from ",
      total_count,
      " to obtain ",
      error_count,
      " errors. ",
      "3. Accuracy = ",
      correct_count,
      " / ",
      total_count,
      " = ",
      fmt_num(accuracy_2d),
      "."
    )

    predictor_value <- "Petal.Length, Petal.Width"
    answer_type <- "numeric"

  } else if (task_name == "accuracy_from_confusion_4d") {

    correct_count <- sum(
      diag(
        confusion_4d
      )
    )

    total_count <- sum(
      confusion_4d
    )

    error_count <- total_count -
      correct_count

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The four-feature confusion matrix is:\n",
      paste(
        capture.output(
          print(
            confusion_4d
          )
        ),
        collapse = "\n"
      ),
      "\nCalculate accuracy and the misclassification rate."
    )

    misclassification_rate <- 1 -
      accuracy_4d

    reference_answer <- paste0(
      "Accuracy = ",
      fmt_num(accuracy_4d),
      " (",
      fmt_pct(accuracy_4d),
      "); misclassification rate = ",
      fmt_num(misclassification_rate),
      " (",
      fmt_pct(misclassification_rate),
      "); errors = ",
      error_count,
      "."
    )

    solution_steps <- paste0(
      "1. Sum the diagonal counts to get ",
      correct_count,
      " correct cases. ",
      "2. Divide by ",
      total_count,
      " to obtain accuracy = ",
      fmt_num(accuracy_4d),
      ". ",
      "3. Misclassification rate = 1 - accuracy = ",
      fmt_num(misclassification_rate),
      "."
    )

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"
    answer_type <- "numeric"

  } else if (task_name == "versicolor_metrics") {

    m <- versicolor_metrics_4d

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "For versicolor as the positive class, the counts are TP = ",
      m$tp,
      ", TN = ",
      m$tn,
      ", FP = ",
      m$fp,
      ", and FN = ",
      m$fn,
      ". Calculate sensitivity, specificity and precision."
    )

    reference_answer <- paste0(
      "Sensitivity = ",
      fmt_num(m$sensitivity),
      " (",
      fmt_pct(m$sensitivity),
      "); specificity = ",
      fmt_num(m$specificity),
      " (",
      fmt_pct(m$specificity),
      "); precision = ",
      fmt_num(m$precision),
      " (",
      fmt_pct(m$precision),
      ")."
    )

    solution_steps <- paste0(
      "1. Sensitivity = TP / (TP + FN) = ",
      m$tp,
      " / ",
      m$tp + m$fn,
      " = ",
      fmt_num(m$sensitivity),
      ". ",
      "2. Specificity = TN / (TN + FP) = ",
      m$tn,
      " / ",
      m$tn + m$fp,
      " = ",
      fmt_num(m$specificity),
      ". ",
      "3. Precision = TP / (TP + FP) = ",
      m$tp,
      " / ",
      m$tp + m$fp,
      " = ",
      fmt_num(m$precision),
      "."
    )

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"
    answer_type <- "numeric"

  } else if (task_name == "compare_model_accuracy") {

    improvement <- accuracy_4d -
      accuracy_2d

    better_model <- if (
      accuracy_4d > accuracy_2d
    ) {
      "four-feature classifier"
    } else if (
      accuracy_4d < accuracy_2d
    ) {
      "two-feature classifier"
    } else {
      "neither; the accuracies are equal"
    }

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The two-feature accuracy is ",
      fmt_num(accuracy_2d),
      " and the four-feature accuracy is ",
      fmt_num(accuracy_4d),
      ". Calculate the accuracy difference as four-feature minus two-feature",
      " and identify the better-performing rule."
    )

    reference_answer <- paste0(
      "Accuracy difference = ",
      fmt_num(improvement),
      " (",
      fmt_num(
        100 * improvement,
        1
      ),
      " percentage points); better method = ",
      better_model,
      "."
    )

    solution_steps <- paste0(
      "1. Subtract ",
      fmt_num(accuracy_2d),
      " from ",
      fmt_num(accuracy_4d),
      ". ",
      "2. The difference is ",
      fmt_num(improvement),
      ". ",
      "3. Compare the two accuracy values. ",
      "4. The better result is from ",
      better_model,
      "."
    )

    predictor_value <- "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width"
    answer_type <- "numeric_and_comparison"

  } else if (task_name == "threshold_accuracy") {

    correct_count <- sum(
      threshold_predictions ==
        truth_test
    )

    total_count <- length(
      truth_test
    )

    error_count <- total_count -
      correct_count

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "Across the 30 held-out cases, the threshold rule classifies ",
      correct_count,
      " correctly and ",
      error_count,
      " incorrectly. Calculate accuracy and error rate."
    )

    reference_answer <- paste0(
      "Accuracy = ",
      fmt_num(threshold_accuracy),
      " (",
      fmt_pct(threshold_accuracy),
      "); error rate = ",
      fmt_num(
        1 - threshold_accuracy
      ),
      " (",
      fmt_pct(
        1 - threshold_accuracy
      ),
      ")."
    )

    solution_steps <- paste0(
      "1. Accuracy = ",
      correct_count,
      " / ",
      total_count,
      " = ",
      fmt_num(threshold_accuracy),
      ". ",
      "2. Error rate = ",
      error_count,
      " / ",
      total_count,
      " = ",
      fmt_num(
        1 - threshold_accuracy
      ),
      "."
    )

    predictor_value <- "Petal.Length, Petal.Width"
    answer_type <- "numeric"

  } else {

    actual_class <- as.character(
      obs_d$Species
    )

    is_correct <- threshold_d ==
      actual_class

    question <- paste0(
      healthcare_scenarios[i],
      "\n\n",
      "The observation has Petal.Length = ",
      fmt_num(obs_d$Petal.Length),
      ", Petal.Width = ",
      fmt_num(obs_d$Petal.Width),
      ", and recorded Species = ",
      actual_class,
      ". Apply the threshold rule and determine whether the classification is correct."
    )

    reference_answer <- paste0(
      "Predicted class = ",
      threshold_d,
      "; actual class = ",
      actual_class,
      "; classification is ",
      ifelse(
        is_correct,
        "correct",
        "incorrect"
      ),
      "."
    )

    solution_steps <- paste0(
      "1. Apply the Petal.Length threshold. ",
      "2. If needed, apply the Petal.Width threshold. ",
      "3. The rule predicts ",
      threshold_d,
      ". ",
      "4. Compare with the recorded class ",
      actual_class,
      ". ",
      "5. The decision is ",
      ifelse(
        is_correct,
        "correct",
        "incorrect"
      ),
      "."
    )

    predictor_value <- "Petal.Length, Petal.Width"
    answer_type <- "class_label_and_decision"
  }

  data.frame(
    id = sprintf(
      "R800_041_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_041",
    dataset_name = "iris",
    statistical_concept = "Classification",
    task = "classification_calculation",
    template_id = paste0(
      "classification_rule_",
      task_name
    ),
    difficulty = "medium",
    scenario = "healthcare",
    language_style = healthcare_styles[i],
    question_type = "calculation",
    predictor = predictor_value,
    response = "Species",
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

classification_questions <- do.call(
  rbind,
  lapply(
    seq_len(10),
    build_classification_question
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

classification_questions <- classification_questions[
  ,
  required_columns
]

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

stopifnot(
  identical(
    names(classification_questions),
    required_columns
  )
)

stopifnot(
  nrow(
    classification_questions
  ) == 10
)

stopifnot(
  length(
    unique(
      classification_questions$id
    )
  ) == 10
)

stopifnot(
  !anyDuplicated(
    classification_questions$question
  )
)

stopifnot(
  all(
    classification_questions$blueprint_id ==
      "R800_041"
  )
)

stopifnot(
  all(
    classification_questions$difficulty ==
      "medium"
  )
)

stopifnot(
  all(
    classification_questions$question_type ==
      "calculation"
  )
)

stopifnot(
  all(
    nchar(
      classification_questions$question
    ) >= 100
  )
)

stopifnot(
  all(
    nchar(
      classification_questions$solution_steps
    ) >= 40
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
    classification_questions$blueprint_id
  )
)

cat(
  "\nAnswer types:\n"
)

print(
  table(
    classification_questions$answer_type
  )
)

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
  classification_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

cat(
  "\n\n================ R800_041 example ================\n\n"
)

cat(
  classification_questions$question[1],
  "\n\nReference answer:\n",
  classification_questions$reference_answer[1],
  "\n\nSolution steps:\n",
  classification_questions$solution_steps[1],
  "\n"
)

# ------------------------------------------------------------
# Export CSV and JSON
# ------------------------------------------------------------

csv_file <- "R800_041_Classification_v2.csv"
json_file <- "R800_041_Classification_v2.json"

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
