# ============================================================
# Classification Question Generator
#
# R800_044
# Dataset: iris
# Domain: Sports Analytics
# Difficulty: Hard
# Question type: Interpretation
# Count: 10
#
# Outputs:
# 1. R800_044_Classification_v2.csv
# 2. R800_044_Classification_v2.json
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

format_ci <- function(lower, upper, digits = 3) {
  paste0(
    "[",
    fmt_num(lower, digits),
    ", ",
    fmt_num(upper, digits),
    "]"
  )
}

format_named_values <- function(x, digits = 3) {
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
# Stratified split
#
# First 40 observations per species are used for training.
# Last 10 observations per species are held out for evaluation.
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

truth_test <- as.character(
  test_data$Species
)

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

  truth_positive <- truth ==
    positive_class

  pred_positive <- prediction ==
    positive_class

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
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision
  )
}

# ------------------------------------------------------------
# Two-feature and four-feature nearest-centroid rules
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

# ------------------------------------------------------------
# Standardised four-feature classifier
# ------------------------------------------------------------

feature_means <- sapply(
  train_data[
    ,
    four_features,
    drop = FALSE
  ],
  mean
)

feature_sds <- sapply(
  train_data[
    ,
    four_features,
    drop = FALSE
  ],
  sd
)

standardise_features <- function(data, features, means, sds) {

  result <- data[, features, drop = FALSE]

  for (feature_name in features) {
    result[[feature_name]] <-
      (result[[feature_name]] - means[feature_name]) /
      sds[feature_name]
  }

  result
}

train_standardised <- standardise_features(
  train_data,
  four_features,
  feature_means,
  feature_sds
)

test_standardised <- standardise_features(
  test_data,
  four_features,
  feature_means,
  feature_sds
)

train_standardised$Species <- train_data$Species

centroids_4d_standardised <- calculate_centroids(
  train_standardised,
  four_features
)

pred_test_4d_standardised <- nearest_centroid_predict(
  test_standardised,
  centroids_4d_standardised,
  four_features
)

accuracy_4d_standardised <- mean(
  pred_test_4d_standardised ==
    truth_test
)

confusion_4d_standardised <- table(
  Actual = truth_test,
  Predicted = pred_test_4d_standardised
)

# ------------------------------------------------------------
# Class-specific metrics
# ------------------------------------------------------------

virginica_metrics <- classification_metrics(
  truth = truth_test,
  prediction = pred_test_4d,
  positive_class = "virginica"
)

versicolor_metrics <- classification_metrics(
  truth = truth_test,
  prediction = pred_test_4d,
  positive_class = "versicolor"
)

# ------------------------------------------------------------
# Selected cases
# ------------------------------------------------------------

case_indices <- c(
  3,
  12,
  18,
  24,
  29
)

selected_cases <- test_data[
  case_indices,
]

case_predictions_2d <- lapply(
  seq_len(
    nrow(selected_cases)
  ),
  function(i) {
    nearest_centroid_predict_one(
      selected_cases[i, ],
      centroids_2d,
      two_features
    )
  }
)

case_predictions_4d <- lapply(
  seq_len(
    nrow(selected_cases)
  ),
  function(i) {
    nearest_centroid_predict_one(
      selected_cases[i, ],
      centroids_4d,
      four_features
    )
  }
)

selected_cases_standardised <- standardise_features(
  selected_cases,
  four_features,
  feature_means,
  feature_sds
)

case_predictions_4d_standardised <- lapply(
  seq_len(
    nrow(selected_cases_standardised)
  ),
  function(i) {
    nearest_centroid_predict_one(
      selected_cases_standardised[i, ],
      centroids_4d_standardised,
      four_features
    )
  }
)

# ============================================================
# Scenarios
# ============================================================

sports_scenarios <- c(

  paste(
    "During pre-season testing, four movement measurements are reduced to",
    "a benchmark classification problem before the club works with real athlete data.",
    "Species represents three performance profiles, while the iris measurements",
    "stand in for mobility and power features."
  ),

  paste(
    "Halfway through a scouting review, one athlete-style profile sits almost",
    "equidistant from two class centroids. The coaching staff wants to know whether",
    "the nearest-centroid label should be treated as a confident decision."
  ),

  paste(
    "Rather than relying on raw distance alone, the performance unit asks whether",
    "standardising the four features changes the test-set classification accuracy.",
    "The concern is that variables measured on wider scales may dominate Euclidean distance."
  ),

  paste(
    "With virginica treated as the positive performance profile, the medical and",
    "coaching teams focus on missed cases rather than overall accuracy.",
    "The confusion-matrix counts are available for interpretation."
  ),

  paste(
    "A talent pathway uses the four-feature classifier to flag versicolor-style",
    "profiles. Selection staff are more concerned about the reliability of positive",
    "flags than about the overall proportion classified correctly."
  ),

  paste(
    "Two versions of the same rule are presented to the coaching board:",
    "one uses only petal-style features, while the other uses all four measurements.",
    "The board asks whether the more complex version is genuinely better."
  ),

  paste(
    "One held-out profile receives different labels from the petal-only and",
    "four-feature classifiers. Instead of choosing a label mechanically,",
    "the analyst must explain what the disagreement reveals about the decision boundary."
  ),

  paste(
    "A performance report states that 90% accuracy means every athlete has a",
    "90% chance of being classified correctly. That sentence appears plausible",
    "but mixes up group-level evaluation with individual probability."
  ),

  paste(
    "Because the held-out set contains equal numbers from all three classes,",
    "overall accuracy is easy to read. The club now asks how the interpretation",
    "would change if one performance class were far more common than the others."
  ),

  paste(
    "For the closing discussion, the analyst must combine centroid distance,",
    "test accuracy, class-specific errors, feature scaling and the use of a",
    "non-sport benchmark into one balanced interpretation."
  )
)

sports_styles <- c(
  "preseason-benchmark",
  "borderline-case",
  "feature-scaling",
  "missed-case-focus",
  "selection-reliability",
  "complexity-comparison",
  "classifier-disagreement",
  "accuracy-misinterpretation",
  "class-balance",
  "integrated-review"
)

sports_tasks <- c(
  "interpret_nearest_centroid",
  "borderline_distance",
  "standardisation_effect",
  "virginica_sensitivity",
  "versicolor_precision",
  "compare_feature_sets",
  "classifier_disagreement",
  "accuracy_not_probability",
  "class_imbalance",
  "balanced_interpretation"
)

# ------------------------------------------------------------
# Build one question
# ------------------------------------------------------------

build_sports_question <- function(i) {

  task_name <- sports_tasks[i]

  if (task_name == "interpret_nearest_centroid") {

    case <- selected_cases[
      1,
    ]

    result <- case_predictions_4d[[1]]

    actual_class <- as.character(
      case$Species
    )

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For the selected profile, the distances to the four-feature centroids are ",
      format_named_values(
        result$distances
      ),
      ". The rule predicts ",
      result$predicted_class,
      ", while the recorded class is ",
      actual_class,
      ". Interpret the decision in context."
    )

    reference_answer <- paste0(
      "The profile is assigned to ",
      result$predicted_class,
      " because that centroid has the smallest Euclidean distance. ",
      "The classification is ",
      ifelse(
        result$predicted_class ==
          actual_class,
        "correct",
        "incorrect"
      ),
      " for this held-out case. ",
      "Even when correct, the label means only that the profile is most similar to that class centre under the chosen features and distance rule; it is not a diagnosis or certainty statement."
    )

    solution_steps <- paste0(
      "1. Compare the three distances. ",
      "2. Select the class with the minimum distance. ",
      "3. Compare the predicted and recorded classes. ",
      "4. Interpret the label as a similarity-based decision rather than a guaranteed truth."
    )

  } else if (task_name == "borderline_distance") {

    # Find the selected case with the smallest gap between the nearest
    # and second-nearest four-feature centroid.
    margins <- sapply(
      case_predictions_4d,
      function(result) {
        ordered <- sort(
          result$distances
        )

        ordered[2] -
          ordered[1]
      }
    )

    selected_index <- which.min(
      margins
    )

    result <- case_predictions_4d[[selected_index]]

    ordered_distances <- sort(
      result$distances
    )

    margin <- ordered_distances[2] -
      ordered_distances[1]

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "The two smallest centroid distances are ",
      fmt_num(
        ordered_distances[1]
      ),
      " for ",
      names(
        ordered_distances
      )[1],
      " and ",
      fmt_num(
        ordered_distances[2]
      ),
      " for ",
      names(
        ordered_distances
      )[2],
      ", leaving a margin of ",
      fmt_num(margin),
      ". How should this result be interpreted?"
    )

    reference_answer <- paste0(
      "The rule still assigns the case to ",
      names(
        ordered_distances
      )[1],
      ", but the small distance margin shows that the profile lies close to the competing class centre. ",
      "The label should therefore be treated as less secure than a classification with a large separation. ",
      "Nearest-centroid distance is not itself a calibrated probability."
    )

    solution_steps <- paste0(
      "1. Identify the two nearest centroids. ",
      "2. Calculate their distance difference. ",
      "3. Interpret a small margin as a borderline geometric decision. ",
      "4. Avoid converting the margin directly into a probability of correctness."
    )

  } else if (task_name == "standardisation_effect") {

    improvement <- accuracy_4d_standardised -
      accuracy_4d

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "Using raw features, test accuracy is ",
      fmt_num(
        accuracy_4d
      ),
      ". After standardising each feature from the training data, accuracy is ",
      fmt_num(
        accuracy_4d_standardised
      ),
      ". The change is ",
      fmt_num(improvement),
      ". Interpret the comparison."
    )

    reference_answer <- paste0(
      "Standardisation changes each feature to a common scale before distance is calculated. ",
      if (
        improvement > 0
      ) {
        paste0(
          "Here it improves accuracy by ",
          fmt_num(
            100 * improvement,
            1
          ),
          " percentage points, suggesting that raw scale differences were affecting classification."
        )
      } else if (
        improvement < 0
      ) {
        paste0(
          "Here it reduces accuracy by ",
          fmt_num(
            100 * abs(improvement),
            1
          ),
          " percentage points, so equalising the scales does not improve this particular test set."
        )
      } else {
        "Here it leaves accuracy unchanged, although the individual distance geometry may still differ."
      },
      " The comparison is based on one fixed split and should not be treated as final evidence without repeated validation."
    )

    solution_steps <- paste0(
      "1. Compare raw and standardised accuracies. ",
      "2. Interpret what scaling changes in Euclidean distance. ",
      "3. State the direction and size of the observed accuracy change. ",
      "4. Add the limitation that one train-test split may be unstable."
    )

  } else if (task_name == "virginica_sensitivity") {

    m <- virginica_metrics

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For virginica, TP = ",
      m$tp,
      " and FN = ",
      m$fn,
      ", giving sensitivity = ",
      fmt_num(
        m$sensitivity
      ),
      ". What does this quantity mean for the flagging system?"
    )

    reference_answer <- paste0(
      "Sensitivity measures the proportion of actual virginica-style profiles that the classifier successfully identifies. ",
      "A value of ",
      fmt_pct(
        m$sensitivity
      ),
      " means that this percentage of true positive profiles is detected, while the remaining ",
      fmt_pct(
        1 -
          m$sensitivity
      ),
      " is missed. ",
      "This metric is especially relevant when failing to flag a target profile is costly."
    )

    solution_steps <- paste0(
      "1. Use sensitivity = TP / (TP + FN). ",
      "2. Identify the denominator as all actual virginica cases. ",
      "3. Translate the result into detected and missed percentages. ",
      "4. Connect the metric to the cost of false negatives."
    )

  } else if (task_name == "versicolor_precision") {

    m <- versicolor_metrics

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For versicolor, TP = ",
      m$tp,
      " and FP = ",
      m$fp,
      ", so precision = ",
      fmt_num(
        m$precision
      ),
      ". Interpret this result for selection staff."
    )

    reference_answer <- paste0(
      "Precision is the proportion of profiles predicted as versicolor that are actually versicolor. ",
      "A value of ",
      fmt_pct(
        m$precision
      ),
      " means that this percentage of positive flags is correct, while the rest are false positives. ",
      "It addresses the reliability of a selection flag, not the proportion of all true versicolor cases found."
    )

    solution_steps <- paste0(
      "1. Use precision = TP / (TP + FP). ",
      "2. Identify the denominator as all predicted versicolor cases. ",
      "3. Translate the value into correct and incorrect positive flags. ",
      "4. Distinguish precision from sensitivity."
    )

  } else if (task_name == "compare_feature_sets") {

    difference <- accuracy_4d -
      accuracy_2d

    better_rule <- if (
      accuracy_4d >
        accuracy_2d
    ) {
      "the four-feature rule"
    } else if (
      accuracy_4d <
        accuracy_2d
    ) {
      "the petal-only rule"
    } else {
      "neither rule; the accuracies are equal"
    }

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "Petal-only accuracy is ",
      fmt_num(
        accuracy_2d
      ),
      ", whereas four-feature accuracy is ",
      fmt_num(
        accuracy_4d
      ),
      ". The difference is ",
      fmt_num(difference),
      ". What conclusion is justified?"
    )

    reference_answer <- paste0(
      "On this particular held-out set, ",
      better_rule,
      " has the higher accuracy. ",
      if (
        difference != 0
      ) {
        paste0(
          "The observed difference is ",
          fmt_num(
            100 * abs(difference),
            1
          ),
          " percentage points."
        )
      } else {
        "No accuracy advantage is observed."
      },
      " The more complex feature set should not be declared universally superior from one split alone; repeated resampling and class-specific metrics would provide stronger evidence."
    )

    solution_steps <- paste0(
      "1. Compare the two accuracy values. ",
      "2. Calculate the percentage-point difference. ",
      "3. Identify the higher-performing rule on this test set. ",
      "4. Qualify the result because accuracy from one split is sample-dependent."
    )

  } else if (task_name == "classifier_disagreement") {

    disagreement_indices <- which(
      pred_test_2d !=
        pred_test_4d
    )

    if (
      length(
        disagreement_indices
      ) == 0
    ) {
      # Use a selected case and discuss agreement if the fixed split
      # happens to produce no disagreement.
      idx <- 1
    } else {
      idx <- disagreement_indices[1]
    }

    case <- test_data[
      idx,
    ]

    pred_2d <- pred_test_2d[
      idx
    ]

    pred_4d <- pred_test_4d[
      idx
    ]

    actual <- truth_test[
      idx
    ]

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "For one case, the petal-only rule predicts ",
      pred_2d,
      ", the four-feature rule predicts ",
      pred_4d,
      ", and the recorded class is ",
      actual,
      ". Explain what the disagreement means."
    )

    reference_answer <- paste0(
      "The rules use different feature spaces, so they place the same case differently relative to the class centroids. ",
      if (
        pred_2d ==
          pred_4d
      ) {
        "In this fixed split the chosen case actually receives the same label, showing agreement rather than disagreement; the broader point is that adding sepal features can change distance geometry."
      } else {
        paste0(
          "Here the disagreement shows that the extra sepal measurements materially change the nearest class. ",
          "The recorded label indicates that ",
          ifelse(
            pred_2d ==
              actual,
            "the petal-only rule is correct for this case",
            ifelse(
              pred_4d ==
                actual,
              "the four-feature rule is correct for this case",
              "neither rule is correct for this case"
            )
          ),
          "."
        )
      },
      " A single case should not determine which classifier is better overall."
    )

    solution_steps <- paste0(
      "1. Identify the feature set used by each rule. ",
      "2. Compare the two predicted labels. ",
      "3. Check both predictions against the recorded class. ",
      "4. Explain that disagreement reflects different geometric representations, not random inconsistency."
    )

  } else if (task_name == "accuracy_not_probability") {

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "The four-feature classifier has test accuracy ",
      fmt_pct(
        accuracy_4d
      ),
      ". Evaluate the statement that every new athlete therefore has exactly this probability of being classified correctly."
    )

    reference_answer <- paste0(
      "The statement is too strong. Test accuracy is the proportion correctly classified in this particular held-out sample. ",
      "It is an estimate of average performance under similar conditions, not a calibrated case-specific probability for every new profile. ",
      "Individual uncertainty depends on location relative to class boundaries and on whether future data resemble the test set."
    )

    solution_steps <- paste0(
      "1. Define empirical test accuracy. ",
      "2. Distinguish a sample-level proportion from a case-specific probability. ",
      "3. Note that classification difficulty varies across observations. ",
      "4. Add the requirement that future data come from a similar population."
    )

  } else if (task_name == "class_imbalance") {

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "The current test set contains 10 observations from each class and the four-feature accuracy is ",
      fmt_pct(
        accuracy_4d
      ),
      ". Explain why the same accuracy could be less informative in a heavily imbalanced athlete population."
    )

    reference_answer <- paste0(
      "With equal class counts, each class contributes similarly to overall accuracy. ",
      "If one class dominated, a classifier could achieve high accuracy by favouring the majority class while performing poorly on rarer but important profiles. ",
      "Sensitivity, specificity, precision, class-wise recall and a balanced metric would then be needed alongside accuracy."
    )

    solution_steps <- paste0(
      "1. Recognise that the current evaluation is class-balanced. ",
      "2. Explain how a majority class can dominate ordinary accuracy. ",
      "3. Identify the risk of hiding poor minority-class performance. ",
      "4. Recommend class-specific or balanced metrics."
    )

  } else {

    question <- paste0(
      sports_scenarios[i],
      "\n\n",
      "Raw four-feature accuracy is ",
      fmt_pct(
        accuracy_4d
      ),
      ", standardised-feature accuracy is ",
      fmt_pct(
        accuracy_4d_standardised
      ),
      ", virginica sensitivity is ",
      fmt_pct(
        virginica_metrics$sensitivity
      ),
      ", and versicolor precision is ",
      fmt_pct(
        versicolor_metrics$precision
      ),
      ". Write a balanced interpretation."
    )

    reference_answer <- paste0(
      "The nearest-centroid rule performs ",
      ifelse(
        accuracy_4d >= 0.90,
        "strongly",
        "moderately"
      ),
      " on this balanced hold-out set, but overall accuracy does not reveal every class-specific weakness. ",
      "Virginica sensitivity describes how many target cases are detected, while versicolor precision describes how trustworthy positive versicolor flags are. ",
      "Standardisation changes the distance geometry and should be evaluated rather than assumed beneficial. ",
      "Because iris is only a non-sport benchmark and the split is fixed, the results do not establish readiness for real athlete classification."
    )

    solution_steps <- paste0(
      "1. Interpret overall accuracy. ",
      "2. Add class-specific sensitivity and precision. ",
      "3. Explain the role of feature scaling. ",
      "4. Add limitations concerning one split, nearest-centroid simplicity and domain transfer from iris to sport."
    )
  }

  data.frame(
    id = sprintf(
      "R800_044_%03d",
      i
    ),
    source = "R-generated",
    blueprint_id = "R800_044",
    dataset_name = "iris",
    statistical_concept = "Classification",
    task = "classification_interpretation",
    template_id = paste0(
      "classification_rule_",
      task_name
    ),
    difficulty = "hard",
    scenario = "sports_analytics",
    language_style = sports_styles[i],
    question_type = "interpretation",
    predictor = "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width",
    response = "Species",
    question = question,
    reference_answer = reference_answer,
    solution_steps = solution_steps,
    answer_type = "written_interpretation",
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
    build_sports_question
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
    names(
      classification_questions
    ),
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
      "R800_044"
  )
)

stopifnot(
  all(
    classification_questions$difficulty ==
      "hard"
  )
)

stopifnot(
  all(
    classification_questions$question_type ==
      "interpretation"
  )
)

stopifnot(
  all(
    nchar(
      classification_questions$question
    ) >= 120
  )
)

stopifnot(
  all(
    nchar(
      classification_questions$reference_answer
    ) >= 100
  )
)

stopifnot(
  all(
    nchar(
      classification_questions$solution_steps
    ) >= 50
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
  "\nLanguage styles:\n"
)

print(
  table(
    classification_questions$language_style
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
  "template_id"
)

print(
  classification_questions[
    ,
    preview_columns
  ],
  row.names = FALSE
)

cat(
  "\n\n================ R800_044 example ================\n\n"
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

csv_file <- "R800_044_Classification_v2.csv"
json_file <- "R800_044_Classification_v2.json"

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
  nrow(
    classification_questions
  ),
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
