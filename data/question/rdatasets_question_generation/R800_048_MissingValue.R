# ============================================================
# R800_048 Missing Value Generator
# Dataset: airquality
# Domain: Environmental Science
# Difficulty: Medium
# Question type: Calculation
# Output:
#   R800_048_MissingValue_v2.csv
#   R800_048_MissingValue_v2.json
# ============================================================

set.seed(20260711)

if (!requireNamespace("jsonlite", quietly=TRUE))
  install.packages("jsonlite")
library(jsonlite)

fmt <- function(x,d=3) formatC(x,format="f",digits=d)

aq <- airquality

stats <- list(
  ozone_na=sum(is.na(aq$Ozone)),
  solar_na=sum(is.na(aq$Solar.R)),
  ozone_mean=mean(aq$Ozone,na.rm=TRUE),
  ozone_median=median(aq$Ozone,na.rm=TRUE),
  solar_mean=mean(aq$Solar.R,na.rm=TRUE),
  wind_mean=mean(aq$Wind,na.rm=TRUE),
  temp_mean=mean(aq$Temp,na.rm=TRUE)
)

scenarios <- c(
"Before calibrating an air-quality forecasting system, technicians first review missing ozone observations.",
"During an environmental impact assessment, analysts discover that several Solar.R values are unavailable.",
"Rather than discarding the entire dataset immediately, a monitoring team compares mean and median imputation.",
"Following an overnight sensor outage, the number of incomplete ozone records must be quantified.",
"A monthly climate report requires missing Solar.R values to be replaced before summary statistics are produced.",
"While validating historical pollution records, an analyst calculates the percentage of missing ozone measurements.",
"Instead of filling every missing value automatically, the project first estimates the mean from observed records.",
"After merging several monitoring stations, engineers compare complete-case analysis with simple imputation.",
"Preparing a teaching example for environmental modelling, a scientist demonstrates how missing values affect averages.",
"To complete the preprocessing pipeline, the analyst documents the chosen imputation method and resulting statistic."
)

tasks <- c(
"count_ozone_na","count_solar_na","mean_imputation","median_imputation",
"solar_mean","ozone_missing_pct","wind_mean","complete_case_rows",
"temp_mean","method_choice")

build_q <- function(i){

task <- tasks[i]

if(task=="count_ozone_na"){
q <- paste0(scenarios[i],"\n\nHow many missing values are recorded in Ozone?")
a <- stats$ozone_na
s <- "Count NA values using sum(is.na(airquality$Ozone))."
}
else if(task=="count_solar_na"){
q <- paste0(scenarios[i],"\n\nHow many missing values are recorded in Solar.R?")
a <- stats$solar_na
s <- "Count NA values using sum(is.na(airquality$Solar.R))."
}
else if(task=="mean_imputation"){
q <- paste0(scenarios[i],"\n\nWhat value would replace each missing Ozone observation under mean imputation?")
a <- fmt(stats$ozone_mean)
s <- paste0("Compute mean(Ozone, na.rm=TRUE) = ",fmt(stats$ozone_mean),".")
}
else if(task=="median_imputation"){
q <- paste0(scenarios[i],"\n\nIf median imputation is selected, what value replaces every missing Ozone observation?")
a <- fmt(stats$ozone_median)
s <- paste0("Compute median(Ozone, na.rm=TRUE) = ",fmt(stats$ozone_median),".")
}
else if(task=="solar_mean"){
q <- paste0(scenarios[i],"\n\nCalculate the mean of Solar.R after excluding missing values.")
a <- fmt(stats$solar_mean)
s <- paste0("Use mean(Solar.R, na.rm=TRUE) = ",fmt(stats$solar_mean),".")
}
else if(task=="ozone_missing_pct"){
pct <- stats$ozone_na/nrow(aq)
q <- paste0(scenarios[i],"\n\nWhat percentage of Ozone observations is missing?")
a <- paste0(fmt(100*pct,1),"%")
s <- paste0("Missing percentage = ",stats$ozone_na,"/",nrow(aq)," ×100 = ",a,".")
}
else if(task=="wind_mean"){
q <- paste0(scenarios[i],"\n\nReport the mean Wind value after ignoring missing observations.")
a <- fmt(stats$wind_mean)
s <- paste0("mean(Wind)= ",fmt(stats$wind_mean),".")
}
else if(task=="complete_case_rows"){
cc <- sum(complete.cases(aq[,c("Ozone","Solar.R","Wind","Temp","Month")]))
q <- paste0(scenarios[i],"\n\nHow many complete cases remain when only Ozone, Solar.R, Wind, Temp and Month are retained?")
a <- cc
s <- "Use complete.cases() on the selected variables and count TRUE values."
}
else if(task=="temp_mean"){
q <- paste0(scenarios[i],"\n\nCalculate the average temperature (Temp).")
a <- fmt(stats$temp_mean)
s <- paste0("mean(Temp)= ",fmt(stats$temp_mean),".")
}
else{
q <- paste0(scenarios[i],"\n\nA small proportion of values is missing. Which simple imputation method replaces every missing value with the average of the observed values?")
a <- "Mean imputation"
s <- "Mean imputation replaces each missing value with the observed-variable mean."
}

data.frame(
id=sprintf("R800_048_%03d",i),
source="R-generated",
blueprint_id="R800_048",
dataset_name="airquality",
statistical_concept="Missing Value",
task="missing_value_calculation",
template_id=paste0("missing_value_",task),
difficulty="medium",
scenario="environmental_science",
language_style=paste0("style_",i),
question_type="calculation",
predictor="Ozone, Solar.R, Wind, Temp, Month",
response="missing value",
question=q,
reference_answer=as.character(a),
solution_steps=s,
answer_type="numeric",
version="v2.0",
stringsAsFactors=FALSE)
}

df <- do.call(rbind,lapply(1:10,build_q))

write.csv(df,"R800_048_MissingValue_v2.csv",row.names=FALSE,fileEncoding="UTF-8")
write_json(df,"R800_048_MissingValue_v2.json",dataframe="rows",pretty=TRUE,auto_unbox=TRUE)

cat("Generated",nrow(df),"questions\n")
