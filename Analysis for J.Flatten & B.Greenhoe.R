# Preliminary Set-Up:

## Installing Necessary Packages
library(tidyverse)
install.packages('rstatix'); library(rstatix)
install.packages('gtsummary'); library(gtsummary)

## Loading in Data & Naming it
PreData <- read.csv("https://raw.githubusercontent.com/abbyvenz/Analysis_for_J.Flatten_and_B.Greenhoe/main/CARED%20Survey%20-%20Baseline.csv")
PostData_1Week <- read.csv("https://raw.githubusercontent.com/abbyvenz/Analysis_for_J.Flatten_and_B.Greenhoe/main/CARED%20Survey%20-%201%20week.csv")
PostData_1Month <- read.csv("https://raw.githubusercontent.com/abbyvenz/Analysis_for_J.Flatten_and_B.Greenhoe/main/CARED%20Survey%20-%201%20month.csv")

SET.M_Data <- read.csv("https://raw.githubusercontent.com/abbyvenz/Analysis_for_J.Flatten_and_B.Greenhoe/main/Simulation%20Effectiveness%20Tool%20-%20Modified%20(SET-M)%20Survey.csv")

#--------------------------------------------------------------------------------------------------------------------------------

# Cleaning & Analyzing the CARED Survey Data:

# *CLEANING*

## Recoding the Likert Scales (1-5)
recode_vec = c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5, "Strongly Agree" = 5)
reverse_recode_vec = c("Strongly disagree" = 5, "Disagree" = 4, "Neutral" = 3, "Agree" = 2, "Strongly agree" = 1, "Strongly Agree" = 1)

Recode <- function(x) {
  recode(x, !!!recode_vec)
}

Reverse_Recode <- function(x) {
  recode(x, !!!reverse_recode_vec)
}

Partial.Recoded.PreData = PreData %>% mutate(across(c(3, 4, 5, 6, 7, 8, 15, 16, 17, 18, 19, 20, 21, 22), Recode))
Recoded.PreData = Partial.Recoded.PreData %>% mutate(across(c(9, 10, 11, 12, 13, 14), Reverse_Recode))

Partial.Recoded.PostData_1Week = PostData_1Week %>% mutate(across(c(2, 3, 4, 5, 6, 7, 14, 15, 16, 17, 18, 19, 20, 21), Recode))
Recoded.PostData_1Week = Partial.Recoded.PostData_1Week %>% mutate(across(c(8, 9, 10, 11, 12, 13), Reverse_Recode))

Partial.Recoded.PostData_1Month = PostData_1Month %>% mutate(across(c(2, 3, 4, 5, 6, 7, 14, 15, 16, 17, 18, 19, 20, 21), Recode))
Recoded.PostData_1Month = Partial.Recoded.PostData_1Month %>% mutate(across(c(8, 9, 10, 11, 12, 13), Reverse_Recode))

## Combining the Pre (Baseline), and Post (1 Week and 1 Month) surveys into one data-set
Partial.Combined.Data = merge(Recoded.PreData, Recoded.PostData_1Week, by = "UniqueID")
Combined.Data = merge(Partial.Combined.Data, Recoded.PostData_1Month, by = "UniqueID")

## Grouping Responses in the Combined Data (into their respective Categories of Measurement) *by aggregating*
Combined.Data$MeanPreConfidence <- rowMeans(Combined.Data[ , c(3, 6, 7, 8, 9, 10, 11, 13)], na.rm = TRUE) 
Combined.Data$Mean1WeekConfidence <- rowMeans(Combined.Data[ , c(23, 26, 27, 28, 29, 30, 31, 33)], na.rm = TRUE)
Combined.Data$Mean1MonthConfidence <- rowMeans(Combined.Data[ , c(43, 46, 47, 48, 49, 50, 51, 53)], na.rm = TRUE)

Combined.Data$MeanPreEnvironment <- rowMeans(Combined.Data[, c(5, 12, 14, 15, 16)], na.rm = TRUE) 
Combined.Data$Mean1WeekEnvironment <- rowMeans(Combined.Data[, c(25, 32, 34, 35, 36)], na.rm = TRUE) 
Combined.Data$Mean1MonthEnvironment <- rowMeans(Combined.Data[, c(45, 52, 54, 55, 56)], na.rm = TRUE)

Combined.Data$MeanPreKnowledge <- rowMeans(Combined.Data[, c(4, 17, 18, 19, 20, 21, 22)], na.rm = TRUE)
Combined.Data$Mean1WeekKnowledge <- rowMeans(Combined.Data[, c(24, 37, 38, 39, 40, 41, 42)], na.rm = TRUE)
Combined.Data$Mean1MonthKnowledge <- rowMeans(Combined.Data[, c(44, 57, 58, 59, 60, 61, 62)], na.rm = TRUE)

## Subsetting the Grouped Columns into their own data-set for the analysis
Combined.Data.Subset = subset(Combined.Data, select = c("UniqueID", "MeanPreConfidence", "Mean1WeekConfidence", "Mean1MonthConfidence", 
                                                        "MeanPreEnvironment", "Mean1WeekEnvironment", "Mean1MonthEnvironment", 
                                                        "MeanPreKnowledge", "Mean1WeekKnowledge", "Mean1MonthKnowledge"))

# *ANALYZING*

## Summary Statistics of Baseline, 1 Week, and 1 Month Surveys
get_summary_stats(Combined.Data.Subset, type = "mean_sd")

## Two-Sample Paired t-tests for Differences in Means -
  ### Confidence
  # Baseline - 1 Week:
t.test(Combined.Data.Subset$Mean1WeekConfidence, Combined.Data.Subset$MeanPreConfidence,
       paired = TRUE)
  # Baseline - 1 Month:
t.test(Combined.Data.Subset$Mean1MonthConfidence, Combined.Data.Subset$MeanPreConfidence,
       paired = TRUE)

  ### Environment
  # Baseline - 1 Week:
t.test(Combined.Data.Subset$Mean1WeekEnvironment, Combined.Data.Subset$MeanPreEnvironment,
       paired = TRUE)
  # Baseline - 1 Month:
t.test(Combined.Data.Subset$Mean1MonthEnvironment, Combined.Data.Subset$MeanPreEnvironment, 
       paired = TRUE)

  ### Knowledge
  # Baseline - 1 Week:
t.test(Combined.Data.Subset$Mean1WeekKnowledge, Combined.Data.Subset$MeanPreKnowledge,
       paired = TRUE)
  # Baseline - 1 Month:
t.test(Combined.Data.Subset$Mean1MonthKnowledge, Combined.Data.Subset$MeanPreKnowledge,
       paired = TRUE)

#--------------------------------------------------------------------------------------------------------------------------------

# Cleaning and Summarizing the SET-M Data:

# *CLEANING*

## Grouping Responses in the SET-M Data (into their respective Categories of Measurement)
SET.M_Data$MeanPrebriefing <- rowMeans(SET.M_Data[, 2:3], na.rm = TRUE)
SET.M_Data$MeanLearning <- rowMeans(SET.M_Data[, 4:9], na.rm = TRUE)
SET.M_Data$MeanConfidence <- rowMeans(SET.M_Data[, 10:15], na.rm = TRUE)
SET.M_Data$MeanDebriefing <- rowMeans(SET.M_Data[, 16:20], na.rm = TRUE)

## Subsetting the Grouped SET-M Columns into their own data-set for the summary
SET.M_Data.Subset = subset(SET.M_Data, select = c("Subject", "MeanPrebriefing",
                                                  "MeanLearning", "MeanConfidence", "MeanDebriefing"))

# *SUMMARIZING* 

get_summary_stats(SET.M_Data.Subset[, -1], type = "mean_sd")
