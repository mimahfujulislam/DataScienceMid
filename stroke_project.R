install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)
library(caret)
library(ggplot2)
library(dplyr)

data <- read.csv("Dataset/stroke.csv")

head(data)
tail(data)
View(data)

dim(data)
names(data)
str(data)
summary(data)

colSums(is.na(data))

class(data)

nrow(data)
ncol(data)

data_missing <- data

data_missing$gender[c(5, 20, 35, 50, 65,
                      80, 95, 110, 125, 140,
                      155, 170, 185, 200, 215,
                      230, 245, 260, 275, 290)] <- NA

colSums(is.na(data_missing))

write.csv(
  data_missing,
  "Dataset/stroke_gender_missing.csv",
  row.names = FALSE
)

mode_gender <- names(
  sort(
    table(data_missing$gender),
    decreasing = TRUE
  )
)[1]

mode_gender

data_imputed <- data_missing

data_imputed$gender[
  is.na(data_imputed$gender)
] <- mode_gender

colSums(is.na(data_imputed))

View(data_imputed)

write.csv(
  data_imputed,
  "Dataset/stroke_gender_mode_imputed.csv",
  row.names = FALSE
)

data_discard <- na.omit(data)

dim(data)

dim(data_discard)

colSums(is.na(data_discard))

View(data_discard)

write.csv(
  data_discard,
  "Dataset/stroke_bmi_discard.csv",
  row.names = FALSE
)

data_noise <- data_discard

data_noise$age[c(15, 100, 250, 500, 700,
                 850, 1000, 1150, 1300, 1450,
                 1600, 1750, 1900, 2050, 2200,
                 2350, 2500, 2650, 2800, 2950)] <-
  c(200, -10, 180, 250, -5,
    160, 300, -15, 190, 210,
    -8, 175, 240, -12, 185,
    260, -20, 170, 230, -7)

data_noise$gender[c(25, 125, 225, 325, 425,
                    525, 625, 725, 825, 925,
                    1025, 1125, 1225, 1325, 1425,
                    1525, 1625, 1725, 1825, 1925)] <-
  c("ABC", "Unknown", "XYZ", "123", "Test",
    "Male123", "Female@", "Other1", "AAAA", "BBBB",
    "Random", "Noise", "NullValue", "Error", "GenderX",
    "Sample", "Dummy", "Wrong", "Invalid", "Hello")

range(data_discard$age, na.rm = TRUE)

min_age <- min(data_discard$age, na.rm = TRUE)
max_age <- max(data_discard$age, na.rm = TRUE)

noise_age <- data_noise[
  data_noise$age < min_age |
    data_noise$age > max_age,
]

noise_age

View(noise_age)

nrow(noise_age)

noise_gender <- data_noise[
  !(data_noise$gender %in% c("Male", "Female", "Other")),
]

noise_gender

View(noise_gender)

table(data_noise$gender)

write.csv(
  data_noise,
  "Dataset/stroke_noise_added.csv",
  row.names = FALSE
)

data_noise$age[
  data_noise$age < min_age |
    data_noise$age > max_age
] <- median(data_discard$age, na.rm = TRUE)

data_noise[
  data_noise$age < min_age |
    data_noise$age > max_age,
]

data_noise$gender[
  !(data_noise$gender %in% c("Male", "Female", "Other"))
] <- mode_gender

data_noise[
  !(data_noise$gender %in% c("Male", "Female", "Other")),
]

nrow(
  data_noise[
    data_noise$age < min_age |
      data_noise$age > max_age,
  ]
)

nrow(
  data_noise[
    !(data_noise$gender %in% c("Male", "Female", "Other")),
  ]
)

write.csv(
  data_noise,
  "Dataset/stroke_noise_handled.csv",
  row.names = FALSE
)

invalid_age <- data_noise[
  data_noise$age < min_age |
    data_noise$age > max_age,
]

View(invalid_age)

nrow(invalid_age)


invalid_gender <- data_noise[
  !(data_noise$gender %in% c("Male", "Female", "Other")),
]

View(invalid_gender)

nrow(invalid_gender)


invalid_hyper <- data_noise[
  !(data_noise$hypertension %in% c(0, 1)),
]

View(invalid_hyper)

nrow(invalid_hyper)


invalid_heart <- data_noise[
  !(data_noise$heart_disease %in% c(0, 1)),
]

View(invalid_heart)

nrow(invalid_heart)


invalid_stroke <- data_noise[
  !(data_noise$stroke %in% c(0, 1)),
]

View(invalid_stroke)

nrow(invalid_stroke)


invalid_work_related_stress <- data_noise[
  !(data_noise$work_related_stress %in% c(0, 1)),
]

View(invalid_work_related_stress)

nrow(invalid_work_related_stress)


invalid_urban_residence <- data_noise[
  !(data_noise$urban_residence %in% c(0, 1)),
]

View(invalid_urban_residence)

nrow(invalid_urban_residence)


invalid_smokes <- data_noise[
  !(data_noise$smokes %in% c(0, 1)),
]

View(invalid_smokes)

nrow(invalid_smokes)


invalid_glucose <- data_noise[
  data_noise$avg_glucose_level < 0,
]

View(invalid_glucose)

nrow(invalid_glucose)


invalid_bmi <- data_noise[
  !is.na(data_noise$bmi) &
    data_noise$bmi < 0,
]

View(invalid_bmi)

nrow(invalid_bmi)




data_normalized <- data_noise

min_age <- min(data_noise$age, na.rm = TRUE)
max_age <- max(data_noise$age, na.rm = TRUE)

data_normalized$age <-
  (data_noise$age - min_age) /
  (max_age - min_age)


min_glucose <- min(data_noise$avg_glucose_level, na.rm = TRUE)
max_glucose <- max(data_noise$avg_glucose_level, na.rm = TRUE)

data_normalized$avg_glucose_level <-
  (data_noise$avg_glucose_level - min_glucose) /
  (max_glucose - min_glucose)


min_bmi <- min(data_noise$bmi, na.rm = TRUE)
max_bmi <- max(data_noise$bmi, na.rm = TRUE)

data_normalized$bmi <-
  (data_noise$bmi - min_bmi) /
  (max_bmi - min_bmi)


summary(data_normalized[, c("age",
                            "avg_glucose_level",
                            "bmi")])

range(data_normalized$age, na.rm = TRUE)

range(data_normalized$avg_glucose_level, na.rm = TRUE)

range(data_normalized$bmi, na.rm = TRUE)


write.csv(
  data_normalized,
  "Dataset/stroke_normalized.csv",
  row.names = FALSE
)
View(data_normalized)



data_outlier <- data_normalized

Q1_age <- quantile(data_outlier$age, 0.25, na.rm = TRUE)
Q3_age <- quantile(data_outlier$age, 0.75, na.rm = TRUE)

IQR_age <- IQR(data_outlier$age, na.rm = TRUE)

Lower_age <- Q1_age - 1.5 * IQR_age
Upper_age <- Q3_age + 1.5 * IQR_age

Lower_age
Upper_age

outlier_age <- data_outlier[
  data_outlier$age < Lower_age |
    data_outlier$age > Upper_age,
]

View(outlier_age)

nrow(outlier_age)

boxplot(
  data_outlier$age,
  main = "Age Outlier Detection",
  ylab = "Age"
)


Q1_glucose <- quantile(data_outlier$avg_glucose_level, 0.25, na.rm = TRUE)
Q3_glucose <- quantile(data_outlier$avg_glucose_level, 0.75, na.rm = TRUE)

IQR_glucose <- IQR(data_outlier$avg_glucose_level, na.rm = TRUE)

Lower_glucose <- Q1_glucose - 1.5 * IQR_glucose
Upper_glucose <- Q3_glucose + 1.5 * IQR_glucose

outlier_glucose <- data_outlier[
  data_outlier$avg_glucose_level < Lower_glucose |
    data_outlier$avg_glucose_level > Upper_glucose,
]

View(outlier_glucose)

nrow(outlier_glucose)

boxplot(
  data_outlier$avg_glucose_level,
  main = "Glucose Outlier Detection",
  ylab = "Glucose"
)


Q1_bmi <- quantile(data_outlier$bmi, 0.25, na.rm = TRUE)
Q3_bmi <- quantile(data_outlier$bmi, 0.75, na.rm = TRUE)

IQR_bmi <- IQR(data_outlier$bmi, na.rm = TRUE)

Lower_bmi <- Q1_bmi - 1.5 * IQR_bmi
Upper_bmi <- Q3_bmi + 1.5 * IQR_bmi

outlier_bmi <- data_outlier[
  data_outlier$bmi < Lower_bmi |
    data_outlier$bmi > Upper_bmi,
]

View(outlier_bmi)

nrow(outlier_bmi)

boxplot(
  data_outlier$bmi,
  main = "BMI Outlier Detection",
  ylab = "BMI"
)


plot(
  data_outlier$age,
  main = "Age Scatter Plot",
  xlab = "Observation",
  ylab = "Age",
  pch = 19,
  col = "blue"
)

plot(
  data_outlier$avg_glucose_level,
  main = "Glucose Scatter Plot",
  xlab = "Observation",
  ylab = "Average Glucose Level",
  pch = 19,
  col = "red"
)

plot(
  data_outlier$bmi,
  main = "BMI Scatter Plot",
  xlab = "Observation",
  ylab = "BMI",
  pch = 19,
  col = "darkgreen"
)


nrow(outlier_age)

nrow(outlier_glucose)

nrow(outlier_bmi)


mean(data_noise$age, na.rm = TRUE)

median(data_noise$age, na.rm = TRUE)

sd(data_noise$age, na.rm = TRUE)

var(data_noise$age, na.rm = TRUE)

min(data_noise$age, na.rm = TRUE)

max(data_noise$age, na.rm = TRUE)

range(data_noise$age, na.rm = TRUE)

summary(data_noise$age)




mean(data_noise$avg_glucose_level, na.rm = TRUE)

median(data_noise$avg_glucose_level, na.rm = TRUE)

sd(data_noise$avg_glucose_level, na.rm = TRUE)

var(data_noise$avg_glucose_level, na.rm = TRUE)

min(data_noise$avg_glucose_level, na.rm = TRUE)

max(data_noise$avg_glucose_level, na.rm = TRUE)

range(data_noise$avg_glucose_level, na.rm = TRUE)

summary(data_noise$avg_glucose_level)


mean(data_noise$bmi, na.rm = TRUE)

median(data_noise$bmi, na.rm = TRUE)

sd(data_noise$bmi, na.rm = TRUE)

var(data_noise$bmi, na.rm = TRUE)

min(data_noise$bmi, na.rm = TRUE)

max(data_noise$bmi, na.rm = TRUE)

range(data_noise$bmi, na.rm = TRUE)

summary(data_noise$bmi)


hist(
  data_noise$age,
  main = "Age Distribution",
  xlab = "Age",
  ylab = "Frequency",
  col = "lightblue",
  border = "black"
)


hist(
  data_noise$bmi,
  main = "BMI Distribution",
  xlab = "BMI",
  ylab = "Frequency",
  col = "lightgreen",
  border = "black"
)



table(data_noise$gender)

prop.table(table(data_noise$gender))

names(sort(table(data_noise$gender), decreasing = TRUE))[1]

barplot(
  table(data_noise$gender),
  main = "Gender Distribution",
  xlab = "Gender",
  ylab = "Count",
  col = c("skyblue", "pink", "lightgreen")
)

pie(
  table(data_noise$gender),
  main = "Gender Distribution",
  col = c("skyblue", "pink", "lightgreen")
)


table(data_noise$stroke)

prop.table(table(data_noise$stroke))

names(sort(table(data_noise$stroke), decreasing = TRUE))[1]

barplot(
  table(data_noise$stroke),
  main = "Stroke Distribution",
  xlab = "Stroke",
  ylab = "Count",
  col = c("lightgreen", "tomato")
)

table(data_noise$hypertension)

prop.table(table(data_noise$hypertension))

names(sort(table(data_noise$hypertension), decreasing = TRUE))[1]

barplot(
  table(data_noise$hypertension),
  main = "Hypertension Distribution",
  xlab = "Hypertension",
  ylab = "Count",
  col = c("lightblue", "orange")
)

table(data_noise$heart_disease)

prop.table(table(data_noise$heart_disease))

names(sort(table(data_noise$heart_disease), decreasing = TRUE))[1]

barplot(
  table(data_noise$heart_disease),
  main = "Heart Disease Distribution",
  xlab = "Heart Disease",
  ylab = "Count",
  col = c("lightblue", "orange")
)

table(data_noise$work_related_stress)

prop.table(table(data_noise$work_related_stress))

names(sort(table(data_noise$work_related_stress), decreasing = TRUE))[1]

barplot(
  table(data_noise$work_related_stress),
  main = "Work Related Stress Distribution",
  xlab = "Work Related Stress",
  ylab = "Count",
  col = c("lightblue", "orange")
)

table(data_noise$urban_residence)

prop.table(table(data_noise$urban_residence))

names(sort(table(data_noise$urban_residence), decreasing = TRUE))[1]

barplot(
  table(data_noise$urban_residence),
  main = "Urban Residence Distribution",
  xlab = "Urban Residence",
  ylab = "Count",
  col = c("lightgreen", "pink")
)

pie(
  table(data_noise$urban_residence),
  main = "Urban Residence Distribution",
  labels = c("0", "1"),
  col = c("lightgreen", "pink")
)


table(data_noise$smokes)

prop.table(table(data_noise$smokes))

names(sort(table(data_noise$smokes), decreasing = TRUE))[1]

barplot(
  table(data_noise$smokes),
  main = "Smokes Distribution",
  xlab = "Smokes",
  ylab = "Count",
  col = c("lightblue", "orange")
)

correlation_data <- data_noise[, c(
  "age",
  "avg_glucose_level",
  "bmi"
)]

cor(
  correlation_data,
  use = "complete.obs"
)

pairs(
  correlation_data,
  main = "Scatter Plot Matrix"
)


boxplot(
  age ~ stroke,
  data = data_noise,
  main = "Age vs Stroke",
  xlab = "Stroke",
  ylab = "Age",
  col = c("lightgreen", "tomato")
)

boxplot(
  bmi ~ stroke,
  data = data_noise,
  main = "BMI vs Stroke",
  xlab = "Stroke",
  ylab = "BMI",
  col = c("lightblue", "orange")
)

boxplot(
  avg_glucose_level ~ stroke,
  data = data_noise,
  main = "Average Glucose Level vs Stroke",
  xlab = "Stroke",
  ylab = "Average Glucose Level",
  col = c("lightgreen", "pink")
)

plot(
  data_noise$age,
  data_noise$avg_glucose_level,
  main = "Age vs Average Glucose Level",
  xlab = "Age",
  ylab = "Average Glucose Level",
  pch = 19,
  col = "blue"
)


set.seed(123)

train_index <- createDataPartition(
  data_normalized$stroke,
  p = 0.80,
  list = FALSE
)

train_data <- data_normalized[train_index, ]

test_data <- data_normalized[-train_index, ]

dim(train_data)

dim(test_data)

table(train_data$stroke)

table(test_data$stroke)

write.csv(
  train_data,
  "Dataset/stroke_train.csv",
  row.names = FALSE
)

write.csv(
  test_data,
  "Dataset/stroke_test.csv",
  row.names = FALSE
)

library(caret)

feature_data <- train_data

feature_data$rownames <- NULL

feature_data$pat_id <- NULL

feature_data$stroke <- as.factor(feature_data$stroke)

model <- train(
  stroke ~ .,
  data = feature_data,
  method = "glm",
  family = "binomial"
)

feature_rank <- varImp(model)

feature_rank

plot(feature_rank)

feature_table <- data.frame(
  Feature = rownames(feature_rank$importance),
  Overall = feature_rank$importance$Overall
)

feature_table <- feature_table[
  order(feature_table$Overall, decreasing = TRUE),
]


feature_table

View(feature_table)

head(feature_table, 5)

write.csv(
  feature_table,
  "Dataset/feature_selection.csv",
  row.names = FALSE
)