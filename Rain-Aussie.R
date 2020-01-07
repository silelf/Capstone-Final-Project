# This R script is a submission for the Capstone Project - Choose Your Own
# for the edX Data Science course PH125.9x.
#
# The aim of the project is to fit a machine learning binary classification
# model to a weather dataset which will predict whether or not it will rain
# on the following day.
#
# The weather dataset used is 'Rain in Australia' which is available on the
# Kaggle website.
#
# 1/7/2020 - Silvia Elaluf-Calderwood
#

# Load the required libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(RSNNS)) install.packages("RSNNS", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

# Function for encoding character columns as numeric
EncodeVec <- function(v) {
  v1 <- as.factor(v)
  levels(v1) <- 1:length(levels(v1))
  v1 <- as.numeric(v1)
  return(v1)
}

# Read in the data from CSV file
dfWeather <- read.csv(file="weatherAUS.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


# INITIAL EXPLORATORY ANALYSIS 

# Display the structure and dimensions of the data frame
str(dfWeather)

# Create a table of locations and number of observations for each
table(dfWeather$Location)
num_loc <- length(unique(dfWeather$Location))
paste("Number of locations:", num_loc)

# Plot number of observations for each location
ggplot(data = dfWeather, mapping = aes(x=Location)) +
  geom_bar(fill = "darkgreen", width = 0.8) +
  labs(title = "Number of Observations by Location") +
  labs(x = "Location") +
  labs(y = "# Observations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  theme(axis.text.x = element_text(angle = 90))

# Display the date range of measurements in the dataset
paste(min(dfWeather$Date), "to", max(dfWeather$Date))


#CHECK FOR MISSING DATA #


# Check columns first, display in order of decreasing number of NAs
sort(sapply(dfWeather, function(col) {sum(is.na(col))}), decreasing = TRUE)

# Display number of rows that would remain if columns with many NAs were removed
nrow(dfWeather[complete.cases(select(dfWeather, -(Evaporation:Sunshine), -(Cloud9am:Cloud3pm))),])


# DATA CLEANING #


# Remove the following columns:
#   Date         - not needed as an input
#   Evaporation  - too many NAs
#   Sunshine     - too many NAs
#   Cloud9am     - too many NAs
#   Cloud3pm     - too many NAs
#   RISK_MM      - used originally to derive RainTomorrow in dataset
dfWeather$Date <- NULL
dfWeather$Evaporation <- NULL
dfWeather$Sunshine <- NULL
dfWeather$Cloud9am <- NULL
dfWeather$Cloud3pm <- NULL
dfWeather$RISK_MM <- NULL

# Remove rows with missing data
dfWeather <- dfWeather[complete.cases(dfWeather),]

# Convert character columns to numeric
dfWeather$WindGustDir <- EncodeVec(dfWeather$WindGustDir)
dfWeather$WindDir9am <- EncodeVec(dfWeather$WindDir9am)
dfWeather$WindDir3pm <- EncodeVec(dfWeather$WindDir3pm)

dfWeather$RainToday<-str_replace_all(dfWeather$RainToday,"No","0")
dfWeather$RainToday<-str_replace_all(dfWeather$RainToday,"Yes","1")
dfWeather$RainTomorrow<-str_replace_all(dfWeather$RainTomorrow,"No","0")
dfWeather$RainTomorrow<-str_replace_all(dfWeather$RainTomorrow,"Yes","1")
dfWeather$RainToday <- as.numeric(dfWeather$RainToday)
dfWeather$RainTomorrow <- as.factor(dfWeather$RainTomorrow)

# Display the dimensions of the resulting dataset
paste("No. of rows (observations):", nrow(dfWeather))
paste("No. of columns (variables):", ncol(dfWeather))


# EXPLORATORY ANALYSIS OF CLEANED DATA #


# Display the structure of the cleaned data
str(dfWeather)

# Display summary statistics for the cleaned data
summary(dfWeather)

# Plot histogram for Temp3pm - 1st example of frequency didtribution
ggplot(dfWeather, aes(Temp3pm)) +
  geom_histogram(bins=16, fill="lightblue", color="black")
# Plot histogram for Pressure3pm- 2nd example of frequency distribution
ggplot(dfWeather, aes(Pressure3pm)) +
  geom_histogram(bins=16, fill="lightblue", color="black")

# Boxplot of Humidity3pm - 1st example of variability across locations
ggplot(dfWeather, aes(x=Location, y=Humidity3pm, fill=Location)) +
  geom_boxplot(outlier.shape=19, outlier.alpha=0.2) +
  labs(title="Relative Humidity at 3 pm by Location") +
  labs(y="Humidity (%)") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position="none")
# Boxplot of Rainfall - 2nd example of variability across locations
ggplot(dfWeather, aes(x=Location, y=Rainfall, fill=Location)) +
  geom_boxplot(outlier.shape=19, outlier.alpha=0.2) +
  labs(title="Daily Rainfall by Location") +
  labs(y="Rainfall (mm)") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position="none")

# Plot number of observations for each location
ggplot(data = dfWeather, mapping = aes(x=Location)) +
  geom_bar(fill = "darkgreen", width = 0.8) +
  labs(title = "Number of Observations by Location") +
  labs(x = "Location") +
  labs(y = "# Observations") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  theme(axis.text.x = element_text(angle = 90))

# Display change in number of locations in data due to cleaning
new_num_loc <- length(unique(dfWeather$Location))
paste("Number of locations originally:", num_loc)
paste("Number of locations remaining :", new_num_loc)

# Calculate and plot average rainfall by location
mean_rain <- dfWeather %>%
  group_by(Location) %>%
  summarize(mean = mean(Rainfall))
mean_rain <- as.data.frame(mean_rain)
ggplot(data = mean_rain, mapping = aes(x=Location, y=mean)) +
  geom_col(fill = "blue", width = 0.8) +
  labs(title = "Average Rainfall by Location") +
  labs(x = "Location") +
  labs(y = "Average Rainfall (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  theme(axis.text.x = element_text(angle = 90))

# Summarize rainy days by location
by_location <- group_by(dfWeather, Location)
s1 <- summarize(by_location,
                `Rainy Days`=sum(RainToday),
                `Total Days`=n(),
                `% Rainy Days`=round(sum(RainToday)/n()*100,1))
print(as.data.frame(s1))

paste("Total days in the dataset:", nrow(dfWeather))
paste("Number of rainy days:     ", sum(dfWeather$RainToday))
paste("Number of fine days:      ", nrow(dfWeather) - sum(dfWeather$RainToday))

# Calculate and plot percentage of rainy days by location
days_rain <- dfWeather %>%
  group_by(Location) %>%
  summarize(percent = 100*sum(RainToday)/n())
days_rain <- as.data.frame(days_rain)
ggplot(data = days_rain, mapping = aes(x=Location, y=percent)) +
  geom_col(fill = "coral", width = 0.8) +
  labs(title = "% Days with Rain by Location") +
  labs(x = "Location") +
  labs(y = "% Days") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  theme(axis.text.x = element_text(angle = 90))

# Calculate and plot percentage of rainy days overall
s2 <- summarize(dfWeather,
                Rain=round(sum(RainToday)/n()*100,1),
                `No Rain`=round((n()-sum(RainToday))/n()*100,1))
s2 <- as.data.frame(t(s2))
names(s2) <- "Percent"
ggplot(s2,aes(x=c("Rain", "No Rain"), y=Percent, color=Percent)) +
  geom_col(fill=c("darkgreen", "Red"), width=0.5) +
  geom_text(aes(label=Percent, vjust=-0.5)) +
  ylim(0,100) +
  labs(title = "% Days With and Without Rain - All Locations") +
  labs(x = "") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position="none")

# Calculate and plot correlations between variables
M <- cor(dfWeather[,2:17])
corrplot(M, method = "circle")

# Perform principal component analysis and report results
pca <- prcomp(dfWeather[,2:17])
summary(pca)
var_expl <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_expl,
     main="Cumulative Proportion of Variance from Principal Components",
     xlab="Components",
     ylab="Proportion")

# Correlation between rain today and rain tomorrow
r <- round(cor(dfWeather$RainToday, as.numeric(dfWeather$RainTomorrow)),2)
paste("Correlation coefficient:", r)


# FITTING THE MODELS AND PREDICTION #


# Scale the data
maxs <- apply(dfWeather[,2:16], 2, max)
mins <- apply(dfWeather[,2:16], 2, min)
dfScaled <- as.data.frame(scale(dfWeather[,2:16], center = mins, scale = maxs-mins))
dfScaled <- cbind(dfScaled, dfWeather[,17:18])

# Create datasets for training and testing
set.seed(1)
test_index <- createDataPartition(y=dfScaled$RainTomorrow, times=1, p=0.2, list=FALSE)
dfTrain <- dfScaled[-test_index,]
dfTest <- dfScaled[test_index,]
rm(dfScaled)

# Train the models
models <- c("glm", "lda", "qda", "mlp", "knn", "rf")
t0 <- proc.time()
fits <- lapply(models, function(model) {
  caret::train(RainTomorrow ~ ., data = dfTrain, method = model)
})

t1 <- proc.time()
names(fits) <- models

# Perform prediction with each of the models
y_hats <- sapply(fits, function(fit) {
  y_hat <- predict(fit, dfTest)
})
t2 <- proc.time()

# Calculate the accuracy of the models and format as a table
accuracy <- colMeans(y_hats == dfTest$RainTomorrow)
accuracy <- sort(round(accuracy, 4), decreasing = TRUE)
accuracy <- cbind(names(accuracy), accuracy)
row.names(accuracy) <- NULL
acc_tbl <- kable(accuracy, col.names = c("Model", "Accuracy"))

# Display times taken for training and predicting
paste("Elapsed time for training:  ", round((t1-t0)[3], 0), "seconds")
paste("Elapsed time for predicting:", round((t2-t1)[3], 0), "seconds")


# REPORTING RESULTS #


# Display the accuracy obtained by each of the models
acc_tbl

# Prepare a table of varaible importance, sorted into descending order
imp <- as.data.frame(importance(fits[[6]]$finalModel))
imp <- cbind(rownames(imp), imp)
names(imp) <- c("Variable", "Importance")
imp <- imp[order(-imp$Importance),]
row.names(imp) <- NULL
kable(imp)

# Display and plot details of the random forest model
print(fits[[6]])
plot(fits[[6]])

# Display the confusion matrix and related statistics for the random forest model
y_hat <- as.factor(y_hats[,6])
print(caret::confusionMatrix(y_hat, dfTest$RainTomorrow, positive="1"))

# Visualization of confusion matrix using boxplot with jitter
y_hat <- as.factor(y_hats[,6])
ggplot(mapping=aes(x=dfTest$RainTomorrow, y=fct_rev(y_hat), color=dfTest$RainTomorrow)) +
  geom_boxplot(outlier.size=0.25) +
  geom_jitter() + 
  labs(title="Confusion Matrix Plot for 'Rain Tomorrow'") +
  labs(x="Observed") +
  labs(y="Predicted") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme(legend.position="none")

