# Set up and create the data

set.seed(123)
Taran <- c("Sujal", "Lavish", "Viru", "Taran")
steps_data <- data.frame(
  Name = Taran,
  replicate(30, sample(1000:15000, 4, replace = TRUE))
)
colnames(steps_data)[2:31] <- paste0("Day_", 1:30)


# Introduce some NA values manually

steps_data[2, 5] <- NA   # Sujal, Day_4
steps_data[3, 10] <- NA  # Lavish, Day_9
steps_data[4, 15] <- NA  # Viru, Day_14
steps_data[1, 20] <- NA  # Taran, Day_19

# View dataset with missing values
View(steps_data)


# Step 1: Detect Missing Values

# How many NAs in each row?
rowSums(is.na(steps_data))

# How many NAs in total?
sum(is.na(steps_data))


# Step 2: Recalculate Statistics

steps_data$Total <- rowSums(steps_data[, 2:31])
steps_data$Average <- rowMeans(steps_data[, 2:31])
steps_data$Median <- apply(steps_data[, 2:31], 1, median)
steps_data$StdDev <- apply(steps_data[, 2:31], 1, sd)


# Step 3: Create Visualizations


# Line Plot
matplot(t(steps_data[, 2:31]), type = "l", lty = 1, lwd = 2,
        col = 1:4, xlab = "Day", ylab = "Steps", main = "Daily Step Trends")
legend("topright", legend = steps_data$Name, col = 1:4, lty = 1)

# Barplot of Averages
barplot(steps_data$Average, names.arg = steps_data$Name,
        main = "Average Steps per User", col = "lightgreen")

# Histogram of all step values
hist(unlist(steps_data[, 2:31]), breaks = 20, col = "skyblue",
     main = "Distribution of Step Counts", xlab = "Steps")


# Step 4: Categorize Activity Level

steps_data$Activity_Level <- cut(steps_data$Average,
                                 breaks = c(0, 5000, 10000, Inf),
                                 labels = c("Low", "Moderate", "Active"))


# Step 5: View Final Dataset and Insights

View(steps_data)

# Most consistent walker
steps_data[which.min(steps_data$StdDev), c("Name", "StdDev")]

# Most active walker
steps_data[which.max(steps_data$Average), c("Name", "Average")]

