data <- read.csv("data/study_logs_rows.csv", stringsAsFactors = FALSE)

head(data)
str(data)
summary(data)
names(data)

# Explore modes and actions
unique(data$mode)
unique(data$action)
table(data$action)

table(data$action[data$mode == "Information"])
# remove non-task mode
data <- subset(data, mode != "Information")

table(data$mode)