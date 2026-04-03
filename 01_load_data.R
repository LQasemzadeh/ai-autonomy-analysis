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

# Check task validity
table(data$action == "TASK_STARTED")
table(data$action == "TASK_COMPLETED")

library(dplyr)

# build session-level summary
session_summary <- data %>%
  group_by(session_id, mode) %>%
  summarise(
    started = any(action == "TASK_STARTED"),
    completed = any(action == "TASK_COMPLETED"),
    error_count = sum(action == "ERROR_SHOWN"),
    field_edit_count = sum(action == "FIELD_EDIT"),
    override_count = sum(action == "OVERRIDE"),
    rejected_count = sum(action == "AI_SUGGESTION_REJECTED"),
    intervention_count = field_edit_count + override_count + rejected_count,
    had_intervention = intervention_count > 0,
    abandoned = started & !completed,
    .groups = "drop"
  )
head(session_summary)

# check abandonment by mode
session_summary %>%
  group_by(mode) %>%
  summarise(
    total_sessions = n(),
    abandoned_sessions = sum(abandoned),
    abandonment_rate = abandoned_sessions / total_sessions
  )
table_abandon <- table(session_summary$mode, session_summary$abandoned)

chisq.test(table_abandon)

# Count session outcomes
session_summary %>%
  summarise(
    started_and_completed = sum(started & completed),
    started_not_completed = sum(started & !completed),
    not_started = sum(!started)
  )

# Session outcome by mode
session_summary %>%
  group_by(mode) %>%
  summarise(
    started_and_completed = sum(started & completed),
    started_not_completed = sum(started & !completed),
    not_started = sum(!started),
    abandonment_rate = started_not_completed / (started_and_completed + started_not_completed)
  )

session_summary %>%
  group_by(mode) %>%
  summarise(
    intervention_rate = mean(had_intervention),
    avg_intervention_count = mean(intervention_count)
  )
