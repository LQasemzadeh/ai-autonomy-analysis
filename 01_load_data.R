#Load libraries
library(dplyr)

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

data$timestamp_iso <- as.POSIXct(data$timestamp_iso)
# Calculate completion time for completed sessions only
time_summary <- data %>%
  group_by(session_id, mode) %>%
  summarise(
    started = any(action == "TASK_STARTED"),
    completed = any(action == "TASK_COMPLETED"),
    start_time = if (any(action == "TASK_STARTED")) min(timestamp_iso[action == "TASK_STARTED"]) else as.POSIXct(NA),
    end_time = if (any(action == "TASK_COMPLETED")) max(timestamp_iso[action == "TASK_COMPLETED"]) else as.POSIXct(NA),
    .groups = "drop"
  ) %>%
  filter(started == TRUE, completed == TRUE) %>%
  mutate(
    completion_time = as.numeric(difftime(end_time, start_time, units = "secs"))
  )

summary(time_summary$completion_time)

# inspect one completed session
data %>%
  filter(session_id == time_summary$session_id[1]) %>%
  select(session_id, mode, action, timestamp_iso)

data %>%
  filter(session_id == time_summary$session_id[1]) %>%
  select(session_id, mode, action, client_ts_ms)

data$client_ts_sec <- data$client_ts_ms / 1000
head(data$client_ts_sec)

# Calculate completion time for completed sessions only
time_summary <- data %>%
  group_by(session_id, mode) %>%
  summarise(
    started = any(action == "TASK_STARTED"),
    completed = any(action == "TASK_COMPLETED"),
    start_time = if (any(action == "TASK_STARTED")) min(client_ts_sec[action == "TASK_STARTED"]) else NA,
    end_time   = if (any(action == "TASK_COMPLETED")) max(client_ts_sec[action == "TASK_COMPLETED"]) else NA,
    .groups = "drop"
  ) %>%
  filter(started == TRUE, completed == TRUE) %>%
  mutate(
    completion_time = end_time - start_time
  )

summary(time_summary$completion_time)

# compare completion time by mode
time_summary %>%
  group_by(mode) %>%
  summarise(
    avg_time = mean(completion_time),
    median_time = median(completion_time)
  )
time_summary %>%
  arrange(desc(completion_time)) %>%
  head(10)

time_summary_clean <- time_summary %>%
  filter(completion_time <= 300)

time_summary_clean %>%
  group_by(mode) %>%
  summarise(
    avg_time = mean(completion_time),
    median_time = median(completion_time)
  )

# Compare completion time across modes
kruskal.test(completion_time ~ mode, data = time_summary_clean)
pairwise.wilcox.test(time_summary_clean$completion_time, time_summary_clean$mode)

unique(data$action)

# Session-level error & submit flags
error_summary <- data %>%
  group_by(session_id, mode) %>%
  summarise(
    had_error = any(action == "ERROR_SHOWN"),
    had_submit = any(action == "SUBMIT_CLICK"),
    .groups = "drop"
  )

error_summary_clean <- error_summary %>%
  filter(had_submit == TRUE)

error_summary_clean %>%
  group_by(mode) %>%
  summarise(
    error_rate = mean(had_error)
  )

kruskal.test(had_error ~ mode, data = error_summary_clean)

error_table <- table(error_summary_clean$mode, error_summary_clean$had_error)
error_table

pairwise.prop.test(
  x = error_table[, "TRUE"],
  n = rowSums(error_table)
)

# Intervention rate
session_summary %>%
  group_by(mode) %>%
  summarise(
    intervention_rate = mean(had_intervention)
  )

kruskal.test(had_intervention ~ mode, data = session_summary)

intervention_table <- table(session_summary$mode, session_summary$had_intervention)

pairwise.prop.test(
  x = intervention_table[, "TRUE"],
  n = rowSums(intervention_table)
)

# Intervention count
session_summary %>%
  group_by(mode) %>%
  summarise(
    avg_intervention = mean(intervention_count),
    median_intervention = median(intervention_count)
  )

kruskal.test(intervention_count ~ mode, data = session_summary)

pairwise.wilcox.test(
  session_summary$intervention_count,
  session_summary$mode
)
