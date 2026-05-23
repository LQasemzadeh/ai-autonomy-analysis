library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

# ── Data ──────────────────────────────────────────────────────────────────────

logs       <- read_csv("data/study_logs_rows.csv", show_col_types = FALSE)
data_clean <- logs |>
  filter(mode != "Information") |>
  mutate(client_ts_sec = client_ts_ms / 1000)

mode_levels  <- c("manual", "Assistance", "Execution")
mode_palette <- c(manual = "#8DA0CB", Assistance = "#66C2A5", Execution = "#FC8D62")

# ── CSS ───────────────────────────────────────────────────────────────────────

dashboard_css <- "
  body { padding-top: 70px; }

  /* Open nav_menu dropdown on hover instead of click */
  .navbar .nav-item.dropdown:hover > .dropdown-menu {
    display: block;
    margin-top: 0;
  }
  .navbar .nav-item.dropdown > .dropdown-toggle:active {
    pointer-events: none;
  }

  .overview-title {
    font-size: clamp(1.1rem, 2.5vw, 1.5rem);
    font-weight: 700;
    line-height: 1.35;
    color: #1a1a2e;
    margin-bottom: 0.6rem;
  }
  .overview-subtitle {
    font-size: 1rem;
    color: #555;
    margin-bottom: 0.4rem;
  }
  .overview-note {
    font-size: 0.9rem;
    color: #777;
    margin-bottom: 1.5rem;
  }

  .table-section {
    margin-bottom: 1.5rem;
    display: flex;
    flex-direction: column;
    align-items: center;
  }
  .table-caption {
    font-size: 0.85rem;
    color: #555;
    font-style: italic;
    margin-bottom: 0.4rem;
    text-align: center;
  }
  .table-wrap { display: flex; justify-content: center; }
  .table-wrap table {
    border-collapse: collapse;
    font-size: 0.9rem;
    min-width: 260px;
  }
  .table-wrap th {
    border-top: 1px solid #333;
    border-bottom: 1px solid #333;
    padding: 6px 20px;
    text-align: left;
    background: #f8f9fa;
  }
  .table-wrap td { padding: 6px 20px; border-bottom: none; }
  .table-wrap tr:last-child td { border-bottom: 1px solid #333; }

  .plot-wrap { width: 100%; }
  .plot-wrap .shiny-plot-output {
    width: 100% !important;
    height: 320px;
  }
  @media (min-width: 768px) {
    .plot-wrap .shiny-plot-output { height: 380px; }
  }
  @media (min-width: 1200px) {
    .plot-wrap .shiny-plot-output { height: 420px; }
  }

  @media (min-width: 768px) {
    .card-equal { height: 100%; }
  }

  .card-grid {
    display: grid;
    gap: 1rem;
    grid-template-columns: 1fr;
  }
  @media (min-width: 768px) {
    .card-grid { grid-template-columns: repeat(2, 1fr); }
  }

  .page-content { padding: 1.5rem 1rem; }
  @media (min-width: 768px) {
    .page-content { padding: 2rem 1.5rem; }
  }
"

# ── Helpers ───────────────────────────────────────────────────────────────────

plot_card <- function(header, plot_id) {
  card(
    class = "card-equal",
    full_screen = TRUE,
    card_header(header),
    div(class = "plot-wrap", plotOutput(plot_id))
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title    = "AI Execution Autonomy",
  position = "fixed-top",
  theme    = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  ),
  fillable = FALSE,
  header   = tags$head(tags$style(HTML(dashboard_css))),
  nav_spacer(),
  nav_item(
    tags$a(
      href   = "https://lqasemzadeh.github.io/ai-autonomy-dashboard/",
      target = "_blank",
      class  = "btn btn-sm btn-outline-light",
      style  = "margin-right: 0.5rem;",
      "\u25b6\u00a0 Open Interactive Dashboard"
    )
  ),

  # ── Overview ────────────────────────────────────────────────────────────
  nav_panel(
    "Overview",
    div(
      class = "container-fluid page-content",

      p(class = "overview-title",
        "The Impact of AI Execution Autonomy on User Task Performance",
        "and Intervention Behavior in Digital Workflows"),
      p(class = "overview-subtitle",
        "Between-subjects study comparing three AI autonomy conditions."),
      p(class = "overview-note",
        "Behavioral data were captured through system-generated event logs,",
        " recording user actions (e.g. TASK_STARTED, TASK_COMPLETED, ERROR_SHOWN,",
        " FIELD_EDIT, OVERRIDE, AI_SUGGESTION_REJECTED), client-side timestamps,",
        " session identifiers, and assigned autonomy conditions."),

      div(
        class = "table-section",
        p(class = "table-caption",
          "Number of participants per condition after data cleaning",
          " (excluding Information condition)"),
        div(class = "table-wrap", tableOutput("table3"))
      ),

      div(
        class = "card-grid",
        plot_card("Figure 1. Overall Task Progression",                     "plot_task_progress"),
        plot_card("Figure 2. Task Outcomes by Condition (Started Sessions)", "plot_outcomes")
      )
    )
  ),

  # ── H1: Task Performance ─────────────────────────────────────────────────
  nav_menu(
    "H1: Task Performance",

    nav_panel(
      "Completion Time",
      div(class = "container-fluid page-content",
          p(class = "text-muted", "Completion time results."))
    ),

    nav_panel(
      "Detected Errors",
      div(class = "container-fluid page-content",
          p(class = "text-muted", "Detected errors results."))
    ),

    nav_panel(
      "Task Abandonment",
      div(class = "container-fluid page-content",
          p(class = "text-muted", "Task abandonment results."))
    )
  ),

  # ── H2: Intervention Behavior ────────────────────────────────────────────
  nav_menu(
    "H2: Intervention Behavior",

    nav_panel(
      "Intervention Occurrence",
      div(class = "container-fluid page-content",
          p(class = "text-muted", "Intervention occurrence results."))
    ),

    nav_panel(
      "Intervention Count",
      div(class = "container-fluid page-content",
          p(class = "text-muted", "Intervention count results."))
    ),

    nav_panel(
      "Type of Intervention",
      div(class = "container-fluid page-content",
          p(class = "text-muted", "Type of intervention results."))
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Table 3
  participant_table_clean <- data_clean |>
    distinct(session_id, mode) |>
    count(mode, name = "Participants") |>
    rename(Condition = mode) |>
    mutate(Condition = recode(Condition,
      manual     = "Manual",
      Assistance = "Assistance",
      Execution  = "Execution"
    ))

  table3_df <- bind_rows(
    tibble(Condition = "Total",
           Participants = sum(participant_table_clean$Participants)),
    participant_table_clean
  )

  output$table3 <- renderTable(
    table3_df,
    striped  = FALSE,
    bordered = FALSE,
    hover    = FALSE,
    width    = "auto",
    align    = "lr",
    rownames = FALSE
  )

  # Session-level summaries
  session_summary <- data_clean |>
    group_by(session_id, mode) |>
    summarise(
      started   = any(action == "TASK_STARTED"),
      completed = any(action == "TASK_COMPLETED"),
      abandoned = started & !completed,
      .groups = "drop"
    ) |>
    mutate(mode = factor(mode, levels = mode_levels))

  started_only <- session_summary |> filter(started)

  completion_rates <- started_only |>
    group_by(mode) |>
    summarise(rate = mean(completed), n = n(), .groups = "drop")

  # Figure 1 data
  task_progress_overall <- data_clean |>
    group_by(session_id) |>
    summarise(
      started   = any(action == "TASK_STARTED"),
      completed = any(action == "TASK_COMPLETED"),
      .groups = "drop"
    ) |>
    mutate(
      status = case_when(
        !started             ~ "Did not start",
        started & !completed ~ "Started, not completed",
        TRUE                 ~ "Started and completed"
      ),
      status = factor(status, levels = c(
        "Did not start", "Started, not completed", "Started and completed"
      ))
    ) |>
    count(status)

  # Figure 2 data
  task_outcome_by_mode <- data_clean |>
    group_by(session_id, mode) |>
    summarise(
      started   = any(action == "TASK_STARTED"),
      completed = any(action == "TASK_COMPLETED"),
      .groups = "drop"
    ) |>
    filter(started) |>
    mutate(
      outcome = if_else(completed, "Completed", "Abandoned"),
      mode    = factor(mode, levels = mode_levels),
      outcome = factor(outcome, levels = c("Abandoned", "Completed"))
    ) |>
    count(mode, outcome)

  # Figure 1
  output$plot_task_progress <- renderPlot({
    ggplot(task_progress_overall, aes(x = status, y = n, fill = status)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = n), vjust = -0.4, size = 4) +
      scale_fill_manual(values = c(
        "Did not start"          = "#8DA0CB",
        "Started, not completed" = "#FC8D62",
        "Started and completed"  = "#66C2A5"
      )) +
      labs(x = NULL, y = "Participants") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 10, hjust = 1))
  }, res = 96)

  # Figure 2
  output$plot_outcomes <- renderPlot({
    ggplot(task_outcome_by_mode, aes(x = mode, y = n, fill = outcome)) +
      geom_col(position = position_dodge(0.75), width = 0.65) +
      geom_text(aes(label = n),
                position = position_dodge(0.75),
                vjust = -0.4, size = 4) +
      scale_fill_manual(values = c(Abandoned = "#FC8D62", Completed = "#66C2A5")) +
      scale_x_discrete(labels = c(manual = "Manual",
                                  Assistance = "Assistance",
                                  Execution  = "Execution")) +
      labs(x = NULL, y = "Participants", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  }, res = 96)
}

shinyApp(ui, server)
