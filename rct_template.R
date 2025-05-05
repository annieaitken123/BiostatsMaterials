#' @title Load Required Libraries
#' @description Load all necessary libraries for data manipulation, visualization, and modeling.
#' @keywords internal
#' @noRd
# Load Required Packages
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(gtsummary)
library(jsonlite)
library(lubridate)
library(ggpubr)
library(rstatix)
library(ez)
library(emmeans)
library(lcsm)
library(lme4)

#' @title Load and Merge Study Data
#' @description Import metadata, main dataset, group assignments, experimenter and dropout data, and merge them into a unified dataframe.
#' @keywords internal
#' @noRd
meta_data <- fromJSON("path/to/meta_data.json")[c("participant_id", "enrollment_date", "event_name", "event_date")] %>%
  unique(., vars = "participant_id", keep_all = TRUE)

main_data <- read.csv("path/to/data.csv")

group_assignments <- read_excel("path/to/group_assignments.xlsx") %>%
  rename(participant_id = `participant_id`, Arm = `group_column_name`)

experimenter_data <- read_excel("path/to/exp_info.xlsx")
dropout_data <- read_excel("path/to/dropout_info.xlsx")

df <- main_data %>%
  left_join(group_assignments, by = "participant_id") %>%
  left_join(experimenter_data, by = "participant_id") %>%
  left_join(meta_data, by = c("participant_id", "event_name"))

#' @title Calculate Derived Variables
#' @description Create participant-level age variables at enrollment and current time.
#' @keywords internal
#' @noRd
df <- df %>%
  mutate(
    age_in_months_current = interval(dob, Sys.Date()) %/% months(1),
    age_in_months_at_enroll = interval(dob, enrollment_date) %/% months(1)
  )

#' @title Filter ITT and PP Datasets
#' @description Generate analysis-ready datasets for intention-to-treat (ITT) and per-protocol (PP) participants.
#' @keywords internal
#' @noRd
df_itt <- df %>% filter(ITT_flag == 1)
df_pp <- df %>% filter(PP_flag == 1)

#' @title Baseline Demographics Summary
#' @description Summary statistics of baseline demographics by treatment arm.
#' @keywords internal
#' @noRd
df_itt %>%
  filter(event_name == "Baseline") %>%
  select(Arm, age_in_months_at_enroll, sex, race, ethnicity) %>%
  tbl_summary(by = "Arm", statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_p()

#' @title Baseline Outcome Measures Summary
#' @description Summary statistics of baseline scores for outcome measures by treatment arm.
#' @keywords internal
#' @noRd
df_itt %>%
  filter(event_name == "Baseline") %>%
  select(Arm, starts_with("baseline_measure")) %>%
  tbl_summary(by = "Arm", statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  add_p()

#' @title Experimenter Characteristics
#' @description Distribution of experimenter variables at baseline by group.
#' @keywords internal
#' @noRd
df %>%
  filter(event_name == "Baseline") %>%
  select(Arm, exp_col) %>%
  tbl_summary(by = "Arm") %>%
  add_p()

#' @title Adverse Event Reporting Summary
#' @description Summarize adverse event reporting after baseline across arms.
#' @keywords internal
#' @noRd
df %>%
  filter(!event_name %in% c("Screening", "Baseline")) %>%
  drop_na(adverse_event_flag) %>%
  group_by(participant_id) %>%
  tbl_summary(by = "Arm")

#' @title VR Session Descriptives
#' @description Filter and summarize time spent per session by arm.
#' @keywords internal
#' @noRd
df_ISD <- df %>%
  select(participant_id, event_name, event_date, Arm, time_spent_column) %>%
  drop_na(time_spent_column)

df_ISD %>%
  tbl_summary(by = "Arm", statistic = list(all_continuous() ~ "{mean} ({sd})"))

#' @title Clean and Aggregate Session Time Data
#' @description Convert session time to numeric, calculate averages and session counts per arm.
#' @keywords internal
#' @noRd
df_ISD_summary <- df_ISD %>%
  mutate(cleaned_time = ms(gsub("[^0-9:]", ":", time_spent_column))) %>%
  group_by(participant_id, Arm) %>%
  summarise(
    avg_time = seconds_to_period(mean(lubridate::seconds(cleaned_time), na.rm = TRUE)),
    session_count = n()
  ) %>%
  group_by(Arm) %>%
  summarise(
    mean_time = mean(lubridate::seconds(avg_time), na.rm = TRUE),
    sd_time = sd(lubridate::seconds(avg_time), na.rm = TRUE),
    mean_sessions = mean(session_count),
    sd_sessions = sd(session_count)
  )

#' @title Pre-Post Summary Tables
#' @description Generate longitudinal descriptive summaries for outcome measures.
#' @keywords internal
#' @noRd
df_pre_post <- df %>%
  filter(event_name %in% c("Baseline", "End of Study")) %>%
  select(participant_id, Arm, event_name, starts_with("outcome_measure"))

tbl_strata(
  data = df_pre_post,
  strata = Arm,
  .tbl_fun = ~ .x %>%
    tbl_summary(by = event_name,
                type = all_continuous() ~ "continuous",
                statistic = list(all_continuous() ~ "{mean} ({sd})"))
)

#' @title Export-Ready Summary (Means Only)
#' @description Summarize pre/post means without standard deviation, for use in reports or Excel.
#' @keywords internal
#' @noRd
tbl_strata(
  data = df_pre_post,
  strata = Arm,
  .tbl_fun = ~ .x %>%
    tbl_summary(by = event_name,
                statistic = list(all_continuous() ~ "{mean}"))
)

#' Plot Trajectories by Arm
#'
#' @description Generate a trajectory plot for a given outcome variable, split by group.
#'
#' @param data A dataframe with participant_id, Arm, and event-specific columns
#' @param variable_name The base name of the variable to plot (e.g., "outcome_score")
#'
#' @return A ggplot object arranged by group arms.
#' @export
plot_trajectory_variable <- function(data, variable_name) {
  arms <- unique(data$Arm)
  plots <- lapply(arms, function(arm) {
    data_filtered <- data %>% filter(Arm == arm)
    plot_trajectories(
      data_filtered, id_var = "participant_id",
      var_list = paste0(variable_name, c("_Baseline", "_End of Study")),
      ylab = variable_name, xlab = paste("Group", arm),
      line_colour = ifelse(arm == "A", "#0073C2FF", "#EFC000FF"),
      point_colour = ifelse(arm == "A", "#0073C2FF", "#EFC000FF")
    ) + scale_x_discrete(labels = c("Baseline", "EOS"))
  })
  ggarrange(plotlist = plots, ncol = length(arms), nrow = 1, common.legend = TRUE)
}

#' @title Repeated-Measures ANOVA
#' @description Conduct repeated-measures ANOVA and post-hoc comparisons using `emmeans`.
#' @keywords internal
#' @noRd
aov_result <- aov(outcome_var ~ Arm * event_name + Error(participant_id), data = df_pre_post)
summary(aov_result)
emmeans(aov_result, pairwise ~ event_name | Arm, adjust = "bonferroni")
