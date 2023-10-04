library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)


dat <- read_xlsx("datasheet_example.xlsx")

# Populate missing values in "scheduled_start" column
dat$scheduled_start <- na.locf(dat$scheduled_start)
# Format scheduled start
dat$scheduled_start <- as.POSIXct(dat$scheduled_start, format="%Y-%M-%D %H:%M:%S", tz="CET")

# Populate missing values in "abbreviations" column based on values already associated with "intervention" column but not filled throughout
dat <- dat %>% 
  group_by(intervention) %>% 
  fill(abbreviations, .direction = "down")


df <- dat
df$category <- as.factor(df$category)
# df$category <- factor(df$category, levels = c("")) # here you can set the order of appearance of the categories

# If abbreviation field is missing, then use the full interventation name. The "intervention_label" column contains the labels that will be plotted on the graph.
df$intervention_label <- ifelse(is.na(df$abbreviations), df$intervention, df$abbreviations)


# Move scheduled starts to 1 day after, if time is after midnight (or in our case, before 7am to distinguish them from first interventions, as we have no day to distinguish)
df$scheduled_start <- do.call("c", lapply(df$scheduled_start, FUN = function(x) if(x < as.POSIXct("1899-12-31 07:00:00")) x + 3600*24 else(x))) # in our example datasheet dates are set by default by Excel to 1899

# Create and populate Intervention scheduled time end values
df$average_time_end <- as.POSIXct(df$scheduled_start)
df$average_time_end <- df$average_time_end + df$average_duration*60 ## POSIXct times only the addition in units of seconds.
df$max_time_end <- as.POSIXct(df$scheduled_start)
df$max_time_end <- df$max_time_end + df$max_duration*60

df$min_time_end <- as.POSIXct(df$scheduled_start)
df$min_time_end <- df$min_time_end + df$min_duration*60


df$average_cumulative_start <- NA
df$average_cumulative_end <- NA
df$max_cumulative_start <- NA
df$max_cumulative_end <- NA
df$min_cumulative_start <- NA
df$min_cumulative_end <- NA


# Loop to compute consecutive (cumulative) time schedules of interventions grouped by category of intervention.
# This is necessary in the case of needing to plot the activities consecutively, one after the completion of the other, and not
# only based on the "scheduled_start", as that would lump them together. To do this in an automated way per category, we need
# to parse each category and subgroup the activities by them, then parse each interventation by row to computed the actual cumulative
# schedule based on the previous row (within category). We do this three times, per intervation duration type: average_duration,
# max_duration, and min_duration.
all_categories_sdf <- list()
for(category in unique(df$category)){
  sdf <- df[df$category == category,]
  for(row in 1:nrow(sdf)){
    sched_start <- sdf[row, "scheduled_start"]
    
    if(row==1){
      sdf[1,'average_cumulative_start'] <- sdf[1, "scheduled_start"]
      sdf[1,'average_cumulative_end'] <- sdf[1, "scheduled_start"] + sdf[1,"average_duration"]*60
      sdf[1,'max_cumulative_start'] <- sdf[1, "scheduled_start"]
      sdf[1,'max_cumulative_end'] <- sdf[1, "scheduled_start"] + sdf[1,"max_duration"]*60
      sdf[1,'min_cumulative_start'] <- sdf[1, "scheduled_start"]
      sdf[1,'min_cumulative_end'] <- sdf[1, "scheduled_start"] + sdf[1,"min_duration"]*60
    } else{
      if(sched_start == sdf[row-1, "scheduled_start"]){
        sdf[row,'average_cumulative_start'] <- sdf[row-1, "average_cumulative_end"] + 60 # we add 60 seconds to add some white space between bars in graph, purely graphical
        sdf[row,'average_cumulative_end'] <- sdf[row, "average_cumulative_start"] + sdf[row,"average_duration"]*60
        sdf[row,'max_cumulative_start'] <- sdf[row-1, "max_cumulative_end"] + 60
        sdf[row,'max_cumulative_end'] <- sdf[row, "max_cumulative_start"] + sdf[row,"max_duration"]*60
        sdf[row,'min_cumulative_start'] <- sdf[row-1, "min_cumulative_end"] + 60
        sdf[row,'min_cumulative_end'] <- sdf[row, "min_cumulative_start"] + sdf[row,"min_duration"]*60
      } else{
        sdf[row,'average_cumulative_start'] <- sdf[row, "scheduled_start"]
        sdf[row,'average_cumulative_end'] <- sdf[row, "scheduled_start"] + sdf[row,"average_duration"]*60
        sdf[row,'max_cumulative_start'] <- sdf[row, "scheduled_start"]
        sdf[row,'max_cumulative_end'] <- sdf[row, "scheduled_start"] + sdf[row,"max_duration"]*60
        sdf[row,'min_cumulative_start'] <- sdf[row, "scheduled_start"]
        sdf[row,'min_cumulative_end'] <- sdf[row, "scheduled_start"] + sdf[row,"min_duration"]*60
      }
    }
  }
  all_categories_sdf <- rbind(all_categories_sdf, sdf)
}

# Loading most distinct colours palette
source("succession_of_N_most_distinct_colours.R")

df <- all_categories_sdf

# Colouring by activity category
colors <- get_colours(length(unique(df$category)))
names(colors) = unique(df$category)
df$colors = colors[as.character(df$category)]

library(vistime)
library(ggplot2)
library(cowplot)

time_step_axis <- 3600
pdf_height <- 8
pdf_width <- 45

df_anticipated <- df


plot_avg <- gg_vistime(df_anticipated, col.start = "average_cumulative_start", col.end = "average_cumulative_end", ## Once fixed, change both to cumulative_time_start/end
                    col.event = "intervention_label", linewidth = 6,
                    col.color = "colors",
                    col.group = "category", # to define as type of intervention
                    title = "Time schedule of activities",
                    optimize_y = T, show_labels = T) +
  theme_bw() +
  scale_x_datetime(breaks = seq(min(as.POSIXct(df_anticipated$average_cumulative_start)),
                                max(as.POSIXct(df_anticipated$average_cumulative_end))+1800, by=time_step_axis), date_labels ="%H:%M")

ggsave2("cumulative_average_time_activity_timeschedule.pdf", plot_avg,  device = "pdf", width = pdf_width, height = pdf_height)


plot_max <- gg_vistime(df_anticipated, col.start = "max_cumulative_start", col.end = "max_cumulative_end", ## Once fixed, change both to cumulative_time_start/end
                    col.event = "intervention_label", linewidth = 6,
                    col.color = "colors",
                    col.group = "category", # to define as type of intervention
                    title = "Time schedule of activities",
                    optimize_y = T, show_labels = T) +
  theme_bw() +
  scale_x_datetime(breaks = seq(min(as.POSIXct(df_anticipated$max_cumulative_start)),
                                max(as.POSIXct(df_anticipated$max_cumulative_end))+1800, by=time_step_axis), date_labels ="%H:%M") 

ggsave2("cumulative_max_time_activity_timeschedule.pdf", plot_max,  device = "pdf", width = pdf_width, height = pdf_height)


plot_min <- gg_vistime(df_anticipated, col.start = "min_cumulative_start", col.end = "min_cumulative_end", ## Once fixed, change both to cumulative_time_start/end
                    col.event = "intervention_label", linewidth = 6,
                    col.color = "colors",
                    col.group = "category", # to define as type of intervention
                    title = "Time schedule of activities",
                    optimize_y = T, show_labels = T) +
  theme_bw() +
  scale_x_datetime(breaks = seq(min(as.POSIXct(df_anticipated$min_cumulative_start)),
                                max(as.POSIXct(df_anticipated$min_cumulative_end))+1800, by=time_step_axis), date_labels ="%H:%M") 

ggsave2("cumulative_min_time_activity_timeschedule.pdf", plot_min,  device = "pdf", width = pdf_width, height = pdf_height)
