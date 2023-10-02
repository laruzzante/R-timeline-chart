library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)


dat <- read_xlsx("datasheet_example.xlsx")

# Populate missing values in Schduled start column
dat$scheduled_start <- na.locf(dat$scheduled_start)
# Format scheduled start
dat$scheduled_start <- as.POSIXct(dat$scheduled_start, format="%Y-%M-%D %H:%M:%S", tz="CET")

# Populate missing values in Abbreviations column based on values already associated with Intervention column but not filled throughout
dat <- dat %>% 
  group_by(intervention) %>% 
  fill(abbreviations, .direction = "down")


df <- dat
df$category <- as.factor(df$category)
# df$category <- factor(df$category, levels = c("Administation & interprofessionelle Austausch", "Kontrollen",
#                                               "Medizinaltechnische Verrichtungen", "Pflegemassnahmen",
#                                               "Eintritte","Austritte","Transport","Unterstützung bei Interventionen"))

df$intervention_label <- ifelse(is.na(df$abbreviations), df$intervention, df$abbreviations)


# Move scheduled starts to 1 day after, if time is after midnight (or in our case, before 7am to distinguish them from first interventions, as we have no day to distinguish)
df$scheduled_start <- do.call("c", lapply(df$scheduled_start, FUN = function(x) if(x < as.POSIXct("1899-12-31 07:00:00")) x + 3600*24 else(x)))

# Create and populate Intervention scheduled time end values
df$average_time_end <- as.POSIXct(df$scheduled_start)
df$average_time_end <- df$average_time_end + df$average_duration*60 ## POSIXct times only works if seconds are added, not minutes

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

# df[34,]
# df <- df[-34,] ## Removing rows with missing duration

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
        sdf[row,'average_cumulative_start'] <- sdf[row-1, "average_cumulative_end"] + 60 # we add 60 seconds to add some white space between bars in plot, purely graphical
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



# df %>% group_by(scheduled_start, category)
# 
# df$average_cumulative_time_start <- NA
# for(row in 1:nrow(df)){
#   if(row == 1) df$aver
# }


# df$formatted_scheduled_start <- as.POSIXct(df$scheduled_start)
# df$formatted_average_time_end <- df$formatted_scheduled_start + minutes(df$average_duration)
# 
# df$formatted_max_time_end <- ifelse(is.na(df$max_duration), df$formatted_average_time_end, df$formatted_scheduled_start + minutes(df$max_duration))
# df$formatted_max_time_end <- as.POSIXct(df$formatted_max_time_end, origin = lubridate::origin)
# 
# 
# df$formatted_average_cumulative_start <- as.POSIXct(df$formatted_average_cumulative_start)
# df$formatted_average_cumulative_end <- df$formatted_average_cumulative_start + minutes(df$average_duration)
# 
# df$formatted_max_cumulative_end <- ifelse(is.na(df$max_duration), df$formatted_average_cumulative_end, df$formatted_average_cumulative_start + minutes(df$max_duration))
# df$formatted_max_cumulative_end <- as.POSIXct(df$formatted_max_cumulative_end, origin = lubridate::origin)


source("succession_of_N_most_distinct_colours.R")

## Colouring by scheduled starting hour
# colors <- get_colours(length(unique(df$scheduled_start)))
# names(colors) = unique(df$scheduled_start)
# df$colors = colors[as.character(df$scheduled_start)]

df <- all_categories_sdf

## Colouring by activity category
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


# plot1 <- gg_vistime(df, col.start = "formatted_scheduled_start", col.end = "formatted_average_time_end", ## Once fixed, change both to cumulative_time_start/end
#                     col.event = "intervention_label",
#                     col.color = "colors",
#                     col.group = "category", # to define as type of intervention, either ICU, or student supervision, or other
#                     title = "Time schedule of nursing activity in IMC",
#                     optimize_y = T, show_labels = T) +
#   theme_bw() +
#   scale_x_datetime(breaks = seq(min(as.POSIXct(df$formatted_scheduled_start)),
#                                 max(as.POSIXct(df$formatted_average_time_end))+1800, by=time_step_axis), date_labels ="%H:%M") 
# 
# ggsave2("average_time_nursing_activity_IMC_timeschedule.pdf", plot1,  device = "pdf", width = pdf_width, height = pdf_height)
# 
# 
# plot2 <- gg_vistime(df, col.start = "formatted_scheduled_start", col.end = "formatted_max_time_end", ## Once fixed, change both to cumulative_time_start/end
#                     col.event = "intervention_label",
#                     col.color = "colors",
#                     col.group = "category", # to define as type of intervention, either ICU, or student supervision, or other
#                     title = "Time schedule of nursing activity in IMC",
#                     optimize_y = T, show_labels = T) +
#   theme_bw() +
#   scale_x_datetime(breaks = seq(min(as.POSIXct(df$formatted_scheduled_start)),
#                                 max(as.POSIXct(df$formatted_max_time_end))+1800, by=time_step_axis), date_labels ="%H:%M") 
# 
# ggsave2("max_time_nursing_activity_IMC_timeschedule.pdf", plot2,  device = "pdf", width = pdf_width, height = pdf_height)
# 
# 
# 
# 
# df_anticipated$category <- as.factor(df_anticipated$category)
# df_anticipated$category <- factor(df_anticipated$category, levels = c("Administation & interprofessionelle Austausch", "Kontrollen",
#                                                                       "Medizinaltechnische Verrichtungen", "Pflegemassnahmen",
#                                                                       "Eintritte","Austritte","Transport","Unterstützung bei Interventionen"))
# 
# df_anticipated$intervention_label <- ifelse(is.na(df_anticipated$abbreviations), df_anticipated$intervention, df_anticipated$abbreviations)
# 
# df_anticipated$average_duration <- df_anticipated$average_duration + 1
# df_anticipated$max_duration <- df_anticipated$max_duration + 1
# 
# df_anticipated$formatted_scheduled_start <- as.POSIXct(df_anticipated$formatted_scheduled_start)
# df_anticipated$formatted_average_time_end <- df_anticipated$formatted_scheduled_start + minutes(df_anticipated$average_duration)
# 
# df_anticipated$formatted_max_time_end <- ifelse(is.na(df_anticipated$max_duration), df_anticipated$formatted_average_time_end, df_anticipated$formatted_scheduled_start + minutes(df_anticipated$max_duration))
# df_anticipated$formatted_max_time_end <- as.POSIXct(df_anticipated$formatted_max_time_end, origin = lubridate::origin)
# 
# 
# df_anticipated$formatted_average_cumulative_start <- as.POSIXct(df_anticipated$formatted_average_cumulative_start)
# df_anticipated$formatted_average_cumulative_end <- df_anticipated$formatted_average_cumulative_start + minutes(df_anticipated$average_duration)
# 
# df_anticipated$formatted_max_cumulative_end <- ifelse(is.na(df_anticipated$max_duration), df_anticipated$formatted_average_cumulative_end, df_anticipated$formatted_average_cumulative_start + minutes(df_anticipated$max_duration))
# df_anticipated$formatted_max_cumulative_end <- as.POSIXct(df_anticipated$formatted_max_cumulative_end, origin = lubridate::origin)
# 
# colors <- get_colours(length(unique(df_anticipated$category)))
# names(colors) = unique(df_anticipated$category)
# df_anticipated$colors = colors[as.character(df_anticipated$category)]

plot3 <- gg_vistime(df_anticipated, col.start = "average_cumulative_start", col.end = "average_cumulative_end", ## Once fixed, change both to cumulative_time_start/end
                    col.event = "intervention_label", linewidth = 6,
                    col.color = "colors",
                    col.group = "category", # to define as type of intervention, either ICU, or student supervision, or other
                    title = "Time schedule of nursing activity in Notfall",
                    optimize_y = T, show_labels = T) +
  theme_bw() +
  scale_x_datetime(breaks = seq(min(as.POSIXct(df_anticipated$average_cumulative_start)),
                                max(as.POSIXct(df_anticipated$average_cumulative_end))+1800, by=time_step_axis), date_labels ="%H:%M")

ggsave2("notfall_cumulative_average_time_nursing_activity_timeschedule.pdf", plot3,  device = "pdf", width = pdf_width, height = pdf_height)


plot4 <- gg_vistime(df_anticipated, col.start = "max_cumulative_start", col.end = "max_cumulative_end", ## Once fixed, change both to cumulative_time_start/end
                    col.event = "intervention_label", linewidth = 6,
                    col.color = "colors",
                    col.group = "category", # to define as type of intervention, either ICU, or student supervision, or other
                    title = "Time schedule of nursing activity in Notfall",
                    optimize_y = T, show_labels = T) +
  theme_bw() +
  scale_x_datetime(breaks = seq(min(as.POSIXct(df_anticipated$max_cumulative_start)),
                                max(as.POSIXct(df_anticipated$max_cumulative_end))+1800, by=time_step_axis), date_labels ="%H:%M") 

ggsave2("notfall_cumulative_max_time_nursing_activity_timeschedule.pdf", plot4,  device = "pdf", width = pdf_width, height = pdf_height)
