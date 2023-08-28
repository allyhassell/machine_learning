# R modelling

### Initialise

# Libraries
library(qicharts2)
library("readr")
library(magrittr)
library(dplyr)

# Data
data = read.csv('monitor.csv')
summary(data)

# Date to day of the year
data$Timestamp <- as.Date(data$Timestamp)
data$DayYear <- as.numeric(strftime(data$Timestamp, format = "%j"))

# Control signal and breach function
control_signals <- function(plot) {
  summary(plot) %>%
    select(runs.signal, sigma.signal)
}

### X-bar 
# Xbar chart 
xbar_plot_mem <- qicharts2::qic(x = DayYear, y = ProcessMemory, data = data, chart = "xbar", subtitle = "Process Memory", freeze=40)
xbar_plot_pred <- qicharts2::qic(x = DayYear, y = Prediction, data = data, chart = "xbar", subtitle = "Prediction", freeze=40)
xbar_plot_time <- qicharts2::qic(x = DayYear, y = PredictionTimeMS, data = data, chart = "xbar", subtitle = "Prediction Time MS", freeze=40)

# Plot X-bar
plot(xbar_plot_mem)
plot(xbar_plot_pred)
plot(xbar_plot_time)

### S-bar 
# S-chart 
s_plot_mem <- qicharts2::qic(x = DayYear, y = ProcessMemory, data = data, chart = "s", subtitle = "Process Memory", freeze=40)
s_plot_pred<- qicharts2::qic(x = DayYear, y = Prediction, data = data, chart = "s", subtitle = "Prediction", freeze=40)
s_plot_time <- qicharts2::qic(x = DayYear, y = PredictionTimeMS, data = data, chart = "s", subtitle = "Prediction Time MS", freeze=40)

# Plot S-chart
plot(s_plot_mem)
plot(s_plot_pred)
plot(s_plot_time)

### Control table
# Results signal and sigma
mem_ctrl <- control_signals(xbar_plot_mem)
pred_ctrl <- control_signals(xbar_plot_pred)
time_ctrl <- control_signals(xbar_plot_time)

# Results signal and sigma
mem_s <- control_signals(s_plot_mem)
pred_s <- control_signals(s_plot_pred)
time_s <- control_signals(s_plot_time)

# Combine X-bar results
df_ctrl = rbind(mem_ctrl, pred_ctrl, time_ctrl)
df_ctrl['Measurement'] = c('ProcessMemory', 'Prediction', 'PredictionTimeMS')
df_s = rbind(mem_s, pred_s, time_s)
df_s['Measurement'] = c('ProcessMemory', 'Prediction', 'PredictionTimeMS')

# Merge X-bar and S-chart 
df = merge(x = df_ctrl, y = df_s, by = "Measurement")

# Rename columns
df <- df %>% 
  rename("xbar_breach" = "runs.signal.x",
         "xbar_runs_signal" = "sigma.signal.x",
         "s_breach" = "runs.signal.y", 
         "s_runs_signal" = "sigma.signal.y"
  )

# Automatic control assessment 
df["Control"] = ifelse(df$xbar_breach==0 & 
                         df$xbar_runs_signal==0 & 
                         df$s_breach==0 & 
                         df$s_runs_signal==0, 
                       "In control", "Out of control")

### Overall summary

# Automatic control assessment 
df["Control"] = ifelse(df$xbar_breach==0 & 
                         df$xbar_runs_signal==0 & 
                         df$s_breach==0 & 
                         df$s_runs_signal==0, 
                       "In control", "Out of control")
df