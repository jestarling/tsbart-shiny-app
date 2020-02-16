# Adding some info to the dataset for the tsbart shiny app.

library(data.table)
library(tidyverse)

# Read data.
data <- fread("./data/tsbart-shiny-data.csv")

# Add posterior median.
data$phat_pm = (data$phat_ub_adj + data$phat_lb_adj)/2

# Arrange.
data <- data[order(id, gest_age34), ]

# Add change in risk from prior week.
data$phat_wk_chg = data$phat_pm - lag(data$phat_pm)
data$phat_wk_chg[which(data$id!=lag(data$id))] = 0
data$phat_wk_chg[1] = 0

# Add percent change in risk from prior week.
data$phat_wk_pctchg = round((data$phat_pm - lag(data$phat_pm)) / lag(data$phat_pm) * 100,3)
data$phat_wk_pctchg[which(data$id!=lag(data$id))] = 0
data$phat_wk_pctchg[1] = 0

head(data %>% select(id, gest_age34, phat_adj, phat_wk_chg, phat_wk_pctchg),30)

fwrite(data, './data/tsbart-shiny-data.csv')
