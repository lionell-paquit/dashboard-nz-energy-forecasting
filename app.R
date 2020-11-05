
# ----------- NZ Energy Forecasting --------------#

# Load libraries

library(shiny)
library(flexdashboard) # devtools::install_github('rstudio/flexdashboard')

library(data.table)
library(prophet)
library(plotly) # devtools::install_github('ropensci/plotly')
library(ggplot2) # devtools::install_github('hadley/ggplot2')





pricing_features <- fread("data/pricing_features.csv")
pricing_actuals <- fread("data/pricing_actual_features.csv")
pricing_futures <- fread("data/pricing_future_features.csv")


# ---------------- KPI CARD --------------------#




#----------------- CHARTS ----------------------#

m <- prophet()

for (f in colnames(pricing_features)[3:12]) {
    m <- add_regressor(m, f)
}

m <- fit.prophet(m, pricing_features)

forecast <- predict(m, pricing_futures)

g <- plot(m, forecast)