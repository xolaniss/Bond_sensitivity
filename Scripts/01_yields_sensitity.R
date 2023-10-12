# Description
# Cleaning data and analysis by Xolani Sibande - 10 October 2023

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import and cleaning -------------------------------------------------------------
us_yield_tbl <- 
  read_csv(here("Data", "DGS10.csv")) %>% 
  mutate(DGS10 = as.numeric( DGS10), DATE = ymd(DATE)) %>% 
  rename("Date" = "DATE") %>% 
  rename("10-year UST yield" = "DGS10") %>% 
  filter(Date >= "2000-01-01") %>% 
  summarise_by_time(.date_var = Date, 
                    .by = "month", 
                    .type = "floor", 
                    `10-year UST yield` = mean(`10-year UST yield`, na.rm = TRUE)) 
sa_yield_tbl <- 
  read_csv(here("Data", "IRLTLT01ZAM156N.csv")) %>% 
  rename("Date" = "DATE") %>% 
  rename("10-year SA yield" = "IRLTLT01ZAM156N") %>% 
  filter(Date >= "2000-01-01") 

# EDA ---------------------------------------------------------------
us_yield_tbl %>% skim()
sa_yield_tbl %>% skim()

# Joining -----------------------------------------------------------------
yields_tbl <- 
  us_yield_tbl %>% 
  left_join(sa_yield_tbl, by = c("Date" = "Date"))

# Graphing ---------------------------------------------------------------
yield_gg <- 
  yields_tbl %>% 
  fx_plot(variables_color = 4) +
  labs(y = "yield (%)", title = "10-year UST yield versus the 10-year SA yield")  +
  annotate("text", 
           x = as.Date("2016-2-01"), 
           y = 12, 
           label = "Fed starts hiking cycle") +
  geom_vline(
    xintercept = as.Date("2022-01-01"), 
    linetype = 2, 
    linewidth = 0.6) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")


# General OLS ------------------------------------------------------------
fomular <- as.formula(`10-year SA yield` ~ `10-year UST yield`)
ols_general <- coeftest(lm(fomular, data = yields_tbl))

# Rolling window OLS ------------------------------------------------------
rolling_reg <-
  slidify(
    .f =  ~coeftest(lm(..1 ~ ..2)),
    .period = 36,
    .align = "right",
    .unlist = FALSE,
    .partial = FALSE
  )

yields_rol_model <- 
  yields_tbl %>% mutate(rolling_sensitity = rolling_reg(`10-year UST yield`, `10-year SA yield`))

yields_rol_model_tbl <- 
  yields_rol_model %>% 
  mutate(glance = map(rolling_sensitity, broom::tidy)) %>% 
  unnest(glance) %>% 
  dplyr::select(Date, term:estimate, statistic) %>% 
  drop_na() %>% 
  pivot_wider(names_from = term, values_from = c(estimate, statistic)) %>% 
  dplyr::rename("Intercept" = `estimate_(Intercept)`,
                "Coefficient" = `estimate_..2`,
                "t-statistic Intercept" = `statistic_(Intercept)`,
                "t-statistic (coefficient)" = `statistic_..2`
  )


yields_rol_model_coef_gg <- 
  yields_rol_model_tbl %>% 
  dplyr::select(Date, Coefficient) %>% 
  fx_plot() +
  geom_hline(yintercept = 0.38048, col = "blue", linetype = 3, linewidth = 1) +
  labs(y = "%", 
       title = "Sensitivity of 10-year SA yield to the 10-year US yield") +
  geom_vline(
    xintercept = as.Date("2022-01-01"), 
    linetype = 2, 
    linewidth = 0.6)
  

yields_rol_model_tstats_gg <- 
  yields_rol_model_tbl %>% 
  dplyr::select(Date,  `t-statistic (coefficient)`) %>% 
  fx_plot() +
  geom_hline(yintercept = 1.96, col = "red", linetype = 3, linewidth = 1) +
  geom_hline(yintercept = -1.96, col = "red", linetype = 3, linewidth = 1) +
  labs( 
       title = "Significance of the sensitivity") +
  geom_vline(
    xintercept = as.Date("2022-01-01"), 
    linetype = 2, 
    linewidth = 0.6) +
  scale_color_manual(values = "#a45851")

graphs_gg <- yields_rol_model_coef_gg / yields_rol_model_tstats_gg


# Export ---------------------------------------------------------------
artifacts_yields <- list (
  data = list(
    yields_tbl = yields_tbl
  ),
  graphs = list(
    yield_gg = yield_gg,
    yields_rol_model_coef_gg = yields_rol_model_coef_gg,
    yields_rol_model_tstats_gg = yields_rol_model_tstats_gg,
    graphs_gg = graphs_gg
  )
)

write_rds(artifacts_yields, file = here("Outputs", "artifacts_yields.rds"))


