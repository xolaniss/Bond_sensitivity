# Description
# Yield sensitivity country comparision by Xolani Sibande - 11 October 2023

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
clean_up <- function(data, name = "10-year SA yield"){
  data %>% 
    rename("Date" = "DATE") %>%
    rename({{ name }} :=  colnames(data[,2])) %>% 
    filter(Date >= "2000-01-01") 
}
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))

# Import -------------------------------------------------------------
folder_path <- 
  file.path(path = here("Data"))
file_names <- 
  list.files(path = here("Data"))
file_paths <- paste0(folder_path, "/", file_names) %>% as.list()

yields <- file_paths %>% map( ~read_csv(.))
yields

# Cleaning -----------------------------------------------------------------

## US --------------------------------------------------------------------
us_yield_tbl <- 
  yields[[2]] %>% 
  mutate(DGS10 = as.numeric( DGS10), DATE = ymd(DATE)) %>% 
  rename("Date" = "DATE") %>% 
  rename("10-year UST yield" = "DGS10") %>% 
  filter(Date >= "2000-01-01") %>% 
  summarise_by_time(.date_var = Date, 
                    .by = "month", 
                    .type = "floor", 
                    `10-year UST yield` = mean(`10-year UST yield`, na.rm = TRUE)) 


## South Africa ----------------------------------------------------------
sa_yield_tbl <- 
  yields[[11]] %>% 
  clean_up(name = "10-year SA yield")

## India -----------------------------------------------------------------
ind_yield_tbl <- 
  yields[[3]] %>% 
  clean_up(name = "10-year India yield")
## Poland ------------------------------------------------------------------
pol_yield_tbl <- 
  yields[[10]] %>% 
  clean_up(name = "10-year Poland yield")

## Colombia --------------------------------------------------------------
col_yield_tbl <- 
  yields[[1]] %>% 
  clean_up(name = "10-year Colombia yield")

## Chile -------------------------------------------------------------------
chile_yield_tbl <- 
  yields[[4]] %>% 
  clean_up(name = "10-year Chile yield") %>% 
  mutate(`10-year Chile yield` = as.numeric(`10-year Chile yield`))

## Czech -------------------------------------------------------------------
korea_yield_tbl <- 
  yields[[8]] %>% 
  clean_up(name = "10-year South Korea yield")

# Combined ----------------------------------------------------------------
combined_yield_tbl <- 
  us_yield_tbl %>% 
  left_join(sa_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(ind_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(pol_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(col_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(chile_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(korea_yield_tbl, by = c("Date" = "Date")) %>% 
  drop_na()

# EDA ---------------------------------------------------------------
combined_yield_tbl %>% skim()


# Graphing ---------------------------------------------------------------
combined_yield_gg <- 
  combined_yield_tbl %>% 
  fx_plot(variables_color = 7) +
  labs(y = "yield (%)", title = "SA EME peer group 10-year yield comparison") +
  geom_vline(
    xintercept = as.Date("2022-01-01"), 
    linetype = 2, 
    linewidth = 0.6) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# OLS ---------------------------------------------------------------------

formula <-  as.formula(Yield ~ `10-year UST yield`)

## Static ------------------------------------------------------------------
ols_full_tbl <- 
  combined_yield_tbl %>% 
  pivot_longer(
    c(-Date, -`10-year UST yield`), 
    names_to = "Category", 
    values_to = "Yield") %>% 
  group_by(Category) %>% 
  ols_nest_full_prep() %>% 
  ols_tidy_group_models(formula = formula) %>% 
  unnest(cols = models_coef, names_repair = "universal") %>% 
  dplyr::select(Category, term, estimate, statistic) %>% 
  mutate(across(2:3, as.character)) %>% 
  mutate(across(2, ~strtrim(., 4))) %>%   
  mutate(across(3, ~strtrim(., 4))) %>% 
  mutate(Category = str_remove_all(Category, " yield"),
         Category = str_remove_all(Category, "10-year "))

ols_full_gg <- 
  ols_full_tbl %>% 
  mutate(estimate = as.numeric(estimate),
         statistic = as.numeric(statistic)) %>% 
  filter(!term == "(Intercept)") %>% 
  dplyr::select(Category, estimate) %>% 
  ggplot(aes(x = reorder(Category, estimate), y = estimate, fill = Category)) +
    geom_col() +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(
    text = element_text(size = 10),
    strip.background = element_rect(colour = "white", fill = "white"),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 10),
    plot.tag = element_text(size = 10),
    plot.title = element_text(face = "bold")
  ) +
  scale_fill_manual(values = pnw_palette("Shuksan2", 6)) +
  labs(x = " ", 
       y = "%",
       title = "Long term sensitivity of the 10-year country bonds to the 10-year UST (2011-2023)") +
  coord_flip()

ols_full_gg

## Rolling -----------------------------------------------------------------
time_models_tbl <- 
  combined_yield_tbl %>% 
  pivot_longer(
    c(-Date, -`10-year UST yield`), 
    names_to = "Category", 
    values_to = "Yield") %>% 
  mutate(Category = str_remove_all(Category, " yield"),
         Category = str_remove_all(Category, "10-year ")) %>% 
  ols_slidify_models_standard() %>% 
  unnest_rol_col_standard(rol_column = models)
 
time_models_coef_gg <- 
  time_models_tbl %>% 
  dplyr::select(-starts_with("t"), -Intercept) %>% 
  slidyfy_gg_workflow_standard() +
  labs(y = "%", 
       title = "Sensitivity of the 10-year country bonds to the 10-year US yield") +
  geom_vline(
    xintercept = as.Date("2022-01-01"), 
    linetype = 2, 
    linewidth = 0.6) +
  theme(legend.position = "none")

time_models_tstats_gg <- 
  time_models_tbl %>% 
  dplyr::select(-Coefficient, -Intercept, -`t-statistic (intercept)`) %>% 
  slidyfy_gg_workflow_standard() +
  labs( 
       title = "Significance of the sensitivity") +
  geom_hline(yintercept = 1.96, col = "red", linetype = 3, linewidth = 1) +
  geom_hline(yintercept = -1.96, col = "red", linetype = 3, linewidth = 1) +
  geom_vline(
    xintercept = as.Date("2022-01-01"), 
    linetype = 2, 
    linewidth = 0.6) 

graphs_gg <-  time_models_coef_gg / time_models_tstats_gg
graphs_gg

## Range analysis -----------------------------------------------------------
sensitivity_range_gg <- 
  time_models_tbl %>% 
  mutate(All = "All") %>% 
  dplyr::select(Date, Category, Coefficient, -All) %>% 
  group_by(Category) %>% 
  plot_time_series_boxplot(
    Date, 
    Coefficient,
    .period      = "year",
    .facet_ncol  = 2,     # 2-column layout
    .interactive = FALSE, 
    .smooth = FALSE,
    .color_var = Category,
    .line_size = 1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(size = 10),
    strip.background = element_rect(colour = "white", fill = "white"),
    axis.text.x = element_text(angle = 90),
    axis.title = element_text(size = 10),
    plot.tag = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  labs(y = "%", title = "Sensitivity range since 2014") +
  scale_color_manual(values = pnw_palette("Shuksan2", 6))
  
sensitivity_range_gg



# Export ---------------------------------------------------------------
artifacts_country_comparison <- list(
  data = list(
    combined_yield_tbl = combined_yield_tbl
    ),
  graphs = list(
    combined_yield_gg = combined_yield_gg,
    ols_full_gg = ols_full_gg,
    graphs_gg = graphs_gg,
    sensitivity_range_gg = sensitivity_range_gg
  )
  
)

write_rds(artifacts_country_comparison, file = here("Outputs", "artifacts_country_comparison.rds"))


