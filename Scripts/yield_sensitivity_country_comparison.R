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

## SA --------------------------------------------------------------------
sa_yield_tbl <- 
  yields[[11]] %>% 
  clean_up()

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
czech_yield_tbl <- 
  yields[[5]] %>% 
  clean_up(name = "10-year Czech Republic yield")

# Combined ----------------------------------------------------------------
combined_yield_tbl <- 
  us_yield_tbl %>% 
  left_join(sa_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(ind_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(pol_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(col_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(chile_yield_tbl, by = c("Date" = "Date")) %>% 
  left_join(czech_yield_tbl, by = c("Date" = "Date")) %>% 
  drop_na()

# EDA ---------------------------------------------------------------
combined_yield_tbl %>% skim()

# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


