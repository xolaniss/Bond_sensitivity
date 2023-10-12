ols_nest_crisis_prep <-
function(data){
  data %>%
    dplyr::relocate(Date, .after = "Category") %>% 
    group_by(Category, Crisis) %>% 
    nest()
}
ols_tidy_group_models <-
function(nested_data, formula){
  formula <-  formula
  nested_data %>% 
    mutate(models = map(data, ~coeftest(lm(formula, data = .), 
                                        vcov = vcovHAC))) %>% 
    mutate(models_coef = map(models, ~tidy(.)))
}
ols_pretty_crisis_results <-
function(data_fitted_models){
  data_fitted_models %>% 
    unnest(cols = models_coef, names_repair = "universal") %>% 
    dplyr::select(Category, Crisis, term, estimate, p.value) %>% 
    mutate(across(2:3, as.character)) %>% 
    mutate(across(2, ~strtrim(., 8))) %>%   
    mutate(across(3, ~strtrim(., 4))) %>% 
    mutate(comb = paste0(estimate, " ", "[", p.value, "]")) %>% 
    dplyr::select(Category,Crisis, term, comb) %>% 
    pivot_longer(-c(Category,Crisis, term)) %>% 
    spread(key = term, value = value) %>% 
    mutate(across(2:4, ~str_replace_all(., "\\[0]", "[0.00]"))) %>% 
    dplyr::select(-name)
}

ols_group_crisis_workflow <-
function(data) {
  data %>% 
    ols_nest_crisis_prep() %>% 
    ols_tidy_group_models(formula = formula) %>% 
    ols_pretty_crisis_results()
}

ols_nest_full_prep <-
  function(data){
    data %>%
      dplyr::relocate(Date, .after = "Category") %>% 
      group_by(Category) %>% 
      nest()
  }

ols_pretty_full_results <-
  function(data_fitted_models){
    data_fitted_models %>% 
      unnest(cols = models_coef, names_repair = "universal") %>% 
      dplyr::select(Category, term, estimate, p.value) %>% 
      mutate(across(2:3, as.character)) %>% 
      mutate(across(2, ~strtrim(., 8))) %>%   
      mutate(across(3, ~strtrim(., 4))) %>% 
      mutate(comb = paste0(estimate, " ", "[", p.value, "]")) %>% 
      dplyr::select(Category, term, comb) %>% 
      pivot_longer(-c(Category, term)) %>% 
      spread(key = term, value = value) %>% 
      mutate(across(2:4, ~str_replace_all(., "\\[0]", "[0.00]"))) %>% 
      dplyr::select(-name)
  }

ols_group_full_workflow <-
  function(data) {
    data %>% 
      ols_nest_full_prep() %>% 
      ols_tidy_group_models(formula = formula) %>% 
      ols_pretty_full_results()
  }


