#############################################
### 0. Packages (NEW)
#############################################

library(tidyverse)
library(tidymodels)
library(readr)
library(vip)        # variable importance
library(themis)     # optional, for imbalance
library(parsnip)
library(stringr)
tidymodels_prefer()

#############################################
### 1. Read & basic filter
#############################################

df <- read_csv("model_variables_new.csv", show_col_types = FALSE)


df <- df %>%
  filter(year > 2016) %>%
  filter(!is.na(timeper1)) %>%
  filter(!is.na(status)) %>%
  mutate(
    # ---- construct overpayment target ----
    overpayment_error_i = factor(
      if_else(status == "overissuance", "yes", "no"),
      levels = c("no", "yes")
    ),
    
    # ---- FIX: convert timeper text -> numeric code ----
    timeper_cat = case_when(
      str_detect(timeper1, regex("before", ignore_case = TRUE)) ~ 1,
      str_detect(timeper1, regex("at time", ignore_case = TRUE)) ~ 2,
      str_detect(timeper1, regex("after", ignore_case = TRUE)) ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(timeper_cat))

table(df$timeper_cat)
table(df$status, df$overpayment_error_i)

#############################################
### 2. Dataset split by error timing
#############################################

# 1) Errors at certification / renewal
df_cert <- df %>%
  filter(timeper_cat == 2)

# 2) Errors between certification
df_between <- df %>%
  filter(timeper_cat %in% c(1, 3))

cat("Cert errors:", nrow(df_cert), "\n")
cat("Between-cert errors:", nrow(df_between), "\n")

#############################################
### 3. Feature set
#############################################

features <- c(
  "fsusize",            # household size
  "children_present",   # children indicator
  "elderly_present",    # elderly indicator
  "fsndis",             # disabled indicator
  "certmth",            # months since certification
  "fstotde2",           # total deductions
  "fsnetinc",           # net income (optional but useful)
  "abwdst1",            # (keep if exists)
  "expedser",           # expedited service
  "cat_elig",           # eligibility category
  "amount_rel_max",
  "state",
  "year"
)

#############################################
### 4. Preprocessing recipe
#############################################

make_recipe <- function(data){
  recipe(
    overpayment_error_i ~ ., 
    data = data %>% select(all_of(c("overpayment_error_i", features)))
  ) %>%
    step_mutate(
      state = as.factor(state),
      year  = as.factor(year)
    ) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())
}

#############################################
### 5. Models (RF + Boosted Trees)
#############################################

rf_spec <- rand_forest(
  mtry = 10,          
  trees = 300,        
  min_n = 10
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

boost_spec <- boost_tree(
  trees = 500,
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

#############################################
### 6. Workflow + CV function
#############################################

run_ml <- function(data, model_spec, model_name){
  
  set.seed(123)
  
  use_strata <- n_distinct(data$overpayment_error_i) > 1
  
  split <- if (use_strata) {
    initial_split(data, strata = overpayment_error_i)
  } else {
    initial_split(data)
  }
  
  train <- training(split)
  test  <- testing(split)
  
  rec <- make_recipe(train)
  
  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(model_spec)
  
  folds <- if (use_strata) {
    vfold_cv(train, v = 5, strata = overpayment_error_i)
  } else {
    vfold_cv(train, v = 5)
  }
  
  has_tune <- any(map_lgl(model_spec$args, ~ inherits(.x, "tune")))
  
  if (has_tune) {
    tuned <- tune_grid(
      wf,
      resamples = folds,
      metrics = metric_set(roc_auc)
    )
    
    best <- select_best(tuned, "roc_auc")
    final_wf <- finalize_workflow(wf, best)
    
    fit <- fit(final_wf, data = train)
    
  } else {
    fit <- fit(wf, data = train)
  }
  
  preds <- predict(fit, test, type = "prob") %>%
    bind_cols(test %>% select(overpayment_error_i))
  
  auc <- roc_auc(
    preds,
    truth = overpayment_error_i,
    .pred_yes
  )
  
  list(
    fit   = fit,
    auc   = auc,
    model = model_name
  )
}


#############################################
### 7. Run models (Certification errors)
#############################################

rf_cert <- run_ml(df_cert, rf_spec, "RF_cert")
# boost_cert <- run_ml(df_cert, boost_spec, "Boost_cert")  

#############################################
### 8. Run models (Between-cert errors)
#############################################

rf_between <- run_ml(df_between, rf_spec, "RF_between")
# boost_between <- run_ml(df_between, boost_spec, "Boost_between")

#############################################
### 9. Compare performance
#############################################

bind_rows(
  rf_cert$auc    %>% mutate(dataset = "cert",    model = "RF"),
  rf_between$auc %>% mutate(dataset = "between", model = "RF")
)
