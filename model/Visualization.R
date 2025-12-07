#############################################
### 0. Packages
#############################################

library(tidyverse)
library(lme4)
library(broom)
library(readr)
library(stargazer)
library(dplyr)
library(ggplot2)

#############################################
### 1. Read data
#############################################

df <- read_csv("model_variables_new.csv")
cat("Loaded:", nrow(df), "rows\n")

df <- subset(df, year>2016)
table(df$year)

#############################################
### 2. Filter rows with error
#############################################

err <- df %>% filter(error_flag == 1)
no_err <- df %>% filter(error_flag == 0)

#############################################
### 3. Identify element columns
#############################################

element_cols <- grep("^element", names(err), value = TRUE)
print(element_cols)

#############################################
### 4. Find top 15 error types
#############################################

all_elements <- err %>%
  select(all_of(element_cols)) %>%
  pivot_longer(everything(), values_to="element") %>%
  filter(!is.na(element))

top15 <- all_elements %>%
  count(element, sort=TRUE) %>%
  slice_head(n=15) %>%
  pull(element)

cat("Top 15 elements:\n")
print(top15)

#############################################
### 5. Create indicator cols
#############################################

clean_name <- function(x){
  x %>% 
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+","_") %>%
    str_replace("_$","")
}

err_element_names <- paste0("err_", clean_name(top15))

for(i in seq_along(top15)){
  elem <- top15[i]
  name <- err_element_names[i]
  err[[name]] <- apply(err[element_cols], 1, function(r){
    as.integer(elem %in% r)
  })
}
names(err)
table(err$err_contributions)

#############################################
### 6. Correct feature lists to match your data
#############################################

#add: 
features_cont <- c(
  "fsusize",
  "fsnetinc",
  "lastcert"
)

#add: expedser
#explore adding authrep
features_bin <- c(
  "children_present",
  "elderly_present",
  "fsndis",
  "abwdst1",
  "expedser"
)

features_cat <- c(
  "actntype...38",
  "cat_elig",
  "year"
)

features <- c(features_cont, features_bin, features_cat)

#############################################
### 7. Select needed columns
#############################################

needed <- c(features, err_element_names, "state...44")

err_and_no_err.df <- bind_rows(err, no_err)
names(err_and_no_err.df)
table(is.na(err_and_no_err.df[,107:121]))
err_and_no_err.df[,107:121][is.na(err_and_no_err.df)[,107:121]] <- 0
table(is.na(err_and_no_err.df[,107:121]))

err_model <- err_and_no_err.df %>%
  select(all_of(needed)) %>%
  drop_na()

cat("Model rows:", nrow(err_model), "\n")

#############################################
### 8. Categorical variables to factor
#############################################

err_model <- err_model %>%
  mutate(across(all_of(features_cat), as.factor))

#############################################
### 9. Run 15 logit (glm)
#############################################

logit_results <- list()

for(target in err_element_names){
  
  formula_str <- paste0(
    target, " ~ log(lastcert + 1) + ",
    paste(c(features_cont[-3], features_bin, features_cat), collapse = " + "),
    " + state...44"
  )
  formula_obj <- as.formula(formula_str)
  
  m <- glm(formula_obj, data=err_model, family=binomial)
  logit_results[[target]] <- m
  
  cat("\nLOGIT done:", target, "\n")
  print(summary(m))
}

# stargazer(logit_results[1:15], type='html', out="element_results.html")


#############################################
### 10. Visualization
#############################################

plot_state_relative <- function(state_name,
                                     models = logit_results,
                                     data   = err_model,
                                     state_var = "state...44") {
  
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(broom)
  
  results <- map_df(names(models), function(target) {
    
    m <- models[[target]]
    
    # -------------------------------
    # 1. Compute Virginia predictions
    # -------------------------------
    data_va <- data
    data_va[[state_var]] <- state_name  # make all rows "Virginia"
    
    pred_va <- mean(predict(m, data_va, type = "response"))
    
    # SE for VA (from model)
    pred_va_link <- predict(m, data_va, type = "link", se.fit = TRUE)
    se_va <- mean(pred_va_link$se.fit)
    
    
    # ------------------------------------------
    # 2. Compute Non-Virginia predictions
    # ------------------------------------------
    data_ref <- data %>% filter(.data[[state_var]] != state_name)
    pred_ref <- mean(predict(m, data_ref, type = "response"))
    
    
    # ------------------------------------------
    # 3. Compute difference + CI
    # ------------------------------------------
    diff <- pred_va - pred_ref
    
    lcl <- diff - 1.96 * se_va
    ucl <- diff + 1.96 * se_va
    
    tibble(
      error_type = target,
      diff = diff,
      lcl  = lcl,
      ucl  = ucl
    )
  })
  
  
  # -------------------------------
  # Draw a forest plot
  # -------------------------------
  ggplot(results, aes(x = diff, y = reorder(error_type, diff))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_point() +
    geom_errorbarh(aes(xmin = lcl, xmax = ucl), height = 0) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste("Relative difference in predicted probability of SNAP errors by state:", state_name),
      subtitle = paste("Compared to all other states"),
      x = "Difference in predicted probability (95% CI)",
      y = "Error Type"
    ) +
    theme_minimal()
}

