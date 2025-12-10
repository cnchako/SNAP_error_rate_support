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
  "actntype",
  "cat_elig",
  "year"
)

features <- c(features_cont, features_bin, features_cat)

#############################################
### 7. Select needed columns
#############################################

needed <- c(features, err_element_names, "state")

err_and_no_err.df <- bind_rows(err, no_err)
names(err_and_no_err.df)
table(is.na(err_and_no_err.df[,111:125]))
err_and_no_err.df[,111:125][is.na(err_and_no_err.df)[,111:125]] <- 0
table(is.na(err_and_no_err.df[,111:125]))

err_model <- err_and_no_err.df %>%
  select(all_of(needed)) %>%
  mutate(`state` = as.factor(`state`)) %>%  
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
    " + state"
  )
  formula_obj <- as.formula(formula_str)
  
  m <- glm(formula_obj, data=err_model, family=binomial)
  logit_results[[target]] <- m
  
  cat("\nLOGIT done:", target, "\n")
  print(summary(m))
}

stargazer(logit_results[1:15], type='html', out="element_results.html")


#############################################
### 10. Visualization
#############################################
plot_state_relative <- function(state_name,
                                       models = logit_results,
                                       data   = err_model,
                                       state_var = "state") {
  
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(broom)
  
  # ----------------------------------------
  # 1. Build representative household
  # ----------------------------------------
  base_row <- data %>% 
    summarize(across(all_of(features_cont), median, na.rm = TRUE))
  
  for (v in features_bin) base_row[[v]] <- 0
  for (v in features_cat) base_row[[v]] <- levels(data[[v]])[1]
  
  # baseline state (model baseline)
  ref_state <- levels(data[[state_var]])[1]  
  
  results <- map_df(names(models), function(target) {
    
    m <- models[[target]]
    
    # --- reference household ---
    new_ref <- base_row
    new_ref[[state_var]] <- factor(ref_state, levels = levels(data[[state_var]]))
    
    # --- target household (Virginia, etc.) ---
    new_target <- base_row
    new_target[[state_var]] <- factor(state_name, levels = levels(data[[state_var]]))
    
    # Predict on link scale w/ SE
    pred_ref <- predict(m, new_ref, type = "link", se.fit = TRUE)
    pred_target <- predict(m, new_target, type = "link", se.fit = TRUE)
    
    # convert to probability
    p_ref <- plogis(pred_ref$fit)
    p_target <- plogis(pred_target$fit)
    
    diff <- p_target - p_ref
    
    # CI uses SE of target (conservative)
    lcl <- plogis(pred_target$fit - 1.96 * pred_target$se.fit) - p_ref
    ucl <- plogis(pred_target$fit + 1.96 * pred_target$se.fit) - p_ref
    
    tibble(
      error_type = target,
      diff = diff,
      lcl = lcl,
      ucl = ucl
    )
  })
  
  ggplot(results, aes(x = diff, y = reorder(error_type, diff))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_point() +
    geom_errorbarh(aes(xmin = lcl, xmax = ucl), height = 0) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = paste("Relative difference in predicted probability of SNAP errors by state:", state_name),
      subtitle = paste("Compared with other states"),
      x = "Difference in predicted probability (95% CI)",
      y = "Error Type"
    ) +
    theme_minimal()
}

#############################################
### 11. Visualization (sjPlot)
#############################################

library(sjPlot)
library(sjmisc)
library(sjlabelled)

# Create a folder for plots (optional)
if (!dir.exists("plots")) dir.create("plots")

# ------------------------------------------
# 10A. Plot coefficient estimates for each model
# ------------------------------------------

for(target in names(logit_results)) {
  
  m <- logit_results[[target]]
  
  # save plot
  p <- plot_model(
    m, 
    type = "est",
    show.values = TRUE,
    value.offset = .3,
    ci_method = "wald" 
  ) +
    ggtitle(paste("Coefficient Plot for:", target))
  
  # save image
  ggsave(
    filename = paste0("plots/coefplot_", target, ".png"),
    plot = p,
    width = 8, height = 6
  )
  
  cat("Saved coefficient plot for:", target, "\n")
}

