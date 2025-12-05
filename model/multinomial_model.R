#############################################
### 0. Packages
#############################################

library(tidyverse)
library(lme4)
library(broom)
library(readr)
library(stargazer)

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

stargazer(logit_results[1:15], type='html', out="element_results.html")


#############################################
### 10. Run random-slope glmer
#############################################

glmer_results <- list()

total_models <- length(err_element_names)

for (i in seq_along(err_element_names)) {
  
  target <- err_element_names[i]
  
  cat("\n===========================================\n")
  cat("Running model", i, "of", total_models, ":", target, "\n")
  cat("Start time:", as.character(Sys.time()), "\n")
  
  start_time <- Sys.time()
  
  # only keep the random intercept (1 | state)
  formula_str <- paste0(
    target, " ~ ",
    paste(features, collapse = " + "),
    " + (1 | `state...44`)"
  )
  
  formula_obj <- as.formula(formula_str)
  
  m <- tryCatch(
    {
      glmer(
        formula_obj,
        data   = err_model,
        family = binomial,

        nAGQ   = 0,
        control = glmerControl(optimizer = "bobyqa")
      )
    },
    error = function(e){
      cat("Model failed:", target, "\n")
      print(e)
      return(NULL)
    }
  )
  
  end_time <- Sys.time()
  elapsed <- end_time - start_time
  
  glmer_results[[target]] <- m
  
  cat("Finished:", target, "\n")
  cat("End time:", as.character(end_time), "\n")
  cat("Elapsed:", round(as.numeric(elapsed), 2), "seconds\n")
}

