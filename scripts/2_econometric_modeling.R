
## ANALYSIS OF THE IMPACT ON IMPORTED FOOD AND BEVERAGE PRICES 
## FOLLOWING THE REDUCTION OF THE "IMPUESTO PAIS"

# Load necessary libraries

library(dplyr)
library(tidyverse)
library(lubridate)
library(AER)
library(plm)
library(stargazer)
library(foreign)
library(ggplot2)
library(tibble)
library(miscTools)
library(sandwich)
library(car)

# Load data
df <- read_csv("df_clean.csv",show_col_types = FALSE)
sapply(df, class)
summary(df)

# Exclude categories that violate the Parallel Trends Assumption
df_excl <- df %>% filter(!categoria_producto %in% c("Galletitas", "Enlatados"))

# Create panel data object for Fixed Effects (FE) analysis
df_panel <- pdata.frame(df_excl, index = c("prod", "time"))

##------------------------------------  Diagnostics  ------------------------------------ 

# Multicollinearity check (VIF)    
model_temp <- lm(price_variation_c ~ post + imported + timeToTreat + dolar_variation + stock_dummy + max_consecutivos_sin_stock , data = df_excl)
    vif(model_temp)

##------------------------------------  Modelos  ------------------------------------  

# Basic Model (Two-Way Fixed Effects)

    did_log_precio <- plm(log_precio ~ post + imported + post*imported, data = df_panel, model = "within", effect = "twoways")
    did_price_variation <- plm(price_variation_c ~ post + imported + post*imported, data = df_panel, model = "within", effect = "twoways")
    ## Robust Standard Errors
    robust_se_log_precio <- coeftest(did_log_precio, vcov. = vcovHC, type = "HC1")
    robust_se_price_variation <- coeftest(did_price_variation, vcov. = vcovHC, type = "HC1")

## Basic Model + Control Variables
    
    did_log_precio3 <- plm(log_precio ~ post + imported + post*imported +  stock_dummy + max_consecutivos_sin_stock , data = df_panel, model = "within", effect = "twoways")
    did_price_variation3 <- plm(price_variation_c ~ post + imported + post*imported + stock_dummy + max_consecutivos_sin_stock , data = df_panel, model = "within", effect = "twoways")
    
    ## Robust Standard Errors
    robust_se_log_precio3 <- coeftest(did_log_precio3, vcov. = vcovHC, type = "HC1")
    robust_se_price_variation3 <- coeftest(did_price_variation3, vcov. = vcovHC, type = "HC1")
    
## Summary Table using stargazer with Robust SEs
    stargazer(did_log_precio, did_price_variation, did_log_precio3, did_price_variation3,
              type = "text",
              se = list(sqrt(diag(vcovHC(did_log_precio, type = "HC1"))),
                        sqrt(diag(vcovHC(did_price_variation, type = "HC1"))),
                        sqrt(diag(vcovHC(did_log_precio3, type = "HC1"))),
                        sqrt(diag(vcovHC(did_price_variation3, type = "HC1")))),
              title = "DiD Models: Impacto of Impuesto PAIS tax reduction",
              add.lines = list(c("Fixed Effects", "Prod, Time", "Prod, Time", "Prod, Time", "Prod, Time"),
                               c("Robust Errors", "HC1 (cluster: prod)", "HC1 (cluster: prod)", "HC1 (cluster: prod)", "HC1 (cluster: prod)")))   


##------------------------------------  Robustness Tests  ------------------------------------ 


## Placebo Test: Fake Event Date (09/20/2024)
    
    df_placebo <- df_excl %>% filter(timeToTreat < 0)
    df_placebo$post_falso <- as.numeric(df_placebo$timeToTreat >= -1)
    
    placebo_var_ols <- lm(price_variation_c ~ post_falso + imported + post_falso*imported + stock_dummy + max_consecutivos_sin_stock, 
                          data = df_placebo)
    placebo_log_ols <- lm(log_precio ~ post_falso + imported + post_falso*imported + stock_dummy + max_consecutivos_sin_stock, 
                          data = df_placebo)
    stargazer(placebo_var_ols, placebo_log_ols, type = "text", title = "Placebo Test OLS (Fake Date: 09/20/2024)")
    
## Placebo Test: Fake Treatment Group
    
    df_control <- df_excl %>% 
    filter(imported == 0 )
    
    set.seed(135)  
    df_control <- df_control %>% 
      group_by(prod) %>% 
      mutate(falso_importado = rbinom(1, 1, 0.5)) %>% 
      ungroup() 
    df_panel_control <- pdata.frame(df_control, index = c("prod", "time"))
    
    placebo_var <- plm(price_variation_c ~ post  + falso_importado*post  + dolar_variation + stock_dummy + max_consecutivos_sin_stock , data = df_panel_control, model = "within", effect = "twoways")
    
    placebo_log <- plm(log_precio ~ post  + falso_importado*post  + dolar_variation + stock_dummy + max_consecutivos_sin_stock , data = df_panel_control, model = "within", effect = "twoways")
    stargazer(placebo_var, placebo_log, type = "text", 
              title = "Control Group Placebo Test (National Products Only)")
    
## Regressions by Product Category (Heterogeneity Analysis)
    
    categories <- unique(df_excl$categoria_producto)
    
    models_var <- list()
    models_log <- list()
    
    for (cat in categories) {
      df_cat <- df_excl %>% filter( categoria_producto == cat)
      
      models_var[[cat]] <- lm(price_variation_c ~ post + imported + post*imported + stock_dummy + max_consecutivos_sin_stock , data = df_cat)
      models_log[[cat]] <- lm(log_precio ~ post + imported + post*imported  + stock_dummy + max_consecutivos_sin_stock , data = df_cat)
      }
    
    
    # Visualize post:imported coefficients
    coefs_var <- sapply(models_var, function(m) coef(m)["post:imported"])
    se_var <- sapply(models_var, function(m) summary(m)$coefficients["post:imported", "Std. Error"])
    df_coefs <- data.frame(category = names(coefs_var), estimate = coefs_var, se = se_var)
    
    ggplot(df_coefs, aes(x = category, y = estimate)) + 
      geom_point() + geom_errorbar(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se)) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      theme_minimal() + ggtitle("post:imported Coefficients by Category - price_variation_c") + 
      coord_flip()
    
    
    # Extract coefficients, SEs, p-values, Adj. R-squared, and Observations into a summary table
    results_table <- lapply(names(models_var), function(cat) {
      model <- models_var[[cat]]
      coef <- coef(model)["post:imported"]
      se <- summary(model)$coefficients["post:imported", "Std. Error"]
      pval <- summary(model)$coefficients["post:imported", "Pr(>|t|)"]
        
      # Bonferroni correction for multiple comparisons
      pval_adj <- pval * length(models_var)
      stars <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
      obs <- nobs(model)
      r2_adj <- summary(model)$adj.r.squared
      data.frame(
        Category = cat,
        Coefficient = round(coef, 3),
        Std.Error = round(se, 3),
        P.Value = round(pval, 3),
        Significance = stars,
        Observations = obs,
        Adj.R2 = round(r2_adj, 3)
      )
    }) %>% bind_rows()
    
    # Sort by coefficient magnitude
    results_table <- results_table %>% arrange(desc(abs(Coefficient)))
    
    
    

    
