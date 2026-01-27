
## ANALISIS DEL IMPACTO EN PRECIOS DE ALIMENTOS Y BEBIDAS IMPORTADAS LUEGO DE LA REDUCCION 
## DEL IMPUESTO PAIS 


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

df <- read_csv("df_clean.csv",show_col_types = FALSE)
sapply(df, class)
summary(df)

#Elimino las categorias que violan el principio de paralelismo
df_excl <- df %>% filter(!categoria_producto %in% c("Galletitas", "Enlatados"))

# Crear panel para análisis FE
df_panel <- pdata.frame(df_excl, index = c("prod", "time"))

##------------------------------------  Chequeos  ------------------------------------ 

    model_temp <- lm(price_variation_c ~ post + imported + timeToTreat + dolar_variation + stock_dummy + max_consecutivos_sin_stock , data = df_excl)
    vif(model_temp)

##------------------------------------  Modelos  ------------------------------------  

## Modelo Básico 

    did_log_precio <- plm(log_precio ~ post + imported + post*imported, data = df_panel, model = "within", effect = "twoways")
    did_price_variation <- plm(price_variation_c ~ post + imported + post*imported, data = df_panel, model = "within", effect = "twoways")
    ## Errores robustos
    robust_se_log_precio <- coeftest(did_log_precio, vcov. = vcovHC, type = "HC1")
    robust_se_price_variation <- coeftest(did_price_variation, vcov. = vcovHC, type = "HC1")

## Modelo Básico + Variables de control Control
    
    did_log_precio3 <- plm(log_precio ~ post + imported + post*imported +  stock_dummy + max_consecutivos_sin_stock , data = df_panel, model = "within", effect = "twoways")
    did_price_variation3 <- plm(price_variation_c ~ post + imported + post*imported + stock_dummy + max_consecutivos_sin_stock , data = df_panel, model = "within", effect = "twoways")
    
    ## Errores robustos
    robust_se_log_precio3 <- coeftest(did_log_precio3, vcov. = vcovHC, type = "HC1")
    robust_se_price_variation3 <- coeftest(did_price_variation3, vcov. = vcovHC, type = "HC1")
    
## Tabla con stargazer usando SE robustos
    stargazer(did_log_precio, did_price_variation, did_log_precio3, did_price_variation3,
              type = "text",
              se = list(sqrt(diag(vcovHC(did_log_precio, type = "HC1"))),
                        sqrt(diag(vcovHC(did_price_variation, type = "HC1"))),
                        sqrt(diag(vcovHC(did_log_precio3, type = "HC1"))),
                        sqrt(diag(vcovHC(did_price_variation3, type = "HC1")))),
              title = "Modelos DiD: Impacto de la Reducción del Impuesto PAIS",
              add.lines = list(c("Efectos Fijos", "Prod, Time", "Prod, Time", "Prod, Time", "Prod, Time"),
                               c("Errores Robustos", "HC1 (cluster: prod)", "HC1 (cluster: prod)", "HC1 (cluster: prod)", "HC1 (cluster: prod)")))   


##------------------------------------  Robustez  ------------------------------------ 


## Placebo test con Fecha de evento falsa 20/09/2024
    
    df_placebo <- df_excl %>% filter(timeToTreat < 0)
    df_placebo$post_falso <- as.numeric(df_placebo$timeToTreat >= -1)
    
    placebo_var_ols <- lm(price_variation_c ~ post_falso + imported + post_falso*imported + stock_dummy + max_consecutivos_sin_stock, 
                          data = df_placebo)
    placebo_log_ols <- lm(log_precio ~ post_falso + imported + post_falso*imported + stock_dummy + max_consecutivos_sin_stock, 
                          data = df_placebo)
    stargazer(placebo_var_ols, placebo_log_ols, type = "text", title = "Placebo Test OLS (Fecha Falsa: 20/09/2024)")
    
## Placebo test con grupo de tratamiento falso
    
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
              title = "Placebo Test en Grupo Control (No Importados, Sin Filtrado por Fecha)")
    
## Regresión por categoía de producto
    
    categories <- unique(df_excl$categoria_producto)
    
    models_var <- list()
    models_log <- list()
    
    for (cat in categories) {
      df_cat <- df_excl %>% filter( categoria_producto == cat)
      
      models_var[[cat]] <- lm(price_variation_c ~ post + imported + post*imported + stock_dummy + max_consecutivos_sin_stock , data = df_cat)
      models_log[[cat]] <- lm(log_precio ~ post + imported + post*imported  + stock_dummy + max_consecutivos_sin_stock , data = df_cat)
      }
    
    
    # Visualizar coeficientes de post:imported
    coefs_var <- sapply(models_var, function(m) coef(m)["post:imported"])
    se_var <- sapply(models_var, function(m) summary(m)$coefficients["post:imported", "Std. Error"])
    df_coefs <- data.frame(category = names(coefs_var), estimate = coefs_var, se = se_var)
    
    ggplot(df_coefs, aes(x = category, y = estimate)) + 
      geom_point() + geom_errorbar(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se)) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      theme_minimal() + ggtitle("Coeficientes post:imported por Categoría - price_variation_c") + 
      coord_flip()
    # Repite para otros outcomes
    
    # Extraer coeficientes, errores estándar, p-valores, R² ajustado, observaciones
    results_table <- lapply(names(models_var), function(cat) {
      model <- models_var[[cat]]
      coef <- coef(model)["post:imported"]
      se <- summary(model)$coefficients["post:imported", "Std. Error"]
      pval <- summary(model)$coefficients["post:imported", "Pr(>|t|)"]
      # Corrección Bonferroni
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
    
    # Ordenar por magnitud del coeficiente
    results_table <- results_table %>% arrange(desc(abs(Coefficient)))
    
    
    
    