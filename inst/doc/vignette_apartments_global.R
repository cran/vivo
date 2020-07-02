## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE, warning=FALSE, error=FALSE, message=FALSE----------------
library("ggplot2")

## ---- warning = FALSE, echo = FALSE, message = FALSE, include = TRUE----------
library("DALEX")
data(apartments)
head(apartments)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
library("randomForest")
apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms, data = apartments)
explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest[,2:5], y = apartmentsTest$m2.price)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
profiles <- model_profile(explainer_rf)
plot(profiles) 

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
library("vivo")
measure <- global_variable_importance(profiles)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
plot(measure)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor +
                                      no.rooms, data = apartments)
explainer_lm <- explain(apartments_lm_model,
                        data = apartmentsTest[,2:5], y = apartmentsTest$m2.price)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
profiles_lm <- model_profile(explainer_lm)

measure_lm <- global_variable_importance(profiles_lm)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
plot(measure_lm, measure, type = "lines")

