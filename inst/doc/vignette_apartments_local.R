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
new_apartment <- data.frame(construction.year = 1998, surface = 88, floor = 2L, no.rooms = 3)
predict(apartments_rf_model, new_apartment)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
library("ingredients")
profiles <- predict_profile(explainer_rf, new_apartment)
plot(profiles) + show_observations(profiles)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
library("vivo")
measure <- local_variable_importance(profiles, apartments[,2:5], 
            absolute_deviation = TRUE, point = TRUE, density = TRUE)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
plot(measure)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
measure_2 <- local_variable_importance(profiles, apartments[,2:5], 
            absolute_deviation = FALSE, point = TRUE, density = TRUE)
measure_3 <- local_variable_importance(profiles, apartments[,2:5], 
            absolute_deviation = FALSE, point = TRUE, density = FALSE)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
plot(measure, measure_2, measure_3, color = "_label_method_")

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
plot(measure, measure_2, measure_3, color = "_label_method_", type = "lines")

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor +
                                      no.rooms, data = apartments)
explainer_lm <- explain(apartments_lm_model,
                        data = apartmentsTest[,2:5], y = apartmentsTest$m2.price)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
profiles_lm <- predict_profile(explainer_lm, new_apartment)

measure_lm <- local_variable_importance(profiles_lm, apartments[,2:5], 
            absolute_deviation = TRUE, point = TRUE, density = TRUE)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE---------
plot(measure, measure_lm, color = "_label_model_", type = "lines")

