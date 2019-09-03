## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE, warning=FALSE, error=FALSE, message=FALSE-----------
library("dplyr")
library("ggplot2")

## ---- warning = FALSE, echo = FALSE, message = FALSE, include = TRUE-----
library("DALEX")
data(apartments)
head(apartments)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE----
library("randomForest")
apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor +
                                      no.rooms, data = apartments)
explainer_rf <- explain(apartments_rf_model,
                        data = apartmentsTest[,2:5], y = apartmentsTest$m2.price)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE----
new_apartment <- data.frame(construction.year = 1998, surface = 88, floor = 2L, no.rooms = 3)
predict(apartments_rf_model, new_apartment)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE----
library("ingredients")
profiles <- ingredients::ceteris_paribus(explainer_rf, new_apartment)
plot(profiles) + show_observations(profiles)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE----
library("vivo")
measure <- local_variable_importance(profiles, apartments[,2:5], 
            absolute_deviation = TRUE, point = TRUE, density = TRUE)

## ---- warning = FALSE, error = FALSE, message = FALSE, include = TRUE----
plot(measure)

