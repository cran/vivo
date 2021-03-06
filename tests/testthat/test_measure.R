context("Check local_variable_importance() function")

source("objects_for_tests.R")

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = TRUE, point = TRUE, density = TRUE)))
})

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = TRUE, point = TRUE, density = FALSE)))
})

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = TRUE, point = FALSE, density = TRUE)))
})

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = FALSE, point = TRUE, density = TRUE)))
})

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = FALSE, point = TRUE, density = FALSE)))
})

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = TRUE, point = FALSE, density = FALSE)))
})

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = FALSE, point = FALSE, density = TRUE)))
})

test_that("local_variable_importance", {
  expect_true(is.data.frame(local_variable_importance(cp, apartments[, 2:5], absolute_deviation = FALSE, point = FALSE, density = FALSE)))
})

test_that("local_variable_importance_caterogical", {
  expect_message(local_variable_importance(cp_2, apartments, absolute_deviation = TRUE, point = TRUE, density = TRUE), "The measure of local variable importance is calculated only for numerical variables.")
})

test_that("local_variable_importance_data.frame", {
  expect_error(local_variable_importance(cp, as.matrix(apartments[, 2:5])))
})

test_that("ceteris_paribus_explainer", {
  expect_error(local_variable_importance(explainer_rf, apartments[, 2:5]))
})

test_that("calculate_weight_cp", {
  expect_error(calculate_weight(explainer_rf, apartments[, 2:5], split))
})

test_that("calculate_weight_data.frame", {
  expect_error(calculate_weight(cp, as.matrix(apartments[, 2:5]), split))
})

test_that("calculate_weight_data.frame", {
  expect_error(calculate_weight(cp, "name", split))
})

test_that("calculate_weight_list", {
  expect_error(calculate_weight(cp, apartments[, 2:5], unlist(split)))
})

test_that("calculate_variable_split", {
  expect_error(calculate_variable_split(apartments[, 2:5], "name", 100))
})

test_that("calculate_variable_split", {
  expect_true(is.list(calculate_variable_split(apartments[, 2:5], colnames(apartments)[2:5], 100)))
})

test_that("partial_dependece_explainer", {
  expect_error(global_variable_importance(cp))
})
