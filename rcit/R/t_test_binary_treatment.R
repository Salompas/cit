library(magrittr)
library(tibble)
library(dplyr)
library(purrr)

diff_in_means <- function(data, split_by) {
  t.test(data[split_by], data[!split_by])$p.value
}

#' @title Difference in means test
#'
#' @description Test if averages from treatment and control groups are statistically equal.
#' @param data Tibble
#' @param treatment Column with binary treatment indicator
#' @param ignore_cols String vector with columns to be ignored in the test
#' @keywords difference-in-means, t-test
#' @export
#' @examples
#' t_test_binary_treatment(data, "treatment", "output")
t_test_binary_treatment <- function(data, treatment, ignore_cols) {
  selected <- data %>% select(-c(treatment, ignore_cols))
  treated <- data[[treatment]] == 1
  mean_treatment <- selected[treated, ] %>%
    map(mean) %>%
    stack()
  mean_control <- selected[!treated, ] %>%
    map(mean) %>%
    stack()
  mean_p_value <- map(selected, diff_in_means, split_by = treated) %>% stack()
  reduce(list(mean_treatment, mean_control, mean_p_value), left_join, by = "ind") %>%
    as_tibble() %>%
    rename(covariate = ind, treatment = values.x, control = values.y, p.value = values) %>%
    select(covariate, treatment, control, p.value)
}
