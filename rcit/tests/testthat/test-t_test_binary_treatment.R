library(magrittr)
library(tibble)

filename <- file.path("..", "testdata", "nswdata.txt")
nsw <- read.table(filename, header = TRUE) %>% as_tibble()
result <- rcit::t_test_binary_treatment(nsw, "treatment", "earnings78")

test_that("multiplication works", {
    expect_equal(2 * 2, 4)
})

test_that("loading works", {
    expect_true("tbl" %in% class(nsw))
})

test_that("produces tibble", {
    expect_true("tbl" %in% class(result))
    expect_true("p.value" %in% names(result))
})

test_that("accurate p-values", {
    expect_equal(round(result$p.value[[1]], 4), 0.7216)
})
