library(vegetools)

context("Check calculate_cover function")

test_that("species and bare cover are correct", {

  ## create dataframe
  df <- structure(list(transect = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                    1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                                    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
                                    3, 3, 3, 3, 3, 3, 3),
                       section = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                       species1 = c(0.1, 10.3, 11.1, 22, 24.4, 35.2, 38, 41.8, 43.5, 44, 47.5, 61.3, 62.9, 66.8, 71, 75.4, 78.8, 79.4, 89.3, 98.5, 9.4, 13.1, 17.5, 18.8, 32, 33.3, 34.4, 46.7, 48.3, 48.9, 51.2, 60, 60.9, 65.3, 65.7, 78.2, 88.6, 89, 91.4, 95.4, 6.1, 9.1, 14.2, 14.7, 15.4, 22, 30.1, 30.8, 32, 36.9, 40.5, 41.1, 54.9, 58.5, 64.8, 72.1, 93.5, 94.8, 95.4, 98.4),
                       species2 = c(14.2, 52.1, 61.9, 66, 67.3, 69, 73.7, 89.1,
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                    NA, NA, NA, NA, NA, NA, NA)),
                  .Names = c("transect", "section", "species1", "species2"),
                  row.names = c(NA, -60L), class = "data.frame")

  ## tests
  sp1 <- calculate_cover(df, split.cols = "transect", first.spcol = 3, tr.length = 100,
                         sort.cols = FALSE)$species1
  expect_equal(sp1, c(68.1, 49.3, 31.5))

  sp2 <- calculate_cover(df, split.cols = "transect", first.spcol = 3, tr.length = 100,
                         sort.cols = FALSE)$species2
  expect_equal(sp2, c(59.1, 0, 0))

  bare <- calculate_cover(df, split.cols = "transect", first.spcol = 3, tr.length = 100,
                          bare = TRUE, precision = 0.1, sort.cols = FALSE)$bare
  expect_equal(bare, c(5.7, 50.7, 68.5))

  prop <- calculate_cover(df, split.cols = "transect", first.spcol = 3, tr.length = 100,
                          bare = TRUE, precision = 0.1, sort.cols = FALSE,
                          prop = TRUE)
  expect_equal(prop[2:4], data.frame(species1 = c(0.681, 0.493, 0.315),
                                species2 = c(0.591, 0, 0),
                                bare = c(0.057, 0.507, 0.685)))



})


