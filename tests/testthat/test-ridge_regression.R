test_that("ridge regression correctly calculates coefficients", {

  my_result <- mtcars %>%
    dplyr::select(mpg, hp, cyl) %>%
    ridge_regression(mpg, lambda = 0.5)

  mtcars2 <- mtcars %>%
    dplyr::select(mpg, cyl, hp) %>%
    dplyr::mutate(
      cyl = scale(cyl),
      hp = scale(hp)
    )

  mass_result <- MASS::lm.ridge(mpg ~ hp + cyl, data = mtcars2, lambda = .5*32/31)

  expect_equal(coef(mass_result)[['hp']], my_result$hp,
               tolerance = 0.05, scale = abs(my_result$hp))
  expect_equal(coef(mass_result)[['cyl']], my_result$cyl,
               tolerance = 0.05, scale = abs(my_result$cyl))

  expect_equal(coef(mass_result)[[1]], my_result$Intercept,
               tolerance = 0.05, scale = abs(my_result$Intercept))
})


test_that("ridge regression returns proper data frame for many lambdas", {

  lambdas <- 0:10*.1

  my_result <- mtcars %>%
    dplyr::select(mpg, hp, cyl) %>%
    ridge_regression(mpg, lambda = lambdas)

  expect_equal(names(my_result), c("Intercept", "hp", "cyl", "lambda"))
  expect_equal(my_result$lambda, lambdas)
})


test_that("find_best_lambda returns proper data frame", {

  set.seed(3920)

  lambdas <- 0:10*.1

  rand <- sample(1:32, 16)

  train_dat <- mtcars %>%
    dplyr::select(mpg, hp, cyl) %>%
    dplyr::slice(rand)

  test_dat <- mtcars %>%
    dplyr::select(mpg, hp, cyl) %>%
    dplyr::slice(-rand)

  my_result <- find_best_lambda(train_dat, test_dat, mpg, lambda = lambdas)

  expect_equal(names(my_result), c("lambda", "error"))
  expect_equal(my_result$lambda, lambdas)
  expect_equal(class(my_result$error), "numeric")
})

