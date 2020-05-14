test_that("simple linear regression is correct", {

  my_result <- mtcars %>%
    simple_linear_regression(mpg, hp)

  mass_result <- lm(mpg ~ hp, data = mtcars)

  expect_equal(coef(mass_result)[['hp']], my_result$hp,
               tolerance = 0.05, scale = abs(my_result$hp))
  expect_equal(coef(mass_result)[[1]], my_result$Intercept,
               tolerance = 0.05, scale = abs(my_result$Intercept))

})


test_that("multiple linear regression is correct", {

  my_result <- mtcars %>%
    dplyr::select(mpg, hp, cyl) %>%
    multiple_linear_regression(mpg)

  mass_result <- lm(mpg ~ hp + cyl, data = mtcars)

  expect_equal(coef(mass_result)[['hp']], my_result$hp,
               tolerance = 0.05, scale = abs(my_result$hp))
  expect_equal(coef(mass_result)[['cyl']], my_result$cyl,
               tolerance = 0.05, scale = abs(my_result$cyl))
  expect_equal(coef(mass_result)[[1]], my_result$Intercept,
               tolerance = 0.05, scale = abs(my_result$Intercept))


})


