test_that("simple linear regression by gradient descent is (approximately) correct", {
  
  my_result <- mtcars %>%
    slr_gd(mpg, hp)
  
  mass_result <- lm(mpg ~ hp, data = mtcars)
  
  expect_equal(coef(mass_result)[['hp']], my_result$hp,
               tolerance = 0.05, scale = abs(my_result$hp))
  expect_equal(coef(mass_result)[[1]], my_result$Intercept,
               tolerance = 0.05, scale = abs(my_result$Intercept))
  
})


test_that("multiple linear regression by gradient descent is correct", {
  
  my_result <- mtcars %>%
    dplyr::select(mpg, hp, cyl) %>%
    mlr_gd(mpg)
  
  mass_result <- lm(mpg ~ hp + cyl, data = mtcars)
  
  expect_equal(coef(mass_result)[['hp']], my_result$hp,
               tolerance = 0.05, scale = abs(my_result$hp))
  expect_equal(coef(mass_result)[['cyl']], my_result$cyl,
               tolerance = 0.05, scale = abs(my_result$cyl))
  expect_equal(coef(mass_result)[[1]], my_result$Intercept,
               tolerance = 0.05, scale = abs(my_result$Intercept))
  
  
})


test_that("multiple linear regression by matrix decomposition is correct", {
  
  my_result <- mtcars %>%
    dplyr::select(mpg, hp, cyl) %>%
    mlr_qr(mpg)
  
  mass_result <- lm(mpg ~ hp + cyl, data = mtcars)
  
  expect_equal(coef(mass_result)[['hp']], my_result$hp,
               tolerance = 0.05, scale = abs(my_result$hp))
  expect_equal(coef(mass_result)[['cyl']], my_result$cyl,
               tolerance = 0.05, scale = abs(my_result$cyl))
  expect_equal(coef(mass_result)[[1]], my_result$Intercept,
               tolerance = 0.05, scale = abs(my_result$Intercept))
  
  
})

