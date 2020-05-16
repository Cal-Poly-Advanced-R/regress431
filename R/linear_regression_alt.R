#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory){
  iters <- 10000
  betas <- matrix(0, 2)

  x <- dat %>% select({{explanatory}})
  x <- as.matrix(cbind(intercept = 1, x))
  y <- dat %>% select({{response}})

  for(i in (1:iters)){

    if (i %% 1000 == 1) {print(betas)}
    pred <- x %*% betas
    error <- (1/nrow(x))*sum((y-pred)^2)

    deriv_b1 <- (-2/nrow(x))*sum(x[,2]*(y - pred))
    deriv_int <-  (-2/nrow(x))*sum((y - pred))

    betas[1] <- (betas[1] - .0001*deriv_int)
    betas[2] <- (betas[2] - .0001*deriv_b1)
  }

  betas <- data.frame(t(betas))
  names(betas) <- names(x)
  results <- betas
  return(results)
}


#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_gd <- function(dat, response){

  iters <- 10000
  betas <- matrix(0, 2)

  x <- dat %>% select({{explanatory}})
  x <- as.matrix(cbind(intercept = 1, x))
  y <- dat %>% select({{response}})

  for(i in (1:iters)){

    if (i %% 1000 == 1) {print(betas)}
    pred <- x %*% betas
    error <- (1/nrow(x))*sum((y-pred)^2)

    deriv_b1 <- (-2/nrow(x))*sum(x[,2]*(y - pred))
    deriv_int <-  (-2/nrow(x))*sum((y - pred))
    for(i in (1:iters)){
    betas[1] <- (betas[1] - .0001*deriv_int)
    betas[2] <- (betas[2] - .0001*deriv_b1)
    }


  betas <- data.frame(t(betas))
  names(betas) <- names(x)
  results <- data.frame(betas)
  return(results)

}

#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_qr <- function(dat, response) {



  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}
