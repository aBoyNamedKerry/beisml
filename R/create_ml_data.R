#' Data divider for machine learning
#'
#' `create_ml_data()`
#'
#' @param df a data.frame object
#' @param proportion the proportion to allocate to training, test and validation.
#' Default split is training 0.8, test 0.2 and validation 0
#' @param validation whether to include a validation split to the data
#' @param seed_val set the seed value for reproducibility default 1537
#'
#' @return Returs up to 3 different data sets straight into global environment
#' @export
#'
#' @examples
#' create_ml_data(df = mtcars)

create_ml_data <- function(df,
                                   proportion =  c(0.8,0,0.2),
                                   validation = FALSE,
                                   seed_val = 1537){

  if(is.data.frame(df) == FALSE){stop("df must be of class data.frame")}
  if(proportion[1] + proportion[2] + proportion[3] != 1) stop("proportions don't add to 1")


  set.seed(seed_val)

  training_prop <- proportion[1]
  validation_prop <- proportion[2]
  test_prop <- proportion[3]

  df$index <- sample(1: nrow(df))

  training_df <<- df[df$index <= nrow(df)* training_prop,]

  test_df <<- df[df$index > nrow(df) * (training_prop + validation_prop),]

  if(validation == TRUE) {

    validation_df <<- df[df$index > nrow(df)* training_prop &
                          df$index <= nrow(df) * (training_prop + validation_prop),]

  }


}
