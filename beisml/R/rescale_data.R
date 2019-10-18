#' Rescale numeric data by standardising or normalising as appropriate
#'
#' @param df a data.frame object
#' @param scale Choose type of recaling, default is to 'standardise'
#' so rescale using the standard deviation. choosing 'normalise'
#' will scale the data in range [0,1] and therefore outliers will
#' be less relavnt.
#'
#' @return A data frame where numeric data has been rescaled
#' @export
#'
#' @import dplyr
#'
#' @examples
#' rescale_data(df = mtcars, scale = "standardise")
rescale_data <-
  function(df, scale = c("standardise", "normalise")) {

    match.arg(scale)

    if(scale == "standardise"){

    df %>% dplyr::mutate_if(is.numeric, ~(.x - mean(.x)) / sd(.x))

    } else {

      df %>% dplyr::mutate_if(is.numeric, ~(.x - min(.x)) / (max(.x) - min(.x)))

    }


  }
