#' Fill the hole
#' 
#' It is possible that during the manipulations, some acquisitions are not made with the same number of points. 
#' Whether it is a human or machine error, all data sets must have the same dimensions.
#'
#' @param data The dataset that needs to be filled
#' @param no_row Number of rows that the dataset must have 
#'
fill_hole <- function(data, no_row){
  no_col <- ncol(data)
  no_row1 <- no_row; no_row2 <- nrow(data);
  
  fill_row <- no_row1 - no_row2
  fill_zero <- matrix(0, nrow = fill_row, ncol = no_col)
  
  data <- rbind(data, fill_zero)
  return(data)
}