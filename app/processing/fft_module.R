#' Module Fast Fourrier Transform
#'
#' @param data from \link{common_offset}
#'
#' @param normalized To normalize fft by 2*T/dt
#'
#' @param time_exp To define normalization
#'
#' @export
#'
.module_fft <- function(data,
                        normalized = FALSE,
                        time_exp = NULL){
  
  no_col <- ncol(data)
  no_row <- nrow(data)
  
  # Initialization
  y <- fft(data[,1])
  y.tmp <- data.frame(Mod(y))
  if(normalized){y.tmp <- data.frame(normalize(y.tmp,0,1))}
  mod_sig <- data.frame(y.tmp)
  
  for(i in 2:no_col){
    y <- fft(data[,i])
    y.tmp <- data.frame(Mod(y))
    if(normalized){y.tmp <- data.frame(normalize(y.tmp,0,1))}
    mod_sig <- cbind(mod_sig,y.tmp)
  }
  
  return(mod_sig)
}
