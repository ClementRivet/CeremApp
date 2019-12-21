#' Make visualization for common offset
#'
#' @param title_plot Graphic's title
#'
#' @param data Obtained by \link{common_offset}
#'
#' @param time_exp Duration of the acquisition, not used if plots is FALSE
#'
#' @param max_time Define time axe, 0 to max_time in second
#' 
#' @param fill if 
#'
#' @export
#'
#' @examples \dontrun{
#'
#' data_table <-data.common.offset$offset_6.5
#' # In second
#' aqcuisition_time <- 2
#'
#' # If save_plot = TRUE
#' plot.common.offset.refrac(title_plot = "my plot",
#'                           data = data_table,
#'                           time_exp = aqcuisition_time,
#'                           max_time = 0.5
#'                           )
#'
#'
#' plots[["my plot"]]
#'
#' # Else
#'
#' my_plot <- plot.common.offset.refrac(data = data_table,
#'                                      time_exp = aqcuisition_time,
#'                                      max_time = 0.5,
#'                                      save_plot = FALSE )
#'
#' my_plot
#'
#'}
#'
plot_refrac <- function(title_plot,
                        data,
                        time_exp,
                        fill = TRUE,
                        max_time = NULL){
  # Variables declarations
  step <- nrow(data); no_trace <- ncol(data);
  T <- time_exp
  dt <- T/step
  # Abscisse temporelle
  t = seq(dt, T, by = dt)
  
  y.axis <- if(!is.null(max_time)){
    T/max_time
    } else {1}
  
  P_REFRAC <- plot_ly(data = data)%>%
    layout(title = title_plot,
           yaxis = list(autorange="reversed"),
           paper_bgcolor= NULL
    )
  
  fill <- if(isTRUE(fill))'tozeroy' else NULL
  for(k in 1:no_trace){
    dfk <- data.frame(Temps = t[1:length(t)/y.axis] , Offset = data[1:length(t)/y.axis,k])
    P_REFRAC <- add_trace(P_REFRAC, y = ~Temps, x = ~Offset, data = dfk,
                          type = "scatter", mode = "lines",
                          fill = fill , fillcolor = '#000000',
                          name = paste0("Geophone ", k),
                          line = list(color = 'rgb(0, 0, 0)',width = "1px"))
  }

  return(P_REFRAC)

}









#' Plot frequency domain of common offset
#'
#' @param data_table Obtained by \link{common_offset} if fft = TRUE
#'
#' @param time_exp Duration of the acquisition, not used if plots is FALSE
#'
#' @param save_plot Save plot in plots environment
#'
#' @export
#'
#' @examples \dontrun{
#'
#' data_table <- my_data
#' # In second
#' aqcuisition_time <- 2
#'
#' # If save_plot = TRUE
#' plot.fft(title_plot = "my fft plot",
#'          data_table = data_table,
#'          time_exp = aqcuisition_time
#'          )
#' )
#'
#' plots[["my fft plot"]]
#'
#' # Else
#'
#' my_plot <- plot.fft(data_table = data_table,
#'                     time_exp = aqcuisition_time,
#'                     save_plot = FALSE )
#'
#' my_plot
#' }
#'
plot_fft <- function(data_table,
                     time_exp,
                     save_plot = TRUE){
  
  T <- time_exp
  step <- nrow(data_table)
  dt <- T/step
  
  f <- 1:length(data_table[,1])/T
  
  minF <- which(f == 1)
  maxF <- which(f == 150)
  
  # On initialize le plot
  no_trace <- ncol(data_table)
  P_FFT <- plot_ly(data = data_table) %>%
    layout(title = "Common offset")
  
  for(k in 1:no_trace){
    dfk <- data.frame(Frequences = f[minF:maxF] ,
                      Amplitudes = data_table[minF:maxF,k])
    
    P_FFT <- add_trace(P_FFT, y = ~Amplitudes, x = ~Frequences, data = dfk,
                       type = "scatter", mode = "lines",
                       x = "Frequencies en Hz",
                       name = paste0("Geophone ", k))
  }
  
  if(save_plot){
    .GlobalEnv$plots <- if(!"plots" %in% ls(.GlobalEnv)) new.env()
    .GlobalEnv$plots[[paste0("fft_offset_")]] <- P_FFT
  } else {return(P_FFT)}
}
