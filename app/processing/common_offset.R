#' Common Offset
#'
#' @description This algorithm allows to rearrange the recorded multi-source and multi-receptor measurements into profiles with common offsets
#'
#' @param dataset_name 
#' 
#' @param common_offset It is the algebraic distance that separates the source and the geophone for each shot
#'
#' @param offset_exp It is the algebraic distance that separates each geophone at regular intervals
#'
#' @param origin.file File that determines the origin at the ordinate of the shots
#'
#' @param normalized To normalize signals
#'
#' @param fft        Fast Fourrier Transform all of signals. If plot is TRUE, fft table will plot also.
#'
#' @param time_exp   To normalize fft by 2*T/dt
#'
#' @export
#'
#' @examples \dontrun{
#' # In meter
#'common_offset <- 6.5
#'experimental_offset <- 1.5
#' # Use integer
#'origine <- 5
#'
#'common.offset(common_offset = common_offset,
#'              offset_exp = experimental_offset,
#'              normalize = True,
#'              fft = TRUE,
#'              dir_output = "C:/Users/Name/Desktop/")
#'
#'data.common.offset$offset_6.5
#' }
#'
common_offset <- function(dataset_name, 
                          common_offset,
                          offset_exp,
                          time_exp,
                          origin.file,
                          normalized = TRUE,
                          fft = FALSE
                          )
{
  dir_output <- paste0(settings$datapath, dataset_name)
  # The number of columns is necessarily an integer
  if(common_offset%%offset_exp != 0){
    return("The initial conditions are incorrect")
  } else {
    
    # Variable declaration
    no_col <- as.integer(common_offset/offset_exp) + 1
    no_geophone <- ncol(.GlobalEnv$processing$toProcess$`1`)
    nb_row <- nrow(.GlobalEnv$processing$toProcess$`1`)
    space = as.numeric(offset_exp/2)
    
    # Initialization of new data
    no_file <- no_col + origin.file 
    offset_table <- if(normalized){
      data.frame(.GlobalEnv$processing$toProcess[[toString(no_file)]][,1])
    } else {
      data.frame(normalize(.GlobalEnv$processing$toProcess[[toString(no_file)]][,1], 0, offset_exp) + space)
    }
    
    if(fft){fft_table <- data.frame(.GlobalEnv$processing$toProcess[[toString(no_file)]][,1])}
    i <- 2;
    
    while(i != no_col){
      tmp_tab <- if(normalized){
        data.frame(.GlobalEnv$processing$toProcess[[toString(no_file)]][,i])
      } else {
        data.frame(normalize(.GlobalEnv$processing$toProcess[[toString(no_file)]][,i], 0, offset_exp) + space)
      }
      if(fft){
        fft_tmp_table <- data.frame(.GlobalEnv$processing$toProcess[[toString(no_file)]][,i])
        fft_table <- cbind(fft_table, fft_tmp_table)
      }
      
      offset_table <- cbind(offset_table, tmp_tab)
      i = i + 1; no_file = no_file + 1; space = space + offset_exp;
    }
    
    ofcol <- 1 + no_col
    no_file <- origin.file
    
    while(ofcol != no_geophone + 1){
      tmp_tab <- if(normalized){
        data.frame(.GlobalEnv$processing$toProcess[[toString(no_file)]][,ofcol])
      } else {
        data.frame(normalize(.GlobalEnv$processing$toProcess[[toString(no_file)]][,ofcol], 0, offset_exp) + space)
      }
      if(fft){
        fft_tmp_table <- data.frame(.GlobalEnv$processing$toProcess[[toString(no_file)]][,ofcol])
        fft_table <- cbind(fft_table, fft_tmp_table)
      }
        
      offset_table <- cbind(offset_table, tmp_tab)
      ofcol = ofcol + 1; no_file = no_file + 1; space = space + offset_exp;
    }
    
    if(normalized){
      offset_table <- data.frame(normalize(offset_table, 0, offset_exp*2))
      space = as.numeric(offset_exp/2)
      for(i in 1:ncol(offset_table)){
        offset_table[,i] <- data.frame(offset_table[,i] + space)
        space = space + offset_exp
      }
    }
    
    write.table(
       offset_table,
       file = paste0(dir_output, "/offsets/offset_",common_offset)
    )
    
    
    if(fft){
      fft_table <- .module_fft(offset_table, !normalized ,time_exp)
      write.table(
        fft_table,
        file = paste0(dir_output, "/fft/fft_offset_", common_offset)
      )
    }
  }
}
