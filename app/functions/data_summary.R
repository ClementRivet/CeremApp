#' Summary of your experimental conditions 
#'
#' @param no_table Number of datasets selected
#' @param offset Experimental offset : distance between each geophone
#' @param exp_time Duration of acquisition
#'
sweet_msg_verif <- function(no_table, offset, exp_time, origine_file){
  sum_table <- data.frame(matrix(data = c(no_table, offset, exp_time, origine_file),nrow = 4, ncol = 1), 
                          row.names = c("Total datasets", "Spatial Incrementation", "Time", "The reference points"))
  colnames(sum_table) <- "Experimental settings"
  return(sum_table)
}


#' Title
#'
#' @param summary_list List containing all parameters  
#'
data_summary <- function(summary_list){
  exp_cond <- sweet_msg_verif(summary_list[1], summary_list[2], summary_list[3], summary_list[4])
  exp_cond <- rbind(exp_cond,"Number of geophone" = summary_list[5])
  exp_cond <- rbind(exp_cond,"Number of acquisition points" = summary_list[6])
  #exp_cond <- rbind(exp_cond,"Common offset" = "No processed")
  #exp_cond <- rbind(exp_cond,"Date of importation" = as.Date.numeric(date())) 
  exp_cond <- data.frame(exp_cond)
  
  return(exp_cond)
}

#' Initialization of the table summarizing the common offset processed
#'
#' @param dataset_name 
#' 
init_data_common_offset <- function(dataset_name){
  datapath <- paste0(settings$datapath, dataset_name, "/data_common_offset")
  
  offset_treated <- data.frame(
    Common_Offset = as.double(),
    FFT = as.logical(),
    Normalized = as.logical(),
    Date = as.Date(as.character()),
    stringsAsFactors = FALSE
  )
  
  write.csv(
    offset_treated,
    file = datapath,
    row.names = FALSE
  )
}

add_offset <- function(data_list, dataset_name){

  new_treatment <- data.frame(Common_Offset = data_list[1],
                              FFT = data_list[2],
                              Normalized = data_list[3],
                              Date = date(),
                              stringsAsFactors = FALSE)
  processing$offset_treated <- rbind(processing$offset_treated, new_treatment)
  
  write.csv(
    processing$offset_treated,
    file = paste0(settings$datapath, dataset_name, "/data_common_offset"),
    row.names = FALSE)
  
}


init_observation <- function(dataset_name){
  datapath <- paste0(settings$datapath, dataset_name, "/obs")
  obs <- data.frame(
    Plot1 = as.numeric(),
    Plot2 = as.numeric(),
    Plot3 = as.numeric(),
    Plot4 = as.numeric(),
    Observation = as.vector(NULL),
    Date = as.Date(as.character()),
    stringsAsFactors = TRUE
  )
  
  write.csv(
    obs,
    file = datapath,
    row.names = FALSE
  )
}


add_observation <- function(list_obs, dataset_name){
  datapath <- paste0(settings$datapath, dataset_name, "/obs")
  new_obs <- data.frame(
    Plot1 = as.numeric(list_obs[1]),
    Plot2 = if(is.na(list_obs[2])) 0 else as.numeric(list_obs[2]),
    Plot3 = if(is.na(list_obs[3])) 0 else as.numeric(list_obs[3]),
    Plot4 = if(is.na(list_obs[4])) 0 else as.numeric(list_obs[4]),
    Observation = as.character(list_obs[5]),
    Date = date(),
    stringsAsFactors = TRUE
  )
  toAnalysis$obs <- rbind(toAnalysis$obs, new_obs)
  write.csv(
    new_obs,
    file = datapath,
    row.names = FALSE
  )
}

read_observation <- function(data_settings){
  #data_settings <- as.vector(data_settings)
  l_data_settings <- length(data_settings)
  
  switch(l_data_settings,
         "1" = {data_settings <- c(data_settings[1], 0, 0, 0)},
         "2" = {data_settings <- c(data_settings[1], data_settings[2], 0, 0)},
         "3" = {data_settings <- c(data_settings[1], data_settings[2], data_settings[3], 0)},
         "4" = {data_settings <- c(data_settings[1], data_settings[2], data_settings[3], data_settings[4])}
  )
  
  read_row <- which(toAnalysis$obs[,1] == data_settings[1] & toAnalysis$obs[,2] == data_settings[2] 
                    & toAnalysis$obs[,3] == data_settings[3] & toAnalysis$obs[,4] == data_settings[4], arr.ind = FALSE)
  
  if(isTRUE(length(read_row) == 0)){
    return(data.frame(Observations = "No observations"))
  } else {
    if(isTRUE(length(read_row) > 1)){
      obs <- data.frame(Observation = as.vector(NULL), Date = as.Date(as.character()))
      for(i in 1:length(read_row)){
        obs <- rbind(obs, data.frame(Observation = toAnalysis$obs[read_row[i],5], Date = toAnalysis$obs[read_row[i],6]))
      }
      return(obs)
    } else {
      single_obs <-data.frame(Observation = toAnalysis$obs[read_row,5], Date = toAnalysis$obs[read_row,6])
      return(single_obs)
    }
  }
  
}