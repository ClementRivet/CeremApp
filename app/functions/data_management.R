#' Creating the directory for the new dataset
#'
#' @param dataset_name Name of the new dataset
#'
dir_create <- function(dataset_name){
  dir.create(paste0(.GlobalEnv$settings$datapath , dataset_name))
  dir.create(paste0(.GlobalEnv$settings$datapath , dataset_name, "/raw"))
  dir.create(paste0(.GlobalEnv$settings$datapath , dataset_name, "/fft"))
  dir.create(paste0(.GlobalEnv$settings$datapath , dataset_name, "/offsets"))
  dir.create(paste0(.GlobalEnv$settings$datapath , dataset_name, "/boards"))
}

#' Upload new data set 
#'
#' This function will allow us to upload the raw data in order to transform them into an R object in order to access them more easily later. 
#' However, the processing time can be long, as it depends on the size of each file. 
#' It is then necessary to parallelize the process so as not to block the user. 
#' 
#' @param session Current session
#' @param dataset_name Name of the new dataset
#' @param data Data from fileInput
#' @param params It's settings from the experimental conditions 
#'
upload_data <- function(session, 
                        dataset_name, 
                        data,
                        params) 
{
  dir_create(dataset_name)
  
  k <- length(data)
  for(i in 1:k){
    updateProgressBar(
      session = session,
      id = "importing",
      value = as.integer(i/k*100)
    )
    
    raw_data <- read.table(data[i])
    raw_data <- if(ncol(raw_data) > nrow(raw_data)) transpose(raw_data) else raw_data
    
    if(i == 1){
      no_row <- nrow(raw_data)
    } else {
      if(no_row > nrow(raw_data)){
        raw_data <- fill_hole(data = raw_data, no_row = no_row)
      }
    }

    write.table(
      raw_data,
      file = paste0(settings$datapath, dataset_name, "/raw/", toString(i))
    )
  }

  exp_cond <- data_summary(c(length(data), params[1], params[2],  params[3], ncol(raw_data), nrow(raw_data)))
  
  write.table(
    exp_cond, 
    file = paste0(.GlobalEnv$settings$datapath, dataset_name,"/exp_conditions"))
  
  init_data_common_offset(dataset_name)
  init_observation(dataset_name)
  .GlobalEnv$settings$list_dataset <- append(.GlobalEnv$settings$list_dataset, dataset_name)
  
  save.image()
}


#' Reset environment
#' 
#' Before starting any treatment, it is necessary to clean the environment "processing". 
#' This new environment will then be saved as an .rds file.
#' This will be the same for "toAnalysis".
#'
#' @param envir The environment to be reset. 
#'
reset_envir <- function(envir = list("processing", "toAnalysis")){
  if(envir == "processing"){
    # Processing of data environment 
    .GlobalEnv$processing <- new.env()
    .GlobalEnv$processing$data.common.offset <- new.env()
    .GlobalEnv$processing$fft.common.offset <- new.env()
  }
  if(envir == "toAnalysis"){
    # Analysis environment 
    .GlobalEnv$toAnalysis <- new.env()
    .GlobalEnv$toAnalysis$plots <- new.env()
    .GlobalEnv$toAnalysis$plots$data.common.offset <- new.env()
    .GlobalEnv$toAnalysis$plots$fft.common.offset <- new.env()
    .GlobalEnv$toAnalysis$current_plots <- new.env()
  }
}

save_envir <- function(dataset_name){
  datapath <- paste0(settings$datapath, dataset_name, "/proc_envir.rda")
  save(data.common.offset, 
       fft.common.offset,  
       envir = .GlobalEnv$processing, 
       file = datapath, 
       compress = FALSE)
}


#' Load dataset in processing enviroment 
#'
#' @param session Current session
#' @param dataset_name Name of the new dataset
#' 
load_data <- function(session, 
                      dataset_name,
                      params){
  datapath <- paste0(settings$datapath, dataset_name, "/raw/")
  reset_envir("processing")
  
  progressSweetAlert(
    session = session, id = "loading",
    title = "Loading datasets",
    display_pct = TRUE, value = 0
  )
  
  k <- params
  for(i in 1:k){
    updateProgressBar(
      session = session,
      id = "loading",
      value = as.integer(i/k*100)
    )
    processing$toProcess[[toString(i)]] <- read.table(paste0(datapath,toString(i)))
  }
  datapath <- paste0(settings$datapath, dataset_name, "/exp_conditions")
  processing$expcond <- read.table(file = datapath)
  datapath <- paste0(settings$datapath, dataset_name, "/data_common_offset")
  processing$offset_treated <- read.csv(file = datapath)
}


load_settings <- function(dataset_name, session){
  reset_envir("toAnalysis")
  datapath <- paste0(settings$datapath, dataset_name, "/data_common_offset")
  toAnalysis$offset_treated <- read.csv(file = datapath)
  
  datapath <- paste0(settings$datapath, dataset_name, "/exp_conditions")
  toAnalysis$exp_cond <- read.table(file = datapath)
  
  datapath <- paste0(settings$datapath, dataset_name, "/obs")
  toAnalysis$obs <- read.csv(file = datapath)
  
  
  datapath1 <- paste0(settings$datapath, dataset_name, "/offsets/")
  datapath2 <- paste0(settings$datapath, dataset_name, "/fft/")
  l_offset <- list.files(datapath1); l_fft <- list.files(datapath2);
  total_load <- length(l_offset) + length(l_fft)
  
  if(!isTRUE(length(l_offset) == 0) | !isTRUE(length(l_fft) == 0)){
    progressSweetAlert(
      session = session, id = "updating",
      title = "Updating data sets",
      display_pct = TRUE, value = 0
    )}
  
  if(isTRUE(length(l_offset) == 0)){
    return(NULL) 
  } else {
    for(i in 1:length(l_offset)){
      tmp_offset <- read.table(paste0(datapath1,l_offset[i]))
      toAnalysis$plots$data.common.offset[[l_offset[i]]] <- plot_refrac(l_offset[i], tmp_offset, toAnalysis$exp_cond[3,1], max_time = 1)
      j <- i
      updateProgressBar(
        session = session,
        id = "updating",
        value = as.integer((j/total_load)*100)
      )
      }
  }
  
  if(isTRUE(length(l_fft) == 0)){
    return(NULL) 
  } else {
    for(i in 1:length(l_fft)){
      tmp_fft <- read.table(paste0(datapath2,l_fft[i]))
      toAnalysis$plots$fft.common.offset[[l_fft[i]]] <-  plot_fft(tmp_fft, toAnalysis$exp_cond[3,1])
      j <- i +length(l_offset)
      updateProgressBar(
        session = session,
        id = "updating",
        value = as.integer((j/total_load)*100)
      )
    }
  }
  closeSweetAlert(session)
}

#' Delete data sets
#'
#' @param input Data sets which will be deleting
#' @param session Current session
#'
delete <- function(input, session){
  
  progressSweetAlert(
    session = session, id = "deleting",
    title = "Deleting datasets",
    display_pct = TRUE, value = 0
  )
  
  k <- length(input)
  for(i in 1:k){
    
    updateProgressBar(
      session = session,
      id = "deleting",
      value = as.integer(i/k*100)
    )
    # On supprime le dossier
    shell(paste0("RD /S /Q %USERPROFILE%\\Documents\\CeremApp\\data\\", toString(input[i])))
    
    ## L'élément à supprimer pour l'utilisateur
    j <- which(.GlobalEnv$settings$list_dataset == input[i])
    .GlobalEnv$settings$list_dataset[j] <- NULL
  }
  save.image()
  
  update_all_UI(session = session, 
                data = .GlobalEnv$settings$list_dataset)
}
