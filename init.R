# Initilization ----
options(shiny.maxRequestSize=100*1024^2)
# .RData exist and empty
if(file.exists(paste0(getwd(),"/.RData"))){
  load(file = paste0(getwd(),"/.RData"), envir = .GlobalEnv)
  if(is.null(length(.GlobalEnv))){
    # Analysis environment ----
    .GlobalEnv$toAnalysis <- new.env()
    .GlobalEnv$toAnalysis$plots <- new.env()
    .GlobalEnv$toAnalysis$current_plots <- new.env()
    
    # Processing of data environment 
    .GlobalEnv$processing <- new.env()
    .GlobalEnv$processing$expcond <- new.env()
    .GlobalEnv$processing$data.common.offset <- new.env()
    .GlobalEnv$processing$fft.common.offset <- new.env()
    
    # Pattern for application
    .GlobalEnv$settings <- new.env()
    .GlobalEnv$settings$workPath <- getwd()
    .GlobalEnv$settings$datapath <- paste0(getwd(),"/data/")
    .GlobalEnv$settings$list_dataset <- list()
    #.GlobalEnv$settings$module <- "~/CeremApp/app/modules/"
    .GlobalEnv$settings$sourcepath <- paste0(getwd(),"/app/functions/") # Only there are new functions, otherwise it's in CeremAppTools package
    .GlobalEnv$settings$dir <- system.file()
    #settings$cores <- if(installed.packages("parallel")) detectCores() ## Not used at this moment. Some troubles with parallel tasks
    save.image()
  }
}

# .RData doesn't exist 
if(!file.exists(paste0(getwd(),"/.RData"))){
  # Analysis environment ----
  .GlobalEnv$toAnalysis <- new.env()
  .GlobalEnv$toAnalysis$plots <- new.env()
  .GlobalEnv$toAnalysis$current_plots <- new.env()
  
  # Processing of data environment 
  .GlobalEnv$processing <- new.env()
  .GlobalEnv$processing$expcond <- new.env()
  .GlobalEnv$processing$data.common.offset <- new.env()
  .GlobalEnv$processing$fft.common.offset <- new.env()
  
  # Pattern for application
  .GlobalEnv$settings <- new.env()
  .GlobalEnv$settings$workPath <- getwd()
  .GlobalEnv$settings$datapath <- paste0(getwd(),"/data/")
  .GlobalEnv$settings$list_dataset <- list()
  #.GlobalEnv$settings$module <- "~/CeremApp/app/modules/"
  .GlobalEnv$settings$sourcepath <- paste0(getwd(),"/app/functions/") # Only there are new functions, otherwise it's in CeremAppTools package
  .GlobalEnv$settings$dir <- system.file()
  #settings$cores <- if(installed.packages("parallel")) detectCores() ## Not used at this moment. Some troubles with parallel tasks
  save.image()
} 