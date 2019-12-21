uiPlots <- function(id, list_common_offset){
  ns <- NS(id)
  
  pickerInput(
    inputId = ns("plot_offset_com"),
    label = "Common offset",
    choices =  list_common_offset,
    options = list(
      `actions-box` = TRUE),
    multiple = TRUE
  )
}


#' Plots dynamic grahics to analysis
#'
#' It will be possible for the user to select several graphs at a time in the same window. So depending on the number of selections, 
#'here 4 maximum, the graph will adapt to the current page.
#'
#' @param input Select data
#' @param output Area where plots are appeared
#' @param plots List of plots 
#'  
plots_analysis <- function(input, output){
  output <- renderUI({
    ns <- NS("uiPlots")
    if(is.null(input)){
      tagList(
        tags$h1("No data was selected")
      )
    }
    print(paste("func : ",input))
    no_plots <- length(input)
    toPlot <- function(x){return(toAnalysis$plots$data.common.offset[[paste0("plot_offset_",toString(x))]])}
    switch(input,
           "1" = {
             tagList(
               withLoader(
                 plotlyOutput(ns(toPlot(input[1]))),
                 type = "html",
                 loader = "dnaspin"
                 
               )
             )
           },
           "2" = {
             tagList(
               withLoader(
                 plotlyOutput(toPlot(input[1])),
                 type = "html",
                 loader = "dnaspin"
               ),
               withLoader(
                 plotlyOutput(toPlot(input[2])),
                 type = "html",
                 loader = "dnaspin"
               )
            )
           },
           "3" = {
             tagList(
               fluidRow(
                 column(6,
                        withLoader(
                          plotlyOutput(toPlot(input[1])),
                          type = "html",
                          loader = "dnaspin"
                        )
                 ),
                 column(6,
                        withLoader(
                          plotlyOutput(toPlot(input[3])),
                          type = "html",
                          loader = "dnaspin"
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        withLoader(
                          plotlyOutput(toPlot(input[4])),
                          type = "html",
                          loader = "dnaspin"
                        )
                 )
               )
             )
           },
           "4" = {
             tagList(
               fluidRow(
                 column(6,
                        withLoader(
                          plotlyOutput(toPlot(input[1])),
                          type = "html",
                          loader = "dnaspin"
                        )
                 ),
                 column(6,
                        withLoader(
                          plotlyOutput(toPlot(input[2])),
                          type = "html",
                          loader = "dnaspin"
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        withLoader(
                          plotlyOutput(toPlot(input[3])),
                          type = "html",
                          loader = "dnaspin"
                        )
                 ),
                 column(6,
                        withLoader(
                          plotlyOutput(toPlot(input[4])),
                          type = "html",
                          loader = "dnaspin"
                        )
                 )
               )
             )
           }
    )
  })
}

toPlot <- function(x){return(toAnalysis$plots$data.common.offset[[paste0("plot_offset_",toString(x))]])}