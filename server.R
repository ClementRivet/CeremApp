server <- function(input, output, session){
  
  # Analysis ----
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  observe({if(input$select == "") return(NULL) else load_settings(input$select, session)})
  
  observeEvent(input$refresh,{
    if(input$select == "") return(NULL) else load_settings(input$select, session)
  })
  
  
  output$selectPlots <- renderUI({
    select <- input$select

    if(select == "") 
      tags$h3("No processing has been carried out")
    else {
      
      if(length(toAnalysis$offset_treated$Common_Offset) == 0){
        tags$h3("No processing has been carried out")
      } else {
        list_offset <- unique(toAnalysis$offset_treated$Common_Offset)
        list_offset <- sort(list_offset)
        list_common_offset <- list("Offset treated" = list_offset)
        
        yes <- as.vector(which(toAnalysis$offset_treated$FFT == "YES"))
        list_fft <- list()
        for(i in 1:length(yes)){list_fft <- append(list_fft,toAnalysis$offset_treated[yes[i],1])}
        list_fft <- unique(list_fft)
        
      tagList(
        pickerInput(
          inputId = "plot_offset_com",
          label = "Common offset",
          choices =  list_common_offset,
          options = list(
            `actions-box` = TRUE),
          multiple = TRUE
        ),
        pickerInput(
          inputId = "plot_fft",
          label = "Fast Fourier Transform",
          choices =  list_fft,
          options = list(
            `actions-box` = TRUE),
          multiple = TRUE
        ),
        sliderInput(
          inputId = "select_time",
          label = "Time axes",
          value = 1,
          min = 0.1,
          max = toAnalysis$exp_cond[3,1]
        )
      )
    }
  }
})
  
  poc <- reactive({input$plot_offset_com})
  pfft <- reactive({input$plot_fft})
  
  output$plots_dataset <- renderUI({
    
    if(input$select == ""){
      return(HTML('<img style="display: block; margin-left: auto; margin-right: auto; vertical-align: middle; justify-content: center;" src="/images/Logo_CEREMA.png" alt="No processing has been carried out" />'))
    }
    # Ne sert à rien de faire comme ça pour la liste car ça ajoute les plots, les uns sur les autres
    poc <- poc(); pfft <- pfft(); 
    tot_plot <- length(poc) + length(pfft)
    if(!isTRUE(length(poc) == 0) | !isTRUE(length(pfft) == 0)){
      if(!isTRUE(length(poc) == 0)){
        for(i in 1:length(poc)){
          toAnalysis$current_plots[[paste0("plot",i)]] <- toAnalysis$plots$data.common.offset[[paste0("offset_",poc[i])]]
        }
      } 
      k <- 4 - length(poc); l <- length(poc) + length(pfft); 
      if(!isTRUE(length(pfft) == 0)){
        for(j in k:l){
          toAnalysis$current_plots[[paste0("plot",j)]] <- toAnalysis$plots$fft.common.offset[[paste0("fft_offset_",pfft[j-k+1])]]
        }
      }
    }
    
    if(isTRUE(length(tot_plot) == 0)){
      return(HTML('<img style="display: block; margin-left: auto; margin-right: auto;" src="/images/Logo_CEREMA.png" alt="No processing has been carried out" />'))
    }
    
    switch(tot_plot,
           "1" = {
             tagList(
               withLoader(
                 renderPlotly({toAnalysis$current_plots$plot1}),
                 type = "html",
                 loader = "dnaspin",
                 proxy.height = NULL
               )
             )
           },
           "2" = {
             tagList(
               withLoader(
                 renderPlotly({toAnalysis$current_plots$plot1}),
                 type = "html",
                 loader = "dnaspin",
                 proxy.height = NULL
               ),
               br(),
               withLoader(
                 renderPlotly({toAnalysis$current_plots$plot2}),
                 type = "html",
                 loader = "dnaspin",
                 proxy.height = NULL
               )
             )
           },
           "3" = {
             tagList(
               fluidRow(
                 column(6,
                        withLoader(
                          renderPlotly({toAnalysis$current_plots$plot1}),
                          type = "html",
                          loader = "dnaspin",
                          proxy.height = NULL
                        )
                 ),
                 
                 column(6,
                        withLoader(
                          renderPlotly({toAnalysis$current_plots$plot2}),
                          type = "html",
                          loader = "dnaspin",
                          proxy.height = NULL
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        offset = 3,
                        withLoader(
                          renderPlotly({toAnalysis$current_plots$plot3}),
                          type = "html",
                          loader = "dnaspin",
                          proxy.height = NULL
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
                          renderPlotly({toAnalysis$current_plots$plot1}),
                          type = "html",
                          loader = "dnaspin",
                          proxy.height = NULL
                        )
                 ),
                 
                 column(6,
                        withLoader(
                          renderPlotly({toAnalysis$current_plots$plot2}),
                          type = "html",
                          loader = "dnaspin",
                          proxy.height = NULL
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(6,
                        withLoader(
                          renderPlotly({toAnalysis$current_plots$plot3}),
                          type = "html",
                          loader = "dnaspin",
                          proxy.height = NULL
                        )
                 ),
                 
                 column(6,
                        withLoader(
                          renderPlotly({toAnalysis$current_plots$plot4}),
                          type = "html",
                          loader = "dnaspin",
                          proxy.height = NULL
                        )
                 )
               )
             )
           }
           
    )
})
  
  observeEvent(input$saveObs,{
    poc <- poc()
    add_observation(list_obs = list(poc[1], poc[2], poc[3], poc[4], input$obs), input$select)
    updateTextAreaInput(session,"obs","","",placeholder = "Save according to current variable settings")
    observations <- read_observation(data_settings = as.list(poc()))
    output$aObs <- renderDT(observations,
                            options = list(paging = FALSE, searching = FALSE, pageLength = 5))
  })

  observeEvent(poc(),{
    observations <- read_observation(data_settings = as.list(poc()))
    output$aObs <- renderDT(observations,
                            options = list(paging = FALSE, searching = FALSE, pageLength = 5))
  })

  

  
  # Upload ----
  data_raw <- reactive({
    file1 <- input$data_exp
    if(is.null(file1)){
      return(NULL)
    } else {return(file1$datapath)}
  })
  
  observeEvent(input$imports,{
    print('hello')
    data_name <- input$data_name
    exp_offset <- input$exp_offset
    time <- input$time
    orig_file <- input$origine
    data_raw  <- data_raw()
    
    # Message d'erreur si data_name or data_raw are incorrect
    if (is.null(data_name) || data_name == " " || is.null(data_raw)) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Impossible to import data",
        type = "error",
        closeOnClickOutside = FALSE) 
    } else {
      # Check that the folder name does not already exist, so as not to overwrite it 
      tmp_datapath <- paste0(settings$datapath, data_name)
      if(dir.exists(tmp_datapath)){
        sendSweetAlert(
          session = session,
          title = "This dataset already exists! \n Please change it.",
          text = NULL,
          type = "warning",
          closeOnClickOutside = FALSE)
        updateTextInput(session, "data_name", "", "")
      } else {
        
        confirmSweetAlert(
          session = session,
          inputId = "upConfirm",
          title = "Confirmation",
          text = tags$div(
            align = "center",
            renderDataTable(sweet_msg_verif(length(data_raw), exp_offset, time, orig_file), 
                            options = list(paging = FALSE, searching = FALSE, pageLength = 4, dom = 't'))
          ),
          btn_labels = c("No","Yes"),
          closeOnClickOutside = FALSE
        )
        
        observeEvent(input$upConfirm,{
          
          if(isTRUE(input$upConfirm)){
            
            progressSweetAlert(
              session = session, id = "importing",
              title = "Importing datasets",
              display_pct = TRUE, value = 0
            )
            
            upload_data(session = session, 
                        dataset_name = data_name, 
                        data = data_raw, 
                        params = c(exp_offset,time,orig_file))
            
            update_all_UI(session = session, 
                          data = .GlobalEnv$settings$list_dataset)
            
            sendSweetAlert(
              session = session,
              title = "Done !",
              text = "All data sets have been processed correctly",
              type = "success",
              closeOnClickOutside = FALSE
            ) 
            
          }
          
        })
      }
      
    }
    
  })
  
  # Processing ----
  process <- reactive({input$process})
  
  observe({
    offsetTreated <- reactiveFileReader(100, session, paste0(settings$datapath,toString(process()), "/data_common_offset"), read.csv)
    output$DT_offset <- renderDT( offsetTreated(), 
                                  options = list(pageLength = 6,
                                                 scrollX = TRUE, scrollY = TRUE,
                                                 fixedColumns = TRUE, fixedRows = TRUE))
  })
  
  output$processing_data <- renderUI({
    process <- process()
    if(process == ""){
      HTML('<center><img src="/images/Logo_CEREMA.png" alt="No processing has been carried out" /></center>')
    } else {
        .GlobalEnv$processing$expcond <- data.frame(read.table(paste0(settings$datapath,toString(process()), "/exp_conditions")))

        no_geo <- processing$expcond[5,1]
        l_offset <- processing$expcond[2,1]
        l_exp <- (no_geo*l_offset)/2
        
        tagList(
          fluidRow(
            column(
               6, 
               label = h5("Experimental conditions"), 
               br(), br(),
               renderDT(processing$expcond,
                        options = list(paging = FALSE, searching = FALSE, pageLength = 6, dom = 't'))
            ),
            
            column(
              6,
              label = h5("Data processed"),
              DTOutput("DT_offset")
            )
          ),
          br(),
          tags$h4("Processing"),
          
          sliderTextInput(
            inputId = "define_offset",
            label = "Common offset", 
            choices = seq(l_offset,l_exp,l_offset),
            selected = c(l_offset,l_exp),
            grid = TRUE
          ),
          
          numericInput("step_offset",
                       label = "Step",
                       value = l_offset,
                       min = l_offset,
                       max = l_offset*3,
                       step = l_offset),
          
          
          fluidRow(
            align ='center',
            column(
              12,
              tags$h4("Options"),
              wellPanel(
                #checkboxGroupButtons(
                #  inputId = "opts",
                #  label = NULL, 
                #  choices = c("Normalize", "FFT"),
                #  status = "default"
                #),
                fluidRow(
                  column(
                    6,
                    awesomeCheckbox(
                      inputId = "fft",
                      label = "FFT", 
                      value = FALSE,
                      status = "danger"
                    )
                  ),
                  column(
                    6,
                    awesomeCheckbox(
                      inputId = "normalise",
                      label = "Normalize", 
                      value = TRUE,
                      status = "danger"
                    )
                  )
                )
              )
            ) 
          ),
          
          actionButton("go_process", "Processing")
        )
    }
  })
  
  
  observeEvent(input$go_process,{
    
    define_offset <- input$define_offset
    step_offset <- input$step_offset
    process <- input$process
    norm <- input$normalise
    s_fft <- input$fft
    no_file <- processing$expcond[1,1]
    offset_exp <- processing$expcond[2,1]
    time_exp <- processing$expcond[3,1]
    orig.file <- processing$expcond[4,1]
    
    
    if((step_offset + define_offset[1]) > define_offset[2]){
      sendSweetAlert(
        session = session,
        title = "Error in interval of common offset",
        text = 
          tags$div(
            tags$p("The length of the interval times the step is superior than the maximum value of the interval."), 
            br(),
            tags$p(paste0("Maximum value x step = ", (step_offset + define_offset[1])))
          ),
        type = "warning",
        closeOnClickOutside = FALSE) 
    } else {
    
      load_data(session = session, 
                dataset_name = process,  
                params = no_file)
      
        progressSweetAlert(
          session = session, id = "processing",
          title = "Processing of data in progress",
          display_pct = TRUE, value = 0
        )
        
        
        m <- if(define_offset[1] == define_offset[2]){
                define_offset[1] 
              } else {
                seq(from = define_offset[1], to = define_offset[2], by = step_offset)
              }
          
        len <- (max(define_offset) - min(define_offset))/step_offset + 1
        i <- 1
        for(k in m){
          
          updateProgressBar(
            session = session,
            id = "processing",
            value = as.integer((i/(len))*100)
          )
          
          common_offset(
            dataset_name = process,
            common_offset = k,
            offset_exp = offset_exp,
            origin.file = orig.file,
            normalized = isTRUE(norm),
            fft = isTRUE(s_fft),
            time_exp = time_exp)
          
          add_offset(
            c(k, if(isTRUE(s_fft)){"YES"} else {"NO"}, if(isTRUE(norm)){"YES"} else {"NO"}),
            process
          )
          i <- i + 1
        }
        
        closeSweetAlert(session)
        
        sendSweetAlert(
          session = session,
          title = "Processing completed",
          text = "Your data has been processed and saved",
          type = "success",
          closeOnClickOutside = FALSE) 
    }
  })


  # Delete ----
  observeEvent( input$deleting_data, {
    del_data <- input$delete
    if(!is.vector(del_data)){
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "No dataset has been selected",
        type = "error",
        closeOnClickOutside = FALSE
      )
    } else  {
      delete(input = del_data, session = session)
    }  
    closeSweetAlert(session)
  })
  
  # output$knitDoc <- renderUI({
  #   input$eval
  #   HTML(knitr::knit2html(text = isolate(input$rmd), fragment.only = TRUE, quiet = TRUE))
  #   #rstudioapi::viewer(knitr::knit2html(text = isolate(input$rmd), fragment.only = T, quiet = T))
  # })
  
  
  # Close ----
  
  
  observeEvent(input$exitModal, {
    showModal(
      modalDialog(
        title = "Exit",
        p("Êtes-vous sur ?"),
        footer = tagList(
          modalButton("No"),
          actionButton("Exit","Yes")
        )
      )
    )
  })
  
  observeEvent(input$Exit, {
    showModal(
      modalDialog(
        title = "Sauvegarde en cours",
        p("Veuillez patienter, je sauvegarde vos travaux !"),
        footer = NULL
      )
    )
    processing$toProcess <- new.env()
    #js$closeWindow()
    save.image()
    stopApp()
  })
  
}