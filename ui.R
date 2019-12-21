options(shiny.maxRequestSize=100*1024^2)
source(paste0(getwd(),"/init.R"))
#load(file = "~/CeremApp/app/.RData", envir = .GlobalEnv)
source(paste0(getwd(),"/app/global.R"))


check.packages(c("shiny",
                 "shinythemes",
                 "shinyWidgets",
                 "shinycustomloader",
                 "shinyjs",
                 "rmarkdown",
                 "data.table",
                 "stringr",
                 "parallel",
                 "stats",
                 "plotly", 
                 "DT"))

# UI ----

ui <- fluidPage(
  responsive = FALSE,
  # HEAD ----
  tags$head(
    tags$title("CEREMA APPLICATION"),
    tags$link(rel = "icon", type = "image/x-icon", href = "http://localhost:1984/www/icon_cerema.ico")
  ),
  
  # Theme ----  
  theme = shinytheme("united"),
  
  # JS function : Close ----
  useShinyjs(),
  extendShinyjs(text = "shinyjs.closeWindow = function() {window.close();}",
                functions = c("closeWindow")),
  
  # Start Body ----
  navbarPage(
    
    # Project's name ----
    "CEREMA Ouest",
    
    # TAB : Logboard ----
    tabPanel(
      "Logbook",
      mainPanel(
        
        # On prend la totalitÃ© de la largeur de l'Ã©cran pour un  meilleur confort
        width = 12,
        
        dropdown(
          
          tags$h3("Update your logbook :"),
          
          # Les donnÃ©es sont en constantes Ã©volution, il est donc nÃ©cessaire de pouvoir en importer
          textInput("nom_date","", placeholder = "Example : July 1, 2019"),
          fileInput("jdb","", multiple = FALSE),
          textOutput("import_new_jb"),
          actionButton("go_actua", "Actualisation")
        ),
        
        includeHTML("www/presentation.html")
        
      )    
      
    ),
    
    # TAB : Data analysis ----
    tabPanel(
      "Data analysis",
      fluidRow(
        sidebarPanel(
          width = 3,
          
          fluidRow(
            
            align = "center",
            
            # DIV : Observations ----      
            
            column(6,
                   align = "center",
                   
                   dropdown(
                     
                     tags$h4("Add an observation :"),
                     
                     textAreaInput("obs",  
                                   "",
                                   "",
                                   width = "260px",
                                   height = "100px",
                                   placeholder = "Save according to current variable settings",
                                   resize = "vertical"),
                     
                     actionButton("saveObs", "Sauve"),
                     
                     style = "unite", icon = icon("fas fa-edit"), 
                     status = "default", width = "300px",
                     animate = animateOptions(
                       enter = animations$fading_entrances$fadeInLeftBig,
                       exit = animations$fading_exits$fadeOutRightBig
                     )
                   )
            ),
            
            column(
              6,
              actionBttn(
                inputId = "refresh",
                label = NULL,
                style = "unite", 
                color = "default",
                icon = icon("fas fa-refresh")
              )
            )
          ),
          
          br(),
          
          # DIV : Select your data ----
          
          selectInput("select",
                      "Choose your data",
                      choices = settings$list_dataset),
          
          uiOutput("selectPlots"),
          
          DTOutput("aObs")
          
        ),
        
        mainPanel(
          width = 9,
          align = "center",
          column(12,
                 wellPanel(
                   uiOutput("plots_dataset")
                 )
          )
        )
      )       
    ),
    
    # MENU : Settings ----
    navbarMenu(
      "Settings",
      
      # TAB : Data sets ----
      tabPanel(
        "Dataset",
        fluidRow(
          column(4,
                 wellPanel(
                   fluidRow(
                     column(
                       7,
                       tags$h3("Importing from data raw")
                     ),
                     column(
                       5,
                       align = "left",
                       dropdown(
                         tags$h4("Importing from data raw"),
                         tags$p("1. Spatial inrementation : Distance between two sensors, 
                                   here between two geophones."),
                         tags$p("2. Acquisition time : Duration in seconds of a seismic shot."),
                         tags$p("3. The reference point: The file corresponding to the seismic 
                                   shot that is located -inc_spatial/2 of the first sensor"),
                         
                         circle = TRUE, status = "info",
                         icon = icon("info"), width = "300px",
                         
                         tooltip = tooltipOptions(title = "About the imporation")
                       )
                     )
                   ),
                   
                   textInput("data_name",
                             "", 
                             placeholder = "Example : 20190625_Sarce"),
                   
                   fileInput("data_exp",
                             "", 
                             multiple = TRUE),
                   
                   numericInput("exp_offset",
                                label = "Spatial incrementing (in meter):",
                                value = 1,
                                min = 0,
                                step = 0.5),
                   fluidRow(
                     column(6,
                            numericInput("time",
                                         label = "Acquisition time (in second): ",
                                         value = 1,
                                         min = 0,
                                         step = 0.5)
                     ),
                     column(6,
                            numericInput("origine",
                                         label = "The reference point: ",
                                         value = 1,
                                         min = 1,
                                         step = 1)
                     )
                   ),
                   
                   actionButton("import", "Importing")
                   
                 ),
                 
                 wellPanel(
                   tags$h3("Delete a dataset"),
                   pickerInput(
                     inputId = "delete",
                     label = "",
                     choices = settings$list_dataset,
                     options = list(
                       `actions-box` = TRUE),
                     multiple = TRUE
                   ),
                   
                   actionButton("deleting_data","Delete")
                 )
          ),
          column(8,
                 wellPanel(
                   fluidRow(
                     column(
                       2,
                       tags$h3("Treatment")
                     ),
                     column(
                       10,
                       align = "left",
                       dropdown( 
                         tags$h4("Importing from data raw"),
                         tags$p("1. Select a data set. (A table of the parameters of your data as well as a summary of your treatments will appear)"),
                         tags$p("2. Select the processing parameters."),
                         tags$p("   2.1 Common offset : Distance in source and sensors"),
                         tags$p("   2.2 Step : For the loop, allows to process several common offset at the same time"),
                         tags$p("   2.3 If you wish to process only one common offset, converge the ends of the interval at one point."),
                         tags$p("3. Select the options if you wish. (By default, signals will be normalized by the maximum value of the seismic shot.)"),
                         tags$p("4. Warning! Warning! With each processing performed, data with the same common offset will be overwritten by the new ones."),
                         circle = TRUE, status = "info",
                         icon = icon("info"), width = "300px",
                         
                         tooltip = tooltipOptions(title = "About the processing")
                       )
                     )
                   ),
                  
                 
                 selectInput("process",
                             "What data should be processed?",
                             choices = settings$list_dataset),
                 uiOutput("processing_data")
                 
                 
            )
          )
        )
        
      ),
      
      
      # TAB : PDF ----
      tabPanel(
        "PDF",
        tags$h1("Under development"),
        tags$h2("For information, the source codes will not be accessible to users.")
      )
    ),
    
    # MENU : Session ----
    navbarMenu(
      "Session",
      
      # TAB : Error message ----
      tabPanel(
        "Error e-mail",
        tags$h1("Under development")
      ),
      
      # TAB : Close ----
      tabPanel(
        actionBttn(
          inputId = "Exit",
          label = "Exit", 
          style = "stretch",
          color = "warning",
          size = "xs", 
          block = TRUE
      ))
    )
  )
)
