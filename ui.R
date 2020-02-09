source(paste0(getwd(),"/init.R"))
source(paste0(getwd(),"/app/global.R"))

# init <- "### Sample knitr Doc
# This is some markdown text. It may also have embedded R code
# which will be executed.
# ```{r}
# 2*3
# rnorm(5)
# ```
# It can even include graphical elements.
# ```{r}
# hist(rnorm(100))
# ```
# "

ui <- dashboardPagePlus(
  skin = "yellow",
  enable_preloader = TRUE,
  #md = TRUE,
  collapse_sidebar = TRUE,
  header = dashboardHeaderPlus(
    title = tagList(
      tags$span(
        class = "logo-mini", 
        HTML('<style type="text/css">
                .roundedImage{
                  overflow:hidden;
                  -webkit-border-radius:50px;
                  -moz-border-radius:50px;
                  border-radius:50px;
                  width:100%;
                  height:100%;
                }
              </style>
              <div class="roundedImage">
                 <center> 
                   <img src="images/icon_cerema.png" alt="1" width="100%"/>
                 </center>
              </div>')
      ),
      tags$span(
        class = "logo-lg", "Cerema Ouest"
      )
    ),
    enable_rightsidebar = TRUE,
    tags$li(
      actionLink("exitModal", label = "", icon = icon("fas fa-sign-out-alt")),
      class = "dropdown"
    )

  ),
  sidebar = dashboardSidebar(
    tags$script("$(document).on('click', '.sidebar-toggle', function () {
                         Shiny.onInputChange('SideBar_col_react', Math.random())});"),
    tags$script("$(document).on('click', '.treeview.active', function () {
                         $(this).removeClass('active');
                         $(this).find( 'ul' ).removeClass('menu-open'); 
                         $(this).find( 'ul' ).css('display', 'none'); 
                });"),
    sidebarMenu(
      br(),
      menuItem("Data analysis", tabName = "da", icon = icon("fas fa-chart-line")),
      menuItem("Data sets", tabName = "dsets", icon = icon("fas fa-database"),
               menuSubItem("Import & Delete", "Import_Delete"),
               menuSubItem("Manage", "man")),
      menuItem("Logbook", tabName = "lg", icon = icon("fas fa-book")),
      menuItem("Help", tabName = "help", icon = icon("fas fa-question-circle"))
      
    )
    
  ),
  body =  dashboardBody(
    setShadow(class = "dropdown-menu"),
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    withMathJax(),
    
    tags$head(
      tags$link(
        rel = "icon", 
        type = "image/x-icon", 
        href = "images/icon_cerema.ico"
      ),
      tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 80;

        $("#map_container").height(boxHeight);
        $("#map").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')
    ),
    
    tabItems(
      tabItem(
        "lg",
        width = 12,
        fluidPage(
          dropdown(
            
            tags$h3("Update your logbook :"),
            
            # Les données sont en constantes évolution, il est donc nécessaire de pouvoir en importer
            textInput("nom_date","", placeholder = "Example : July 1, 2019"),
            fileInput("jdb","", multiple = FALSE),
            textOutput("import_new_jb"),
            actionButton("go_actua", "Actualisation")
          ),
          #includeMarkdown("www/markdown/presentation.md")
          includeHTML("www/html/presentation.html")
          
          # fluidPage(
          #   h1("Shiny Ace knitr Example"),
          #   fluidRow(
          #     column(
          #       6,
          #       h2("Source R-Markdown"),
          #       aceEditor("rmd", mode = "markdown", value = init),
          #       actionButton("eval", "Update")
          #     ),
          #     column(
          #       6,
          #       h2("Knitted Output"),
          #       box(
          #         width = 12,
          #         htmlOutput("knitDoc")
          #       )
          #     )
          #   )
          # )
        )
        
      ),
      
      tabItem(
        "Import_Delete",
        box(
          fluidRow(
            "Importing from data raw",
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
          
          actionButton("imports", "Importing")
          
        ),
        
        box(
          title = "Delete a dataset",
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
      
      tabItem(
        "man",
        box(
          title = "Treatment",
          #   dropdown( 
          #     includeHTML('www/html/importation.html'),
          #     circle = TRUE, status = NULL,
          #     icon = icon("info"), width = "450px",
          #     
          #     tooltip = tooltipOptions(title = "About the processing")
          #   )
          # ),
          width = 12,
          
        br(),  
         selectInput("process",
                      "What data should be processed?",
                      choices = settings$list_dataset),
         uiOutput("processing_data")
      
        )
      ),
      
      tabItem(
        "da",
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
            box(
              id = "map_container",
              width = 12,
              uiOutput("plots_dataset")
            )
            
          )
        )
      ),
      
      tabItem(
        "help",
        fluidRow(
          width = 12,
          box(
            title = "Fonctionnement général de l'application",
            collapsible = T,
            solidHeader = T,
            collapsed = T,
            status = "warning",
            width = 12,
            includeMarkdown("www/markdown/help.md"),
            img(style="display: block; margin-left: auto; margin-right: auto;", src="../images/help menu.png")
          ),
          box(
            title = "Analyser vos données",
            solidHeader = T,
            collapsible = T,
            collapsed = T,
            status = "warning",
            width = 12,
            includeMarkdown("www/markdown/help2.md")
          )
        )
      )
    )
  ),
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      id = 1,
      icon = NULL,
      active = TRUE,
      title = "Report"
    )
  ),
  title = "CeremApp"
  
)