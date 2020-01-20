server <- function(input, output, session){
  
  # Analysis ----
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}