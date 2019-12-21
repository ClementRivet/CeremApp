#' Update Users Interface in application
#' 
#' After importing a new dataset, it is essential to be able to access it later.
#'
#' @param session The current session
#' @param data This variable allows you to update the list of your data sets that have been imported
#' @examples \dontrun{
#' # From \link{upload_data} the list of your data set have changed
#' list_dataset <- append(list_dataset, new_dataset)
#' update_all_UI(session = session,
#'               data = list_dataset)
#' }
update_all_UI <- function(session, 
                          data)
{
  updateTextInput(session,
                  "data_name",
                  "",
                  "")
  
  updateSelectInput(session,
                    "select",
                    "",
                    choices = data)
  
  updateSelectInput(session,
                    "process",
                    "",
                    choices = data)
  
  updatePickerInput(session,
                    "delete",
                    "",
                    choices = data)
}