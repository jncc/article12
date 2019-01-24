#' Create reporting tool environment
#'
#' This function creates the reporting tool environment used to
#' store the id values required as primary and foreign keys in
#' the reporting tool database
#' 
#' @return
#' @export
#'
#' @examples
create_reporting_tool_environment <- function() {
  
  if (!exists("reporting_tool", mode = "environment")) {
    assign("reporting_tool", new.env(parent = emptyenv()), envir = globalenv())
    
    # assign values in environment
    assign("latest_bird_id", 0L, envir = reporting_tool, inherits = FALSE)
    assign("latest_annex_ii_id", 0L, envir = reporting_tool, inherits = FALSE)
    assign("latest_change_id", 0L, envir = reporting_tool, inherits = FALSE)
    assign("latest_measure_info_id", 0L, envir = reporting_tool, inherits = FALSE)
    assign("latest_measure_id", 0L, envir = reporting_tool, inherits = FALSE)
    assign("latest_pressure_id", 0L, envir = reporting_tool, inherits = FALSE)
    assign("latest_pressure_info_id", 0L, envir = reporting_tool, inherits = FALSE)
    assign("latest_notes_id", 0L, envir = reporting_tool, inherits = FALSE)
  }
}
