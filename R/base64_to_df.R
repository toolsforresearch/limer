#' Convert base64 encoded data to a data frame
#'
#' This function converts raw base64 results into a data frame.
#' @param x \dots
#' @importFrom utils read.csv
#' @export
#' @examples \dontrun{
#' base64_to_df()
#' }

base64_to_df <- function(x) {
  raw_csv <- rawToChar(base64enc::base64decode(x))

  have_copy_survey <- call_limer(method="copy_survey", params = list (iSurveyID_org = -1, sNewname = "This should fail"))
  # returns NULL in LS2
  # returns array(status => "Copy failed", error => "Invalid survey ID") in LS3
  if (is.null(have_copy_survey)) {
    return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE))
  } else {
    return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE, sep = ";"))
  }
}
