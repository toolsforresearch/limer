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
  lsversion <- call_limer(method="get_site_settings",params=list(sSetttingName="versionnumber"))
  lsmajor <- as.numeric(substr(lsversion, 1, 1))
  if (lsmajor < 3) {
    return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE))
  } else {
    return(read.csv(textConnection(raw_csv), stringsAsFactors = FALSE, sep = ";"))
  }
}
