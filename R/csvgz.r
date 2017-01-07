#' CSV to GZ Function
#'
#' This function allows you to save a csv file in gz format.
#' @param submission data.frame or data.table data type
#' @param filename file name you want to save it as - string data type
#' @keywords csv
#' @export
#' @examples
#' csv.gz

csv.gz <- function(submission, filename = 'submission'){
  gz <- gzfile(paste0(filename, '.gz'), 'w')
  write.csv(submission, gz, row.names = FALSE)
  close(gz)
}