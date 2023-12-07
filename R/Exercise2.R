#' My own version of Pig Latin
#'
#' @description This function takes an input string then converts to my version of Pig Latin. 
#' @param x An input string 
#'
#' @return My version of the Pig Latin based on the input string
#' @export
#'
#' @examples mylatin("nba,nfl") 
#' @examples mylatin("djfkdfjDFjR") 
mylatin <- function(x) {
  if(!(is.character(x))){
    stop("Please input a string")
  }
  if(str_length(x) == 1){
    stop("Length of the string must be greater than or equal to two")
  }
  last <- str_sub(x, -1)
  rest <- str_sub(x, 1, str_length(x)-1)
  return(str_c(last,rest,"ya"))
}