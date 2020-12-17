invert <- function(list, simplify = T){
  #' Inverts named vectors, list
  #' list: list or vector
  #' simplyfy: (bool) if FALSE, add quote
  #' :return: vector
  
  new_list <- names(list)
  quote <- paste0('"', as.character(list), '"')
  unquote <- as.character(list)
  names(new_list) <- if(simplify){unquote}else{quote}
  return(new_list)
}


is.valid <- function(x) {
  #' !is.null, !is.na, !is.nan
  #' :return: bool

  require(shiny)
  is.null(need(x, message = FALSE))  
}


listToReactive <- function(list){
  #' Create reactiveValues from list
  #' list: named list of vectors
  #' :return: shiny reactive
  
  require(shiny)
  do.call("reactiveValues", list)
}