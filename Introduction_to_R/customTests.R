check_equality_of_vector = function(correctVal) {
  e = get('e', parent.frame())
  if (length(correctVal) != length(e$val)) {
    return(FALSE)
  }
  return(all(correctVal == e$val))
}

check_equality_of_data = function(correctVal) {
  e = get('e', parent.frame())
  return(identical(correctVal, e$val))
}

getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Retrieve the log from swirl's state
getLog <- function(){
  return(getState()$log)
}

# Save the log to an .Rdata file
saveLog <- function(){
  log <- getLog()
  save(log, file = "log.Rdata")
}
