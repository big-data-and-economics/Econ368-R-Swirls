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
  log[['encoded_skip']] <- base64encode(log$skipped)
  save(log, file = "log.Rdata")
}


submit_log <- function(){
  
  # Please edit the link below
  pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSdkRFTcPE9P0CgbZ4jvI8JJMZwDhC0Lmw99vbPlge8FjFV3WQ/viewform?usp=pp_url&entry.792538263="
  
  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }
  
  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}

  log_ <- getLog()
  nrow_ <- max(unlist(lapply(log_, length)))
  log_tbl <- data.frame(user = rep(log_$user, nrow_),
                        course_name = rep(log_$course_name, nrow_),
                        lesson_name = rep(log_$lesson_name, nrow_),
                        question_number = p(log_$question_number, nrow_, NA),
                        correct = p(log_$correct, nrow_, NA),
                        attempt = p(log_$attempt, nrow_, NA),
                        skipped = p(log_$skipped, nrow_, NA),
                        datetime = p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  # Set column of data.frame equal to vector, so each row is an element of the vector


  log_tbl[['encoded_skip']] <- sapply(log_tbl$skipped,base64encode)
  filename = paste("swirl_log", log_tbl$user[1], log_tbl$course_name[1], log_tbl$lesson_name[1], ".csv", sep = "_")
  write.csv(log_tbl, file = filename, row.names = FALSE)
  encoded_log <- base64encode(filename)
  browseURL(paste0(pre_fill_link, encoded_log))
}