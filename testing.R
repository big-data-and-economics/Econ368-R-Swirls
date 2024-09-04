load('log.Rdata')

# Retrieve the log from swirl's state

share_completed <- function(log){
    user <- log$user
    share_completed <- mean(as.logical(base64decode(log$encoded_skip)), na.rm = TRUE)
    return(c(user, share_completed))
}

graded <- share_completed(log)

print(paste('User', graded[1], 'completed', graded[2], 'of the questions.'))