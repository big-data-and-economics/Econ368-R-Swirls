library(base64enc)
library(tidyverse)
library(googlesheets4)
library(swirlify)

gs4_auth(email = "kcoombs@bates.edu")

# Load the data
swirls <- read_sheet('https://docs.google.com/spreadsheets/d/1ID-A78lyPDIapVgvod0h2jdFObHpFI0zVswJOeIy_po/edit?gid=529476745#gid=529476745',
    col_names=c('time','email','hash'),skip=1)

decode_hash <- function(hash=NA) {
    return(
        read_csv(
            rawToChar(
                base64decode(hash)
            )
        )
    )
}

head(swirls) %>% map_dfr(swirls, function(dat) {
    performance <- decode_hash(dat$hash)
    taken <- distinct(dat,time,email)
    return(
        mutate(performance, time=taken$time, email=taken$email))
}) 
