setwd("D:/REACH/MATCH/EMA_W3_temp/wockets")

folders <- grep("^.*w3.*$", list.dirs("D:/remoteEMA/data", recursive = FALSE), ignore.case = TRUE, value = TRUE)

# Parallel computing
library(foreach)
library(doParallel)

# Function to split a vector down to job list for each node
triage <- function(jobs, nCL) {    
    splits <- rep(1:nCL, length.out=length(jobs))
    return(split(jobs, splits))
}

nCL <- 4
cl <- makeCluster(nCL)
registerDoParallel(cl)

jobList <- triage(folders, nCL)
foreach (m = 1:nCL) %dopar% {
    job <- jobList[[m]]
    for (i in job) {
        id.start <- regexpr("[0-9]{5}", i)
        idX <- substr(i, id.start, id.start+4)
        commandX <- paste0('java -jar D:/Temp/matchEmaDecryption.jar "', i, '" D:/REACH/MATCH/EMA_W3_temp/wockets/', idX, 'W3')
        system(commandX)
        cat(paste0("node = ", m, "; ", which(job == i), "/", length(job), "; ", idX,"\n"), file = paste0("W3EMA_wockets_copy_checkpoints", Sys.Date(),".txt"), append = TRUE)
    }
}

stopCluster(cl)