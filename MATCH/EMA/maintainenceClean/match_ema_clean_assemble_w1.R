work_dir = "D:/REACH/MATCH/EMA_Clean_RDS/W1"
setwd(work_dir)

target_dir = "D:/REACH/MATCH/EMA_Clean/Wave1"
checklist = data.frame(job = list.dirs(target_dir, full.names = TRUE, recursive = FALSE), survey = NA, stringsAsFactors = FALSE)
checklist$survey = sapply(checklist$job, function(x) {any(grepl("survey", list.dirs(x, full.names = FALSE, recursive = FALSE)))})

# load necessary functions
source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_aggregate.R")

for (i in 1:nrow(checklist)) {
    dirX = tail(strsplit(checklist$job[i], "/")[[1]], 1)
    pathX = paste0(checklist$job[i], "/survey")
    surveydate = grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", list.files(pathX), value = TRUE)
    for (j in 1:length(surveydate)) {
        emaX = ema.aggregate(paste0(pathX, "/", surveydate[j]), clean = TRUE)
        if (!all(is.na(emaX))) {
            if (!dir.exists(dirX)) {dir.create(dirX)}
            emaXname = paste0(dirX, "/", paste(dirX, paste(emaX$id, collapse="_"), surveydate[j], paste0("B", length(emaX$battery)), "clean.rds", sep = "_"))
            saveRDS(emaX, file = paste0("./", emaXname))
        }
    }
    if (i%%10 == 0) {print(paste0(i, "; ", round(i/length(checklist$job)*100, 2), "%"))}
}

# summarize
write.csv(ema.archive(work_dir), paste0("MATCH_EMA_List_Clean_", Sys.Date(), ".csv"), row.names=FALSE, quote=FALSE)
