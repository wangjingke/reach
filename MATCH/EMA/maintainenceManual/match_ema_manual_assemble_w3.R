work_dir <- "D:/REACH/MATCH/EMA_ManualRetrieve/W3"
setwd(work_dir)

manual <- list.dirs("D:/REACH/MATCH/EMA_W3_temp/manual", recursive = FALSE)

source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_aggregate.R")

for (k in manual) {
    survey.list <- grep("survey$|surveys$", list.dirs(k, recursive = TRUE, full.names = TRUE), value = TRUE)
    if (length(survey.list>=1)) {
        for (i in survey.list) {
            surveydate <- grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}.*$", list.files(i), value = TRUE)
            for (j in surveydate) {
                dirX <- grep("[0-9]{5}", strsplit(i, "/")[[1]], value = TRUE)[1]
                # chose to use old or new format based on survey names
                if (!any(grepl("Survey_KEY_EMA_", list.files(paste0(i, "/", j))))) {
                    emaX <- ema.aggregate(paste0(i, "/", j))
                } else {
                    emaX <- ema.aggregate.key(paste0(i, "/", j))
                }
                if(!all(is.na(emaX))) {
                    if (!file.exists(dirX)) {dir.create(dirX)}
                    emaXname <- paste0(dirX, "/", paste(dirX, paste(emaX$id, collapse = "_"), substring(j, 1, 10), paste0("B", length(emaX$battery)), "manual.rds", sep = "_"))
                    saveRDS(emaX, file = paste0("./", emaXname))
                }
            }
        }
    }
    print(paste0(grep(k, manual), "/", length(manual)))
}

write.csv(ema.archive(work_dir), paste0("MATCH_EMA_W3_List_manual_", Sys.Date(), ".csv"), row.names=FALSE, quote=FALSE)
