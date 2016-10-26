setwd("D:/Temp")

indivList = list.dirs("./round3manual/", recursive = FALSE, full.names = FALSE)

compareFolder = function(folder1, folder2) {
    wockets.read=function(target, header = TRUE) {
        output=try(read.csv(target, header = header, stringsAsFactors = FALSE, skipNul = TRUE, encoding = "UTF-8"), silent = TRUE)
        if (inherits(output, "try-error")) return(NA) else return(output)
    }
    
    fileList1 = list.files(folder1)
    fileList2 = list.files(folder2)
   
    commonFile = intersect(fileList1, fileList2)
    if (length(commonFile)!=length(fileList1) || length(commonFile)!=length(fileList2)) {
        print("File number doesn't match")
        break
    } else {
        for (i in 1:length(commonFile)) {
            fileX1 = wockets.read(paste0(folder1, "/", commonFile[i]), header = FALSE)
            fileX2 = wockets.read(paste0(folder2, "/", commonFile[i]), header = FALSE)
            if (identical(fileX1, fileX2)) {
                next
            } else {
                print(paste0("content doesn't match: ", commonFile[i]))
                break
            }
        }
        print("match")
    }
}


dates = c("06", "07", "08", "09", "10")

for (j in 1:5) {
    print(dates[j])
    prefix1 = "D:/Temp/round3manual/test11@match.com"
    prefix2 = "D:/Temp/round3auto/test11@match_com"
    
    folder1 = paste0(prefix1, "/logs/2016-10-", dates[j], ".uploaded")
    folder2 = paste0(prefix2, "/logs/2016-10-", dates[j])
    print("log")
    compareFolder(folder1, folder2)
    
    folder1 = paste0(prefix1, "/surveys/2016-10-", dates[j], ".uploaded")
    folder2 = paste0(prefix2, "/surveys/2016-10-", dates[j])
    print("survey")
    compareFolder(folder1, folder2)
}
