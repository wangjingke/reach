setwd("Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 2")
w2zip = list.files(pattern = ".*\\.zip$")

for (i in 1:length(w2zip)) {
    folderX = gsub(".zip", "", w2zip[i])
    dup = grep(folderX, list.files())
    if (length(dup)==1) {
        unzip(w2zip[i], exdir = paste0("./",folderX))
    }
    print(i)
}

