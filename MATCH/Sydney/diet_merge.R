setwd("Y:/Jing/Sydney")

# EMA
load("D:/REACH/MATCH/Project MATCH/MATCH_EMA_V14.RData")
w1=emaW1
w1$TIME=NULL
w1$dateForMerging = as.character(w1$DATE)

# take out all the mother variables
w1=w1[w1$CHILD==1, !grepl("MOTHER|ACTIVITY|VIG|VALID|NONVALID|MOD|SED|MET|LIGHT|MVPA|^.*_[0-2][M|P][0-2]$|^.*_0C$", names(w1))]
w1$response_time=strptime(w1$START_TIME, format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
# dietary recall
diet=read.table("Y:/Jing/Sydney/MATCH_W1_24hr_Validation_3.3.2016.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
diet$time=strptime(paste0(diet$dintake, " ", diet$tmtime), format="%d%b%Y %H:%M:%S", tz="America/Los_Angeles")
diet$dateForMerging = as.character(as.Date(diet$dintake, format="%d%b%Y"))

# dietary variables to be merged
diet.var=names(diet)[12:15]
# time windows to be scanned
win=c("2h", "208h", "217h", "25h", "3h", "4h", "6h", "1d")
distance=c(0, 2.5/60, 5/60, 0.25, 0.5, 1, 2)
# new variables to be added to EMA
ema.diet=c(t(outer(diet.var, paste0("_", win), paste0)))
ema.diet.count=c(t(outer(diet.var, paste0("_", win, "_count"), paste0)))
ema.diet.totalmeal=c(t(outer(diet.var, paste0("_", win, "_totalmeal"), paste0)))

common=data.frame(uid=intersect(paste0(unique(w1$ID), "W1"), unique(diet$cpartid)), stringsAsFactors = FALSE)
common$ID=substr(common$uid, 1, 5)

unique(diet$cpartid)[!unique(diet$cpartid) %in% common$uid]

# calculating time windows by hrs (hrs to sec)
hrs = function(u) {return(u*3600)}
# function to determine dietary intake
eaten = function(x) {return(ifelse(sum(x, na.rm = TRUE)>=1, 1, 0))}

output=c()
for (i in 1:nrow(common)) {
    dietX=diet[diet$cpartid==common$uid[i],]
    emaX=w1[w1$ID==common$ID[i] & w1$dateForMerging %in% unique(dietX$dateForMerging) & w1$COMPLY==1,]
    if (nrow(dietX)>0 & nrow(emaX)>0) {
        outputX=cbind(emaX, matrix(NA, nrow=nrow(emaX), ncol=length(ema.diet)*3, dimnames=list(c(), c(ema.diet, paste0(ema.diet, "_count"), paste0(ema.diet, "_totalmeal")))))

        for (j in 1:nrow(outputX)) {
            for (k in 1:length(distance)) {
                if (outputX[j, "WEEKEND"]==0 & outputX[j, "WINDOW"]==10) {
                    win.upper=strptime(strftime(outputX$response_time[j], format = "%Y-%m-%d", tz="America/Los_Angeles"), format = "%Y-%m-%d", tz="America/Los_Angeles")
                } else {
                    win.upper=outputX$response_time[j]-hrs(2)-hrs(distance[k])
                }
                win.lower=outputX$response_time[j]+hrs(distance[k])
                outputX[j, paste0(diet.var, "_", win[k])]=sapply(dietX[dietX$time>win.upper & dietX$time<win.lower, c(diet.var)], eaten)
                outputX[j, paste0(diet.var, "_", win[k], "_count")]=sapply(dietX[dietX$time>win.upper & dietX$time<win.lower, c(diet.var)], sum, na.rm = TRUE)
                outputX[j, paste0(diet.var, "_", win[k], "_totalmeal")]=sapply(dietX[dietX$time>win.upper & dietX$time<win.lower, c(diet.var)], length)
            }
            outputX[j, paste0(diet.var, "_1d")]=sapply(dietX[min(outputX$response_time[outputX$dateForMerging==outputX[j,"dateForMerging"]])-hrs(2) <= dietX$time & dietX$time <= max(outputX$response_time[outputX$dateForMerging==outputX[j,"dateForMerging"]]), c(diet.var)], eaten)
            outputX[j, paste0(diet.var, "_1d_count")]=sapply(dietX[min(outputX$response_time[outputX$dateForMerging==outputX[j,"dateForMerging"]])-hrs(2) <= dietX$time & dietX$time <= max(outputX$response_time[outputX$dateForMerging==outputX[j,"dateForMerging"]]), c(diet.var)], sum, na.rm = TRUE)
            outputX[j, paste0(diet.var, "_1d_totalmeal")]=sapply(dietX[min(outputX$response_time[outputX$dateForMerging==outputX[j,"dateForMerging"]])-hrs(2) <= dietX$time & dietX$time <= max(outputX$response_time[outputX$dateForMerging==outputX[j,"dateForMerging"]]), c(diet.var)], length)
        }
        output=rbind(output, outputX)
        rm(outputX)
    }
    print (paste0(i, "; ", round(i/nrow(common)*100, 2), "%"))
}


# swtich position of col 1&2 to avoid being SYLK in excel
output=output[, c(2, 1, 3:ncol(output))]
write.csv(output, paste0("MATCH_EMA_Diet_", gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE, quote = FALSE)


