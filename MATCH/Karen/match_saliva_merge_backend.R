# test merging saliva log and lab results for MATCH W1

# function to uniform time in log and saliva
timeSplit=function(x) {
    segments=strsplit(x, " ")[[1]]
    
    if (length(segments)>1) {
        timepoint=segments[2]
    } else {
        timepoint=segments[1]
    }
    
    if (grepl("AM|PM|am|pm", timepoint)) {
        if (grepl("AM|am", timepoint)) {
            hr=strsplit(timepoint, "AM|am|:")[[1]][1]
            if (nchar(hr)==1) {hr=paste("0", hr, sep="")}
            min=strsplit(timepoint, "AM|am|:")[[1]][2]
            return(paste(hr, min, "00", sep=":"))
        } else {
            hr=as.numeric(strsplit(timepoint, "PM|pm|:")[[1]][1])
            hr=ifelse(hr==12, hr, hr+12)
            min=strsplit(timepoint, "PM|pm|:")[[1]][2]
            return(paste(hr, min, "00", sep=":"))
        }
    } else {
        return(timepoint)
    }
}

# function to uniform date in saliva lab results
dateSplit=function(x) {
    segments=strsplit(x, " ")[[1]][1]
    
    if (grepl("\\.", segments)) {
        year=strsplit(segments, "\\.")[[1]][3]
        mon=strsplit(segments, "\\.")[[1]][1]
        day=strsplit(segments, "\\.")[[1]][2]
        return(paste(year, mon, day, sep="-"))
    } else {
        return(segments)
    }
}

library(XLConnect)

log=readWorksheetFromFile(log_file, sheet="Physical Log", header=TRUE, startCol=1, endCol=17, colTypes=rep("character", 17))
log$ordnung=1:nrow(log)
log$WriDate=sapply(strsplit(log$WrittenDate, " "), "[[", 1)
log$WriTime=sapply(log$WrittenTime, timeSplit)
log$timestamp=paste(log$WriDate, log$WriTime, sep=" ")
log$uid=sapply(log$ID, substr, start=1, stop=5)
# drop observations with ID=NA
log=log[!is.na(log$ID),]
colnames(log)[names(log)=="SaliviaWindow..1.WU..2.WU.30..3.Afternoon..4.Bedtime."]="SalivaWindow"
colnames(log)[names(log)=="Eat.drink.brush.smoke.exercise"]="eaten"

saliva=readWorksheetFromFile(result_file, sheet="Wave 1", header=TRUE, startCol=1, endCol=12, colTypes=rep("character", 12))
saliva$WriDate=sapply(saliva$Date, dateSplit)
saliva$WriTime=sapply(saliva$Time, timeSplit)
saliva$timestamp=paste(saliva$WriDate, saliva$WriTime, sep=" ")
saliva$uid=sapply(saliva$ID, substr, start=1, stop=5)
# drop observations with ID=NA
saliva=saliva[!is.na(saliva$ID),]
# drop duplicated labels with missing saliva measurements
saliva$dup=paste0(saliva$ID, "_", saliva$Tube.Label)
dup=unique(saliva[duplicated(saliva$dup), "dup"])
saliva=saliva[!((saliva$dup %in% dup) & is.na(saliva$Cortisol.nmol.l)),] # drop if duplicated and missing
saliva=saliva[!((saliva$dup %in% dup) & saliva$Cortisol.nmol.l=="0"),] # drop if duplicated and equal to 0

# extract log and result IDs
indv_saliva=unique(saliva$uid)
indv_log=unique(log$uid)

# common list
common=intersect(indv_saliva, indv_log)

# people in saliva but not in log
indv_saliva[!indv_saliva %in% indv_log]
# people in log but not in saliva
indv_log[!indv_log %in% indv_saliva]

mark_log=c("Label", "Number", "timestamp")
mark_saliva=c("Tube.Label", "Tube..", "timestamp")

result=c()
prob=c()

# merge the ones that can be merged perfectly
for (i in 1:length(common)) {
    logX=log[log$uid==common[i], c("ordnung", "uid", "Label", "Number", "timestamp", "SalivaWindow", "eaten")]
    salivaX=saliva[saliva$uid==common[i], c("uid", "Tube.Label", "Tube..", "Cortisol.nmol.l", "run.Cortisol.in.duplicate", "timestamp")]
  
    # if label is a perfect match
    label=all(logX$Label %in% salivaX$Tube.Label, salivaX$Tube.Label %in% logX$Label, !is.na(logX$Label), !is.na(salivaX$Tube.Label))
    # if Tube num is a perfect match
    tube=all(!is.na(logX$Number), !is.na(salivaX$Tube..), logX$Number %in% salivaX$Tube.., salivaX$Tube.. %in% logX$Number)
    # if timestamp is a perfect match
    timestamp=all(!is.na(logX$timestamp), !is.na(salivaX$timestamp), logX$timestamp %in% salivaX$timestamp, salivaX$timestamp %in% logX$timestamp)
    
    if (identical(any(label, tube, timestamp), FALSE) | nrow(logX)!=nrow(salivaX)) {
        prob=c(prob, common[i])
    } else {
        # the first mark that fits for merging
        pos=which(c(label, tube, timestamp)==TRUE)[1]
        resultX=merge(logX, salivaX[c(mark_saliva[pos], "Cortisol.nmol.l", "run.Cortisol.in.duplicate")], by.x=mark_log[pos], by.y=mark_saliva[pos], all.x=TRUE, sort=FALSE)
        result=rbind(result, resultX)
    }
}
write.table(result[c("ordnung", "uid", "Label", "Number", "timestamp", "SalivaWindow", "eaten", "Cortisol.nmol.l", "run.Cortisol.in.duplicate")], "merged_saliva.txt", sep="\t", row.names = FALSE, quote=FALSE)

# update exempt table
{
    update.exempt=readline("Do you want to update the exempt list (y/n)")
    if (grepl("y", update.exempt)) {
        exempt=read.table(exempt.list, header=TRUE, sep=",", comment.char = "#", stringsAsFactors = FALSE)
        prob=prob[!prob %in% exempt$uid]
        if (length(prob)==0) {
            print("All problematic ones are in the exempt list")
        } else {
            for (m in 1:length(prob)) {
                print(paste0("There are ", length(prob)-m+1, " left"))
                print(paste("The ID is ", prob[m], "W1", sep=""))
                logX=log[log$uid==prob[m], c("ordnung", "uid", "Label", "Number", "timestamp", "SalivaWindow", "eaten")]
                salivaX=saliva[saliva$uid==prob[m], c("uid", "Tube.Label", "Tube..", "Cortisol.nmol.l", "run.Cortisol.in.duplicate", "timestamp")]
                repeat {
                    print("The log looks like")
                    print(cbind(logX[order(logX$Label),], seq=1:nrow(logX)))
                    print("The lab results look like")
                    print(cbind(salivaX[order(salivaX$Tube.Label),], seq=1:nrow(salivaX)))
                    exempt.add=readline("Add this participant to the exempt list (y/n)?")
                    if (grepl("y", exempt.add)) {
                        merge.var=readline(
                            "Please choose which variable to use for merging (input the number of your selection \n
                        1. Label \n
                        2. Tube Number \n
                        3. Timestamp"
                        )
                        if (merge.var %in% c("1", "2", "3")) {
                            pos=as.numeric(merge.var)
                        } else {
                            print("couldn't recognize your choice")
                        }
                        write(paste0(prob[m], ",", mark_log[pos]), exempt.list, append=TRUE)
                    }
                    continue=readline("Continue (y/n)?")
                    if (grepl("y", continue)) break
                }
            }
        }
    }
}

# merge the previously exempt ones
{
    prev.exempt=readline("Do you want to merge the previously exempt ones (y/n)?")
    if (grepl("y", prev.exempt)) {
        result = read.table("merged_saliva.txt", header = TRUE, sep="\t", stringsAsFactors = FALSE)
        exempt = read.table(exempt.list, header = TRUE, sep=",", comment.char = "#", stringsAsFactors = FALSE)
        exempt = exempt[!duplicated(exempt$uid),]
        if (nrow(exempt)>0) {
            for (k in 1:nrow(exempt)) {
                logX=log[log$uid==exempt$uid[k], c("ordnung", "uid", "Label", "Number", "timestamp", "SalivaWindow", "eaten")]
                salivaX=saliva[saliva$uid==exempt$uid[k], c("uid", "Tube.Label", "Tube..", "Cortisol.nmol.l", "run.Cortisol.in.duplicate", "timestamp")]
                pos=grep(exempt$variable[k], mark_log)
                resultX=merge(logX, salivaX[c(mark_saliva[pos], "Cortisol.nmol.l", "run.Cortisol.in.duplicate")], by.x=mark_log[pos], by.y=mark_saliva[pos], all=FALSE, sort=FALSE)
                result=rbind(result, resultX)
            }
            write.table(result, "merged_saliva.txt", sep="\t", row.names = FALSE, quote=FALSE)
        } else {
            x=1
            while(x<=5) {x=x+1; print("The exempt list is empty")}
        }
    }
}

# build in interactive functions for merging problematic ones
result=read.table("merged_saliva.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
# create output for manually input ones
result.manual=read.table(result.manual, header=TRUE, sep="\t", stringsAsFactors = FALSE, comment.char = "#")
result=rbind(result, result.manual)
# take the merged ones off the list
prob=prob[!prob %in% result$uid]

{
    for (j in 1:length(prob)) {
        print("Something is wrong with the data, no unique ID, Label, TubeNum, or Time can be found for merging")
        print(paste("The ID is ", prob[j], "W1", sep=""))
        logX=log[log$uid==prob[j], c("ordnung", "uid", "Label", "Number", "timestamp", "SalivaWindow", "eaten")]
        salivaX=saliva[saliva$uid==prob[j], c("uid", "Tube.Label", "Tube..", "Cortisol.nmol.l", "run.Cortisol.in.duplicate", "timestamp")]
        repeat {
            print("The log looks like")
            print(cbind(logX[order(logX$Label),], seq=1:nrow(logX)))
            print("The lab results look like")
            print(cbind(salivaX[order(salivaX$Tube.Label),], seq=1:nrow(salivaX)))
            manual=readline("Do you want to manually input the data for this one (y/n)?")
            if (grepl("y", manual)) {
                resultX=logX
                resultX$Cortisol.nmol.l="NA"
                resultX$run.Cortisol.in.duplicate="NA"
                fix(resultX)
                done=readline("Can the results be merged with the log (y/n)")
                if (grepl("y", done)) {
                    result.manual=rbind(result.manual, resultX)
                    write.table(result.manual, "merged_saliva_manual.txt", sep="\t", row.names=FALSE, quote=FALSE)
                    result=rbind(result, resultX)
                    write.table(result, "merged_saliva.txt", sep="\t", row.names=FALSE, quote=FALSE)
                    break
                }
            } else {
                done=readline("Are you done with this one (y/n)")
                if (grepl("y", done)) {break}
            }
            
        }
        continue=readline("Shall we continue? (y/n)")
        if (grepl("n", continue)) {
            stop(paste("Stopped before the end of log, ID = ", prob[j], "W1", sep=""))
        } else {
            print(paste0("OK, you have ", length(prob)-j, " left"))
        }
    } 
}

result=read.table("merged_saliva.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)
sequence=data.frame(ordnung=1:max(log$ordnung))
sequence=merge(sequence, result[c("ordnung", "Cortisol.nmol.l", "run.Cortisol.in.duplicate")], by="ordnung", all.x=TRUE, sort=FALSE)
sequence=sequence[order(sequence$ordnung),]
write.table(sequence, "merged_saliva_by_sequence.txt", sep="\t", row.names=FALSE, quote=FALSE)
