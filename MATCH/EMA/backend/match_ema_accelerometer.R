ema.ACC = function(ema, missing_output, accDir, accList) {
    # generating emaAccMatch for pairing EMA and acc
    ema.emaAccMatch = function(ema, accList) {
        validIndiv=unique(ema[, c("SubjectID", "Date", "DayInStudy")]) # compliance should be 1, otherwise there is no start time for the prompt
        validIndiv$Date = as.Date(validIndiv$Date, format = "%Y-%m-%d", tz = "America/Los_Angeles")
        emaAccMatch = aggregate(Date~SubjectID, data = validIndiv, min)
        emaAccMatch$file=NA

        accList=read.csv(accList, header = TRUE, stringsAsFactors = FALSE)
        accList$date = as.Date(accList$date, format = "%Y-%m-%d", tz = "America/Los_Angeles")

        for (i in 1:nrow(emaAccMatch)) {
            pos=which(accList$SubjectID==emaAccMatch$SubjectID[i] & abs(difftime(accList$date, emaAccMatch$Date[i], units = "days"))<=10)
            if (length(pos)>0) {emaAccMatch$file[i] = accList$file[pos]}
            rm(pos)
        }
        return(emaAccMatch)
    }
    emaAccMatch=ema.emaAccMatch(ema=ema, accList = paste0(accDir, "/", accList))
    # individuals missing ACC data
    accMissing=emaAccMatch[is.na(emaAccMatch$file),]
    write.csv(accMissing, missing_output, row.names = FALSE, quote = FALSE)

    for (i in 1:nrow(emaAccMatch)) {
        # filling in the MVPA variables
        if (!is.na(emaAccMatch$file[i])) {
            accX=readRDS(paste0(accDir, "/", emaAccMatch$file[i]))
            promptLine=which(ema$SubjectID==emaAccMatch$SubjectID[i] & !is.na(ema$COMPLY))
            for (j in promptLine) {
                mvpaX=ema.extractACC(ema$prompt_start[j], accX)
                ema[j, names(mvpaX)]=mvpaX
                rm(mvpaX)
            }
            rm(accX)
            rm(promptLine)
        }
        # filling in the OMVPA variables
        targetID=ifelse(emaAccMatch$SubjectID[i]>=12000, as.numeric(emaAccMatch$SubjectID[i])-1000, as.numeric(emaAccMatch$SubjectID[i])+1000)
        pos = which(emaAccMatch$SubjectID==targetID)
        if (length(pos)>0 && !is.na(emaAccMatch$file[pos])) {
            accX=readRDS(paste0(accDir, "/", emaAccMatch$file[pos]))
            promptLine=which(ema$SubjectID==emaAccMatch$SubjectID[i] & !is.na(ema$COMPLY))
            for (j in promptLine) {
                mvpaX=ema.extractACC(ema$prompt_start[j], accX)
                ema[j, paste0("O", names(mvpaX))]=mvpaX
                rm(mvpaX)
            }
            rm(accX)
            rm(promptLine)
        }
        rm(targetID)
        rm(pos)
        if (i%%10==0) {print(paste0(i, " ", round(i/nrow(emaAccMatch)*100, 2), "%"))}
    }
    return(ema)
}

# integreting accelerometer data to ema
ema.attachACC=function(ema, prefix = "") {
    acc_point=c(outer(outer(c("VALID", "NONVALID", "SED", "LIGHT", "MOD", "VIG", "MVPA"), c("_15", "_30", "_60", "_120"), paste0), c("_BEFORE", "_AFTER"), paste0))
    acc_win=paste0(outer(c("VALID", "NONVALID", "SED", "LIGHT", "MOD", "VIG", "MVPA"), c("_30", "_60", "_120", "_240"), paste0), "_WINDOW")
    varlist=paste0(prefix, c(acc_point, acc_win))
    acc=data.frame(matrix(NA, nrow=nrow(ema), ncol=length(acc_point)+length(acc_win), dimnames = list(c(), varlist)))
    return(cbind(ema, acc))
}

ema.extractACC = function(timepoint, accX) {
    # function to subset by time, and aggregate MVPA (fork from common)
    ema.mvpa = function(accX, start, end) {
        skeleton = data.frame(MVPA = c("light", "moderate", "nonwear", "sedentary", "vigorous"))
        accX.seq = accX$data[accX$data$stamp > start & accX$data$stamp <= end, ]
        mvpa = aggregate(min ~ MVPA, data = accX.seq, sum, na.rm = TRUE)
        result = merge(skeleton, mvpa, by = "MVPA", all.x = TRUE, sort = FALSE)
        result[is.na(result)] = 0
        return(result)
    }
    MVPA=rbind(
        category=c("nonwear", "sedentary", "light", "moderate", "vigorous"),
        var=c("NONVALID", "SED", "LIGHT", "MOD", "VIG")
    )

    output=ema.attachACC(data.frame(1))
    output=output[2:ncol(output)]

    anchor1=strptime(timepoint, format = "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
    if (anchor1<min(accX$data$stamp) | anchor1>max(accX$data$stamp)) {
        return(output)
    } else {
        # loop through all MVPA var
        MinToSec=function(u) {return(u*60)}
        for (k in c(15, 30, 60, 120)) {
            anchor0=anchor1-MinToSec(k)
            anchor2=anchor1+MinToSec(k)
            # before prompt
            mvpaX=ema.mvpa(accX, start=anchor0, end=anchor1)
            for (m in 1:nrow(mvpaX)) {
                type=MVPA["var", grep(mvpaX$MVPA[m], MVPA["category",])]
                output[1, paste0(type, "_", k, "_BEFORE")]=mvpaX[m, "min"]
            }
            # after prompt
            mvpaX=ema.mvpa(accX, start=anchor1, end=anchor2)
            for (m in 1:nrow(mvpaX)) {
                type=MVPA["var", grep(mvpaX$MVPA[m], MVPA["category",])]
                output[1, paste0(type, "_", k, "_AFTER")]=mvpaX[m, "min"]
            }
        }
        for (k in c(15, 30, 60, 120)) {
            output[1, paste0("VALID_", k, "_BEFORE")]=k-output[1, paste0("NONVALID_", k, "_BEFORE")]
            output[1, paste0("MVPA_", k, "_BEFORE")]=output[1, paste0("MOD_", k, "_BEFORE")]+output[1, paste0("VIG_", k, "_BEFORE")]
            output[1, paste0("VALID_", k, "_AFTER")]=k-output[1, paste0("NONVALID_", k, "_AFTER")]
            output[1, paste0("MVPA_", k, "_AFTER")]=output[1, paste0("MOD_", k, "_AFTER")]+output[1, paste0("VIG_", k, "_AFTER")]
            for (m in c("VALID", "NONVALID", "SED", "LIGHT", "MOD", "VIG", "MVPA")) {
                output[1, paste0(m, "_", 2*k, "_WINDOW")]=output[1, paste0(m, "_", k, "_BEFORE")]+output[1, paste0(m, "_", k, "_AFTER")]
            }
        }
        return(output)
    }
}

# merge daily physical activity
ema.dailyPA = function(ema, AccSummary) {
    accSum = read.table(AccSummary, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    colnames(accSum)[colnames(accSum)=="nonvalid"]="DAY_NONVALID"
    colnames(accSum)[colnames(accSum)=="valid"]="DAY_VALID"
    colnames(accSum)[colnames(accSum)=="sedentary"]="DAY_SED"
    colnames(accSum)[colnames(accSum)=="light"]="DAY_LIGHT"
    colnames(accSum)[colnames(accSum)=="moderate"]="DAY_MOD"
    colnames(accSum)[colnames(accSum)=="vigorous"]="DAY_VIG"
    colnames(accSum)[colnames(accSum)=="mvpa"]="DAY_MVPA"

    output=merge(ema, accSum[c("SubjectID", "Date", grep("DAY_", names(accSum), value=TRUE))], by=c("SubjectID", "Date"), all.x=TRUE, sort=FALSE)
    return(output)
}
