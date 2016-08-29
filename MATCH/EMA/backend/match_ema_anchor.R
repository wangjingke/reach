# ema.anchor=function(ema) {
#     t0=Sys.time()
#     # variable list to do anchor
#     # need to find ways to exclude the OMVPA variables
#     varList=grep("^(MOTHER|CHILD|VALID|NONVALID|SED|LIGHT|MOD|VIG|MVPA|COMPLY|COMPLETE).*", names(ema), value = TRUE)
#     anchor=c("1P1", "1P2", "1M1", "1M2", "0C", "0P1", "0P2", "0M1", "0M2")
#     extra=data.frame(matrix(NA, nrow=nrow(ema), ncol=length(varList)*length(anchor), dimnames = list(c(), c(outer(varList, anchor, paste, sep="_")))))
#     ema=cbind(ema, extra)
#
#     for (i in 1:nrow(ema)) {
#         id=ema$SubjectID[i]
#         day=ema$DayInStudy[i]
#         win=as.numeric(ema$win_seq[i])
#         for(j in 1:length(varList)) {
#             for(k in 1:length(anchor)) {
#                 object=substring(anchor[k], 1, 1)
#                 direction=substring(anchor[k], 2, 2)
#                 distance=as.numeric(substring(anchor[k], 3, 3))
#
#                 idX=switch(object, "1"=id, "0"=ifelse(id>=12000, as.numeric(id)-1000, as.numeric(id)+1000))
#                 winX=switch(direction, "P"=win+distance, "M"=win-distance, "C"=win)
#
#                 pos=which(ema$SubjectID==idX & ema$DayInStudy==day & ema$win_seq==winX)
#                 if (length(pos)>0) {
#                     ema[i, paste0(varList[j], "_", anchor[k])]=ema[pos, varList[j]]
#                 }
#             }
#         }
#         print(paste0("i=", i, "; ", round(i/nrow(ema)*100, 2), "%; ", "estimated time remaining = ", round(difftime(Sys.time(), t0, units = "min")*(nrow(ema)-i)/i, 2), "min"))
#     }
#     return(ema)
# }

# ema anchor function for parallele computing
ema.anchor.parallel=function(emaSeg, ema, node, checkpoint) {
    t0=Sys.time()
    # variable list to do anchor
    varList=grep("MOTHER|CHILD|VALID|NONVALID|SED|LIGHT|MOD|VIG|MVPA|COMPLY|COMPLETE", names(emaSeg), value = TRUE)
    anchor=c("1P1", "1P2", "1M1", "1M2", "0C", "0P1", "0P2", "0M1", "0M2")
    extra=data.frame(matrix(NA, nrow=nrow(emaSeg), ncol=length(varList)*length(anchor), dimnames = list(c(), c(outer(varList, anchor, paste, sep="_")))))
    emaSeg=cbind(emaSeg, extra)

    for (i in 1:nrow(emaSeg)) {
        id=emaSeg$SubjectID[i]
        day=emaSeg$DayInStudy[i]
        win=as.numeric(emaSeg$win_seq[i])
        for(j in 1:length(varList)) {
            for(k in 1:length(anchor)) {
                object=substring(anchor[k], 1, 1)
                direction=substring(anchor[k], 2, 2)
                distance=as.numeric(substring(anchor[k], 3, 3))

                idX=switch(object, "1"=id, "0"=ifelse(id>=12000, as.numeric(id)-1000, as.numeric(id)+1000))
                winX=switch(direction, "P"=win+distance, "M"=win-distance, "C"=win)

                pos=which(ema$SubjectID==idX & ema$DayInStudy==day & ema$win_seq==winX)
                if (length(pos)>0) {
                    emaSeg[i, paste0(varList[j], "_", anchor[k])]=ema[pos, varList[j]]
                }
                rm(object)
                rm(direction)
                rm(distance)
                rm(idX)
                rm(winX)
                rm(pos)
            }
        }
        rm(id)
        rm(day)
        rm(win)
        if (i %% 10 == 0) {
            cat(paste0("node=", m, "; i=", i, "; ", round(i/nrow(emaSeg)*100, 2), "%; ", "estimated time remaining = ", round(difftime(Sys.time(), t0, units = "min")*(nrow(emaSeg)-i)/i, 2), "min \n"), file=checkpoint, append=TRUE)
        }
    }
    return(emaSeg)
}
