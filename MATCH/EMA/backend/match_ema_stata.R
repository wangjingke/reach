# function to generate STATA script to label all variables
ema.stata = function(script) {
    source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_keys.R")
    recode = 'mvdecode _all, mv(99999=. ) \n ds, has(type string) \n quietly foreach var in `r(varlist)\' { \n replace `var\' = "" if `var\' =="99999" \n}' # recode 99999 to missing in STATA
    # EMA
    emaMother=data.frame(variable=paste0("m", keys.mother$variable), label=paste0(keys.mother$short, ifelse(is.na(keys.mother$choices), "", paste0("; ", keys.mother$choices))), stringsAsFactors = FALSE)
    emaChild=data.frame(variable=paste0("c", keys.child$variable), label=paste0(keys.child$short, ifelse(is.na(keys.child$choices), "", paste0("; ", keys.child$choices))), stringsAsFactors = FALSE)
    # ACC
    acc=c(
        c(outer(outer(c("valid", "nonvalid", "sed", "light", "mod", "vig", "mvpa"), c("_15", "_30", "_60", "_120"), paste0), c("_before", "_after"), paste0)),
        paste0(outer(c("valid", "nonvalid", "sed", "light", "mod", "vig", "mvpa"), c("_30", "_60", "_120", "_240"), paste0), "_window")
    )
    accLabel=function(x, prefix="", daily = FALSE) {
        segX=strsplit(x, "_")[[1]]
        activity = ifelse(daily, segX[2], segX[1])
        if (prefix=="o") activity=substring(activity, 2)
        type = switch (activity,
            "valid" = "valid",
            "nonvalid" = "nonvalid",
            "sed" = "sedentary",
            "light" = "light",
            "mod" = "moderate",
            "vig" = "vigorous",
            "mvpa" = "MVPA"
        )
        if (daily) {
            labelX = paste("date specific cumulative", type, "in minutes", sep=" ")
        } else {
            direction = switch (segX[3], "before" = "before", "after" = "after", "window" = "around")
            labelX = ifelse (prefix=="o",
                             paste(type, segX[2], "min", direction, "the prompt of the counterpart", sep = " "),
                             paste(type, segX[2], "min", direction, "the prompt", sep = " ")
                             )
        }
        return(labelX)
    }
    emaAcc=data.frame(variable=acc, label=sapply(acc, accLabel), stringsAsFactors = FALSE, row.names = NULL)
    # OACC
    oAcc=paste0("o", acc)
    emaOacc=data.frame(variable=oAcc, label=sapply(oAcc, accLabel, prefix="o"), stringsAsFactors = FALSE, row.names = NULL)
    # daily averaged physical activity
    dAcc=paste0("day_", c("nonvalid", "valid", "sed", "light", "mod", "vig", "mvpa"))
    emaDacc=data.frame(variable=dAcc, label=sapply(dAcc, accLabel, daily=TRUE), stringsAsFactors = FALSE, row.names = NULL)
    # manual adds-on
    manual=data.frame(variable=NA, label=NA, stringsAsFactors = FALSE)
    manual[1,]=c("comply", "survey finished, yes/no")
    manual[2,]=c("complete", "survey completed till the last question, yes/no")

    # anchored
    regular = rbind(emaMother, emaChild, emaAcc, emaOacc, manual[manual$variable %in% c("comply", "complete")])
    anchor = c("1p1", "1p2", "1m1", "1m2", "0c", "0p1", "0p2", "0m1", "0m2")
    emaAnchor = data.frame(variable=c(outer(regular$variable, anchor, paste, sep="_")), label=NA, stringsAsFactors = FALSE)
    anchorLabel = function(x) {
        type = switch (tail(strsplit(x, "_")[[1]], 1),
            "1p1" = "; +1, self",
            "1p2" = "; +2, self",
            "1m1" = "; -1, self",
            "1m2" = "; -2, self",
            "0c" = "; 0, other",
            "0p1" = "; +1, other",
            "0p2" = "; +2, other",
            "0m1" = "; -1, other",
            "0m2" = "; -2, other"
        )
        return(type)
    }
    for (i in 1:nrow(emaAnchor)) {
        varX=gsub(paste(paste0("_", anchor), collapse = "|"), "", emaAnchor$variable[i])
        emaAnchor$label[i] = paste0(regular$label[which(regular$variable==varX)], anchorLabel(emaAnchor$variable[i]))
    }

    output = rbind(emaMother, emaChild, emaAcc, emaOacc, emaDacc, emaAnchor)
    # output to file
    stataScript = file(script, encoding = "utf-8", "w")
    write(recode, file=stataScript, append = TRUE)
    for (i in 1:nrow(output)) {
        commandX=paste0('label variable ', output$variable[i], ' "', output$label[i], '"')
        write(commandX, file=stataScript, append = TRUE)
    }
    write('label variable tod "Time of the day"', file=stataScript, append = TRUE)
    write('drop rownames', file=stataScript, append = TRUE)
    write('drop file', file=stataScript, append = TRUE)
    write('drop tmrwaketime', file=stataScript, append = TRUE)
    close(stataScript)
}
