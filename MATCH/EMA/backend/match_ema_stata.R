# function to generate STATA script to label all variables
ema.stata = function(script) {
    source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_keys.R")
    recode = 'mvdecode _all, mv(99999=. ) \n ds, has(type string) \n quietly foreach var in `r(varlist)\' { \n replace `var\' = "" if `var\' =="99999" \n}' # recode 99999 to missing in STATA
    # EMA
    emaMother=data.frame(variable=paste0("MOTHER_", keys.mother$variable), label=paste0(keys.mother$short, ifelse(is.na(keys.mother$choices), "", paste0("; ", keys.mother$choices))), stringsAsFactors = FALSE)
    emaChild=data.frame(variable=paste0("CHILD_", keys.child$variable), label=paste0(keys.child$short, ifelse(is.na(keys.child$choices), "", paste0("; ", keys.child$choices))), stringsAsFactors = FALSE)
    # ACC
    acc=c(
        c(outer(outer(c("VALID", "NONVALID", "SED", "LIGHT", "MOD", "VIG", "MVPA"), c("_15", "_30", "_60", "_120"), paste0), c("_BEFORE", "_AFTER"), paste0)),
        paste0(outer(c("VALID", "NONVALID", "SED", "LIGHT", "MOD", "VIG", "MVPA"), c("_30", "_60", "_120", "_240"), paste0), "_WINDOW")
    )
    accLabel=function(x, prefix="", daily = FALSE) {
        segX=strsplit(x, "_")[[1]]
        activity = ifelse(daily, segX[2], segX[1])
        if (prefix=="O") activity=substring(activity, 2)
        type = switch (activity,
            "VALID" = "valid",
            "NONVALID" = "nonvalid",
            "SED" = "sedentary",
            "LIGHT" = "light",
            "MOD" = "moderate",
            "VIG" = "vigorous",
            "MVPA" = "MVPA"
        )
        if (daily) {
            labelX = paste("daily cumulative", type, "in minutes", sep=" ")
        } else {
            direction = switch (segX[3], "BEFORE" = "before", "AFTER" = "after", "WINDOW" = "around")
            labelX = ifelse (prefix=="O",
                             paste(type, segX[2], "min", direction, "the prompt of the counterpart", sep = " "),
                             paste(type, segX[2], "min", direction, "the prompt", sep = " ")
                             )
        }
        return(labelX)
    }
    emaAcc=data.frame(variable=acc, label=sapply(acc, accLabel), stringsAsFactors = FALSE, row.names = NULL)
    # OACC
    oAcc=paste0("O", acc)
    emaOacc=data.frame(variable=oAcc, label=sapply(oAcc, accLabel, prefix="O"), stringsAsFactors = FALSE, row.names = NULL)
    # daily averaged physical activity
    dAcc=paste0("DAY_", c("NONVALID", "VALID", "SED", "LIGHT", "MOD", "VIG", "MVPA"))
    emaDacc=data.frame(variable=dAcc, label=sapply(dAcc, accLabel, daily=TRUE), stringsAsFactors = FALSE, row.names = NULL)
    # manual adds-on
    manual=data.frame(variable=NA, label=NA, stringsAsFactors = FALSE)
    manual[1,]=c("COMPLY", "survey finished, yse/no")
    manual[2,]=c("COMPLETE", "survey completed till the last question, yse/no")

    # anchored
    regular = rbind(emaMother, emaChild, emaAcc, manual[manual$variable %in% c("COMPLY", "COMPLETE")])
    anchor = c("1P1", "1P2", "1M1", "1M2", "0C", "0P1", "0P2", "0M1", "0M2")
    emaAnchor = data.frame(variable=c(outer(regular$variable, anchor, paste, sep="_")), label=NA, stringsAsFactors = FALSE)
    anchorLabel = function(x) {
        type = switch (tail(strsplit(x, "_")[[1]], 1),
            "1P1" = "; +1, self",
            "1P2" = "; +2, self",
            "1M1" = "; -1, self",
            "1M2" = "; -2, self",
            "0C" = "; 0, other",
            "0P1" = "; +1, other",
            "0P2" = "; +2, other",
            "0M1" = "; -1, other",
            "0M2" = "; -2, other"
        )
        return(type)
    }
    for (i in 1:nrow(emaAnchor)) {
        varX=gsub(paste(paste0("_", anchor), collapse = "|"), "", emaAnchor$variable[i])
        emaAnchor$label[i] = paste0(regular$label[which(regular$variable==varX)], anchorLabel(emaAnchor$variable[i]))
    }

    output = rbind(emaMother, emaChild, emaAcc, emaOacc, emaAnchor)
    # output to file
    stataScript = file(script, encoding = "utf-8", "w")
    write(recode, file=stataScript, append = TRUE)
    for (i in 1:nrow(output)) {
        commandX=paste0('label variable ', output$variable[i], ' "', output$label[i], '"')
        write(commandX, file=stataScript, append = TRUE)
    }
    close(stataScript)
}
