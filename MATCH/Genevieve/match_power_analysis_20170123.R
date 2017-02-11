setwd('Y:/Jing/Genevieve/20170103_matchRenew/power_analysis')

sleep = read.csv('Wave2_Sleep.csv', header = TRUE, stringsAsFactors = FALSE)

# icc
library(irr)
icc(sleep[sprintf('MSTR%d', 2:7)], model = 'oneway', unit = 'average')
icc(sleep[sprintf('CSTR%d', 2:7)], model = 'oneway', unit = 'average')
icc(sleep[sprintf('MSLP%d', 2:7)], model = 'oneway', unit = 'average')
icc(sleep[sprintf('CSLP%d', 2:7)], model = 'oneway', unit = 'average')



long = data.frame(DID = c(sapply(unique(sleep$did), rep, 6)), day = rep(2:7, length(unique(sleep$did))))
long$mSTR = NA
long$cSTR = NA
long$mSLP = NA
long$cSLP = NA
for (i in 1:nrow(long)) {
    for (j in c('mSTR', 'cSTR', 'mSLP', 'cSLP')) {
        long[i, j] = sleep[sleep$did == long$DID[i], paste0(toupper(j), long$day[i])]
    }
}

wsbs = function(data, var) {
    indivMean = setNames(aggregate(eval(substitute(y~DID, list(y = as.name(var)))), data = data, mean, na.rm = TRUE), c('DID', 'indivMean'))
    grandMean = mean(indivMean$indivMean, na.rm = TRUE)
    indivMean[paste0(var, '_bs')] = indivMean$indivMean - grandMean
    
    wsbs = merge(data[c('DID', 'day', var)], indivMean, by.x = 'DID', by.y = 'DID', all.x = TRUE, sort = FALSE)
    wsbs[paste0(var, '_ws')] = wsbs[,var] - wsbs$indivMean
    
    data = merge(data, wsbs[c('DID', 'day', paste0(var, '_bs'), paste0(var, '_ws'))], by = c('DID', 'day'), all.x = TRUE, sort = FALSE)
    
    return (data)
}

long = wsbs(long, 'mSTR')
long = wsbs(long, 'cSTR')

standardize = function(x) {
    x = as.numeric(x)
    return ((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
}

long$mSTR_bs_st = standardize(long$mSTR_bs)
long$mSTR_ws_st = standardize(long$mSTR_ws)
long$cSTR_bs_st = standardize(long$cSTR_bs)
long$cSTR_ws_st = standardize(long$cSTR_ws)
long$mSLP_st = standardize(long$mSLP)
long$cSLP_st = standardize(long$cSLP)
long[is.na(long)] = ''

write.csv(long, 'power_analysis_sleep_20170123_long.csv', row.names = FALSE, quote = FALSE)

# transform
did = unique(long$DID)
wideName = c('DID', outer(names(long)[3:ncol(long)], paste0('_', c(2:7)), paste0))
wide = data.frame(matrix(ncol = length(wideName), nrow = length(did), dimnames = list(c(), wideName)))
wide$DID = did
for (i in 1:nrow(long)) {
    lineX = grep(long$DID[i], wide$DID)
    for (j in 3:ncol(long)) {
        wide[lineX, paste0(names(long)[j], '_', long$day[i])] = long[i, names(long)[j]]
    }
}

write.csv(wide, 'power_analysis_sleep_20170123_wide.csv', row.names = FALSE, quote = FALSE)
