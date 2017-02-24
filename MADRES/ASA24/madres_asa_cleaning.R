setwd("D:/REACH/MADRES/ASA/data")

target <- "X:/MADRES/ASA24 Diet Recall/Raw Data Files/2016 Version 093016"

total14 <- read.csv("X:/MADRES/ASA24 Diet Recall/Raw Data Files/2014 Version - FINAL FILES/MAD1_61323_TNMYPHEI.csv", header = TRUE, stringsAsFactors = FALSE)
# change asa14 variable name to align with asa 16
colnames(total14)[names(total14)=="G_WHL"] <- "G_WHOLE"
colnames(total14)[names(total14)=="ADD_SUG"] <- "ADD_SUGARS"

total16.1 <- read.csv("X:/MADRES/ASA24 Diet Recall/Raw Data Files/2016 Version 093016/MMAD1_15670_Totals.csv", header = TRUE, stringsAsFactors = FALSE)
total16.2 <- read.csv("X:/MADRES/ASA24 Diet Recall/Raw Data Files/2016 Version 093016/MAD2_15664_Totals.csv", header = TRUE, stringsAsFactors = FALSE)
common = intersect(names(total14), names(total16))

total =  rbind(total14[common], total16[common])

# subjectID
extractID = function(x) {
    temp = substring(gsub("[[:alpha:]]", "", x), 4)
    return(substring(temp, 1, nchar(temp)-2))
}

total$id = extractID(total$UserName)



# common nutrient check
basic = c("KCAL", "PROT", "TFAT", "VC", "BCAR")
total[order(total$id), c("id", basic)]



# create variables for dietary recall

# daily servings of fruit
# F_TOTAL
# daily servings of vegetables
# V_TOTAL
# daily servings of sugar sweetened beverages

# daily added sugars
# ADD_SUGARS
# daily glycemic load

# daily energy
# KCAL
# daily total SFA
# SFAT
# daily dietary fiber
# FIBE
# daily total fat
# TFAT
# daily total carbohydrate
# CARB
# daily total protein
# PROT
# daily total sugar
# SUGR
# daily total whole grains
# G_WHOLE

varlist = c("F_TOTAL", "V_TOTAL", "KCAL", "VC", "SFAT", "FIBE", "TFAT", "CARB", "PROT", "SUGR", "G_WHOLE")

batchAggregate = function(varlist, anchor, data) {
    output = data.frame(anchor=unique(data[anchor]))
    for (i in varlist) {
        depentName = i
        outputX = setNames(aggregate(get(i)~get(anchor), data = data, mean), c(anchor, i))
        output = merge(output, outputX, by=anchor, all=TRUE, sort=FALSE)
    }
    return(output)
}

dailyTotal = batchAggregate(varlist, "id", total)

ptVarlist = c("F_TOTAL", "FIBE", "TFAT", "CARB", "PROT", "SUGR", "G_WHOLE")
for (i in ptVarlist) {
    dailyTotal[paste0(i, "_std")] = dailyTotal[i]/dailyTotal["KCAL"]*1000
}

dailyTotal



