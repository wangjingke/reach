setwd("C:/Users/wangjink/Documents/REACH/MADRES/ASA")

pilot = read.csv("X:/MADRES/ASA24 Diet Recall/Raw Data Files/2014 Version - FINAL FILES/MAD1_61324_TNS.csv", header = TRUE)

sumContinous = function (var, data) {
    print(paste0("Var=", var))
    print(summary(unlist(data[var])))
    print(paste0("SD=",sd(unlist(data[var]), na.rm = TRUE)))
}

sapply(c("KCAL", "PROT", "TFAT", "VC", "BCAR"), sumContinous, data=pilot)
