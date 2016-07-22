setwd("R:/")
library(haven)

pem = read_sas("X:/Maternal and Child Health Study/Pilot/Jing/microPEM/mpem_pilotdata_jing.sas7bdat")

test = pem[pem$participant_ID=="N0179",]

pemX = test
pem.process = function(pemX, id, epoch = 60) {
    data = pemX[,c("date", "mpem_time", "Vector_Sum_Composite", "RH_Corrected_Nephelometer")]
    # convert seconds since midnight to actual time
    convSec = function (x) {
        add0 = function (y) {paste0(rep("0", 2-nchar(y)), y)}
        x = as.numeric(x)
        hr = x %/% 3600
        min = (x-hr*3600) %/% 60
        sec = x-hr*3600-min*60
        return(paste0(add0(hr), ":", add0(min), ":", add0(sec)))
    }
    
    data$timestamp = strptime(paste0(data$date, " ", sapply(data$mpem_time, convSec)), format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
}
