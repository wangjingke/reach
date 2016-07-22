* merge dietary and demographic info
clear
import delimited Y:\Jing\Sydney\MATCH_EMA_Diet_20160719.csv, case(preserve)
rename ID Child_ID
merge m:1 Child_ID using "C:\Users\wangjink\Documents\REACH\MATCH\Project MATCH\MATCH_PERSON_V13.DTA", keep(master match) keepusing(Childage Child_Gender cHispanic cRace_Native cRace_Asian cRace_Black cRace_Hawaiian cRace_White cRace_Other Income incomeQ cAvgWaist cBMI cBMIcat cBMIpercentile)
