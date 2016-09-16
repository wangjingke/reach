use "Y:\Jing\Genevieve\20160802_SBM\sbm_20160908_ancillary.dta", clear

// Table 4
foreach indepent in rMOTHER_STRESS_HOMEWK rMOTHER_STRESS_JOB rMOTHER_STRESS_DEMANDS ///
rMOTHER_STRESS_COWRKR rMOTHER_STRESS_SPOUSE rMOTHER_STRESS_CHILD totalPerceivedStress {
	mixed totalActivityParent_1P1 Child_Gender Childage TOD `indepent'_BS `indepent'_WS || SubjectID:, nolog
	mixed totalEatingParent_1P1 Bi_BornUS weekend `indepent'_BS `indepent'_WS || SubjectID:, nolog
	mixed totalPhysicalActivityParent_1P1 Bi_college Nchildren TOD `indepent'_BS `indepent'_WS  || SubjectID:, nolog
	mixed totalSedentaryActivityParnt_1P1 Child_Gender weekend `indepent'_BS `indepent'_WS || SubjectID:, nolog
	mixed totalHealthyEatingParent_1P1 Nchildren TOD `indepent'_BS `indepent'_WS || SubjectID:, nolog
	mixed totalUnhealthyEatingParent_1P1 CC_Grandparent mBMIcat `indepent'_BS `indepent'_WS || SubjectID:, nolog
}
