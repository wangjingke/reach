/*main*/
log using "Y:\Jing\Genevieve\20160802_SBM\sbm_20170316_main.smcl"
use "Y:\Jing\Genevieve\20160802_SBM\sbm_20170316_main.dta", clear
foreach indepent in rmStress_homewk rmStress_job rmStress_demands rmStress_cowrkr ///
rmStress_spouse rmStress_child totalStressors totalPerceivedStress totalStress mStressed {
	mixed totalActivityParent Child_Gender ChildAge tod wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalEatingParent mBi_BornUS weekend wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalPhysicalActivityParent mBi_college mNchildren tod wave `indepent'_bs `indepent'_ws  || id:, nolog
	mixed totalSedentaryActivityParnt Child_Gender weekend wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalHealthyEatingParent mNchildren tod wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalUnhealthyEatingParent mCC_Grandparent mBMIcat wave `indepent'_bs `indepent'_ws || id:, nolog
}
log close

log using "Y:\Jing\Genevieve\20160802_SBM\sbm_20170316_anc.smcl"
use "Y:\Jing\Genevieve\20160802_SBM\sbm_20170316_anc.dta", clear
foreach indepent in rmStress_homewk rmStress_job rmStress_demands rmStress_cowrkr ///
rmStress_spouse rmStress_child totalStressors totalPerceivedStress totalStress mStressed {
	mixed totalActivityParent_1p1 Child_Gender ChildAge tod wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalEatingParent_1p1 mBi_BornUS weekend wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalPhysicalActivityParent_1p1 mBi_college mNchildren tod wave `indepent'_bs `indepent'_ws  || id:, nolog
	mixed totalSedentaryActivityParnt_1p1 Child_Gender weekend wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalHealthyEatingParent_1p1 mNchildren tod wave `indepent'_bs `indepent'_ws || id:, nolog
	mixed totalUnhealthyEatingParent_1p1 mCC_Grandparent mBMIcat wave `indepent'_bs `indepent'_ws || id:, nolog
}
log close
