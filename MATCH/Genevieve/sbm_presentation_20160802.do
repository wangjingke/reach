use "Y:\Jing\Genevieve\20160802_SBM\sbm_20160808_main.dta"

// Table 4
//check for normality of outcome, no need to log transformation
foreach depent in totalPhysicalActivityParenting totalHealthyEatingParenting {
	foreach indepent in totalPerceivedStress totalStressors totalStress {
		mixed `depent' `indepent'_BS `indepent'_WS Bi_college /// 
		Nchildren TOD Bi_CC_Fred Hispanic cHispanic HouseholdSize ///
		Child_Gender Age WEEKEND Bi_Fulltime Childage Bi_BornUS || ID:
	}
}

// Table 5
foreach depent in totalPhysicalActivityParenting totalHealthyEatingParenting {
	foreach indepent in rMOTHER_STRESS_HOMEWK rMOTHER_STRESS_JOB ///
	rMOTHER_STRESS_DEMANDS rMOTHER_STRESS_COWRKR rMOTHER_STRESS_SPOUSE ///
	rMOTHER_STRESS_CHILD rMOTHER_STRESS_ELSE{
		mixed `depent' `indepent'_BS `indepent'_WS Bi_college /// 
		Nchildren TOD Bi_CC_Fred Hispanic cHispanic HouseholdSize ///
		Child_Gender Age WEEKEND Bi_Fulltime Childage Bi_BornUS || ID:
	}
}

// Table 6
// recode limitTV and limitJunkfood to binary
generate rMOTHER_LIMITTV_b = rMOTHER_LIMITTV
replace rMOTHER_LIMITTV_b = 1 if rMOTHER_LIMITTV == 0.5
generate rMOTHER_LIMITJUNKFOOD_b = rMOTHER_LIMITJUNKFOOD
replace rMOTHER_LIMITJUNKFOOD_b = 1 if rMOTHER_LIMITJUNKFOOD == 0.5

foreach depent of varlist MOTHER_GOPLAY MOTHER_TAKEPLAY rMOTHER_ASKTV ///
rMOTHER_LIMITTV_b MOTHER_GOFRESH MOTHER_COOKFRESH rMOTHER_ASKJUNKFOOD ///
rMOTHER_LIMITJUNKFOOD_b {
	foreach indepent of varlist rMOTHER_STRESS_HOMEWK rMOTHER_STRESS_JOB ///
	rMOTHER_STRESS_DEMANDS rMOTHER_STRESS_COWRKR rMOTHER_STRESS_SPOUSE ///
	rMOTHER_STRESS_CHILD rMOTHER_STRESS_ELSE totalStressors totalPerceivedStress totalStress {
		melogit `depent' `indepent'_BS `indepent'_WS Bi_college Nchildren TOD ///
		Bi_CC_Fred Hispanic cHispanic HouseholdSize Child_Gender Age WEEKEND ///
		Bi_Fulltime Childage Bi_BornUS || ID:, nolog iterate(1000)
	}
}

		
use "Y:\Jing\Genevieve\20160802_SBM\sbm_20160808_ancillary.dta", clear
// Table 4
//check for normality of outcome, no need to log transformation
foreach depent in totalPhysicalActivityPrntng_1P1 totalHealthyEatingParenting_1P1 {
	foreach indepent in totalPerceivedStress totalStressors totalStress {
		mixed `depent' `indepent'_BS `indepent'_WS Bi_college /// 
		Nchildren TOD Bi_CC_Fred Hispanic cHispanic HouseholdSize ///
		Child_Gender Age WEEKEND Bi_Fulltime Childage Bi_BornUS || ID:
	}
}

// Table 5
foreach depent in totalPhysicalActivityPrntng_1P1 totalHealthyEatingParenting_1P1 {
	foreach indepent in rMOTHER_STRESS_HOMEWK rMOTHER_STRESS_JOB ///
	rMOTHER_STRESS_DEMANDS rMOTHER_STRESS_COWRKR rMOTHER_STRESS_SPOUSE ///
	rMOTHER_STRESS_CHILD rMOTHER_STRESS_ELSE {
		mixed `depent' `indepent'_BS `indepent'_WS Bi_college /// 
		Nchildren TOD Bi_CC_Fred Hispanic cHispanic HouseholdSize ///
		Child_Gender Age WEEKEND Bi_Fulltime Childage Bi_BornUS || ID:
	}
}

// Table 6
// recode limitTV and limitJunkfood to binary
generate rMOTHER_LIMITTV_1P1_b = rMOTHER_LIMITTV_1P1
replace rMOTHER_LIMITTV_1P1_b = 1 if rMOTHER_LIMITTV_1P1 == 0.5
generate rMOTHER_LIMITJUNKFOOD_1P1_b = rMOTHER_LIMITJUNKFOOD_1P1
replace rMOTHER_LIMITJUNKFOOD_1P1_b = 1 if rMOTHER_LIMITJUNKFOOD_1P1 == 0.5

foreach depent in MOTHER_GOPLAY_1P1 MOTHER_TAKEPLAY_1P1 rMOTHER_ASKTV_1P1 rMOTHER_LIMITTV_1P1_b ///
MOTHER_GOFRESH_1P1 MOTHER_COOKFRESH_1P1 rMOTHER_ASKJUNKFOOD_1P1 rMOTHER_LIMITJUNKFOOD_1P1_b {
	foreach indepent in rMOTHER_STRESS_HOMEWK rMOTHER_STRESS_JOB rMOTHER_STRESS_DEMANDS ///
	rMOTHER_STRESS_COWRKR rMOTHER_STRESS_SPOUSE rMOTHER_STRESS_CHILD rMOTHER_STRESS_ELSE ///
	totalStressors totalPerceivedStress totalStress {
		
			melogit `depent' `indepent'_BS `indepent'_WS Bi_college /// 
			Nchildren TOD Bi_CC_Fred Hispanic cHispanic HouseholdSize ///
			Child_Gender Age WEEKEND Bi_Fulltime Childage Bi_BornUS || ID:, nolog
		
	}
}






