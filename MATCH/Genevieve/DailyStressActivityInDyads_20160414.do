use "Y:\Jing\Genevieve\20160418_DailyStressActivityInDyads\activity_20160426.dta", clear

mixed CHILD_STRESSED ///
	MOTHER_STRESSED_0C_BS MOTHER_STRESSED_0C_WS ///
	OMVPA_120_BEFORE_BS OMVPA_120_BEFORE_WS ///
	MVPA_120_BEFORE_BS MVPA_120_BEFORE_WS ///
	OSED_120_BEFORE_BS OSED_120_BEFORE_WS ///
	SED_120_BEFORE_BS SED_120_BEFORE_WS ///
	if MOTHER==0 & NONVALID_120_BEFORE<60 & ONONVALID_120_BEFORE<60 || DID:

mixed MOTHER_STRESSED ///
	CHILD_STRESSED_0C_BS CHILD_STRESSED_0C_WS ///
	MVPA_120_BEFORE_BS MVPA_120_BEFORE_WS ///
	OMVPA_120_BEFORE_BS OMVPA_120_BEFORE_WS ///
	SED_120_BEFORE_BS SED_120_BEFORE_WS ///
	OSED_120_BEFORE_BS OSED_120_BEFORE_WS ///
	if MOTHER==1 & NONVALID_120_BEFORE<60 & ONONVALID_120_BEFORE<60 || DID:
	
	

mixed CHILD_STRESSED ///
	MOTHER_STRESSED_0C_BS MOTHER_STRESSED_0C_WS ///
	OSED_120_BEFORE_BS OSED_120_BEFORE_WS ///
	SED_120_BEFORE_BS SED_120_BEFORE_WS ///
	if MOTHER==0 & NONVALID_120_BEFORE<60 & ONONVALID_120_BEFORE<60 || DID:

mixed MOTHER_STRESSED ///
	CHILD_STRESSED_0C_BS CHILD_STRESSED_0C_WS ///
	SED_120_BEFORE_BS SED_120_BEFORE_WS ///
	OSED_120_BEFORE_BS OSED_120_BEFORE_WS ///
	if MOTHER==1 & NONVALID_120_BEFORE<60 & ONONVALID_120_BEFORE<60 || DID:	
	
	
	
mixed CHILD_STRESSED ///
	MOTHER_STRESSED_0C_BS MOTHER_STRESSED_0C_WS ///
	OMVPA_120_BEFORE_BS OMVPA_120_BEFORE_WS ///
	MVPA_120_BEFORE_BS MVPA_120_BEFORE_WS ///
	if MOTHER==0 & NONVALID_120_BEFORE<60 & ONONVALID_120_BEFORE<60 || DID:

mixed MOTHER_STRESSED ///
	CHILD_STRESSED_0C_BS CHILD_STRESSED_0C_WS ///
	MVPA_120_BEFORE_BS MVPA_120_BEFORE_WS ///
	OMVPA_120_BEFORE_BS OMVPA_120_BEFORE_WS ///
	if MOTHER==1 & NONVALID_120_BEFORE<60 & ONONVALID_120_BEFORE<60 || DID:
	


mixed logOMVPA_120_AFTER ///
	MOTHER_STRESSED_BS MOTHER_STRESSED_WS ///
	CHILD_STRESSED_0C_BS CHILD_STRESSED_0C_WS ///
	MVPA_120_AFTER_BS MVPA_120_AFTER_WS ///
	if MOTHER==1 & NONVALID_120_AFTER<60 & ONONVALID_120_AFTER<60 || DID:

mixed logOMVPA_120_AFTER ///
	MOTHER_STRESSED_0C_BS MOTHER_STRESSED_0C_WS ///
	CHILD_STRESSED_BS CHILD_STRESSED_WS ///
	MVPA_120_AFTER_BS MVPA_120_AFTER_WS ///
	if MOTHER==0 & NONVALID_120_AFTER<60 & ONONVALID_120_AFTER<60 || DID:


	
mixed OSED_120_AFTER ///
		MOTHER_STRESSED_BS MOTHER_STRESSED_WS ///
		CHILD_STRESSED_0C_BS CHILD_STRESSED_0C_WS /// 
		SED_120_AFTER_BS SED_120_AFTER_WS ///
		if MOTHER==1 & NONVALID_120_AFTER<60 & ONONVALID_120_AFTER<60 || DID:

mixed OSED_120_AFTER ///
		MOTHER_STRESSED_0C_BS MOTHER_STRESSED_0C_WS ///
		CHILD_STRESSED_BS CHILD_STRESSED_WS /// 
		SED_120_AFTER_BS SED_120_AFTER_WS ///
		if MOTHER==0 & NONVALID_120_AFTER<60 & ONONVALID_120_AFTER<60 || DID:



