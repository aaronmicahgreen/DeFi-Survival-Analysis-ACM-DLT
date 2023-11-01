createSurvDataTable <- function(survData, strata, rmstMaxTime = -1) {
  survData[[strata]] <- as.factor(survData[[strata]])
  survFit <- surv_fit(as.formula(paste0("Surv(time, status) ~ ", strata)), data = survData)
  coxFit <- coxph(as.formula(paste0("Surv(time, status) ~ ", strata)), data = survData)
  
  hazardRatios <- data.frame(summary(coxFit)$coefficients) %>%
    rownames_to_column() %>%
    rename(Strata = rowname,
           Hazard_Ratio = `exp.coef.`,
           P_Value = `Pr...z..`
    ) %>%
    mutate(Strata = gsub(strata, '', Strata)) %>%
    select(Strata, Hazard_Ratio, P_Value)
  
  # If the rmstMaxTime is set to -1, change it to the default which is the max timeDiff in the data:
  if(rmstMaxTime == -1){
    rmstMaxTime = max(survData$time)
  }
  
  rmsts <- data.frame(survival:::survmean(survFit, rmean=rmstMaxTime)$matrix) %>%
    rownames_to_column() %>%
    rename(Strata = rowname) %>%
    mutate(totalCount = sum(records),
           Percentage = records*100 / totalCount) %>%
    select(Strata, Percentage, RMST = rmean) %>%
    mutate(Strata = gsub(paste0(strata, "="), '', Strata)) %>%
    left_join(hazardRatios, by = "Strata") %>%
    mutate(Hazard_Ratio = case_when(is.na(Hazard_Ratio) ~ "Reference",
                                       TRUE ~ as.character(Hazard_Ratio)),
           P_Value = case_when(is.na(P_Value) ~ "Reference",
                               TRUE ~ as.character(P_Value))) %>%
    rename(!!strata := Strata)
  
  rmsts
}
