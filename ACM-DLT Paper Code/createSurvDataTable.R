createSurvDataTable <- function(survData, strata, rmstMaxTime = -1) {
  survData[[strata]] <- as.factor(survData[[strata]])
  survFit <- surv_fit(as.formula(paste0("Surv(timeDiff/86400, status) ~ ", strata)), data = survivalData)
  coxFit <- coxph(as.formula(paste0("Surv(timeDiff/86400, status) ~ ", strata)), data = survivalData)
  
  hazardRatios <- data.frame(summary(coxFit)$coefficients) %>%
    rownames_to_column() %>%
    rename(Strata = rowname,
           Cox_Coefficient = coef,
           P_Value = `Pr...z..`
    ) %>%
    mutate(Strata = gsub(strata, '', Strata)) %>%
    select(Strata, Cox_Coefficient, P_Value)
  
  # If the rmstMaxTime is set to -1, change it to the default which is the max timeDiff in the data:
  if(rmstMaxTime == -1){
    rmstMaxTime = max(survivalData$timeDiff/86400)
  }
  
  rmsts <- data.frame(survival:::survmean(survFit, rmean=rmstMaxTime)$matrix) %>%
    rownames_to_column() %>%
    rename(Strata = rowname) %>%
    mutate(totalCount = sum(records),
           Percentage = records*100 / totalCount) %>%
    select(Strata, Percentage, RMST = rmean) %>%
    mutate(Strata = gsub(paste0(strata, "="), '', Strata)) %>%
    left_join(hazardRatios, by = "Strata") %>%
    mutate(Cox_Coefficient = case_when(is.na(Cox_Coefficient) ~ "Reference",
                                       TRUE ~ as.character(Cox_Coefficient)),
           P_Value = case_when(is.na(P_Value) ~ "Reference",
                               TRUE ~ as.character(P_Value))) %>%
    rename(!!strata := Strata)
  
  rmsts
}
