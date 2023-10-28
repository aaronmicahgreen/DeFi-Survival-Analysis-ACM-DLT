createCRDataTable <- function(survData, strata) {
  require(tidycmprsk)
  survData[[strata]] <- as.factor(survData[[strata]])
  
  # First, we get the relevant information regarding the CSHRs using the multi-state cox model:
  coxFit <- coxph(as.formula(paste0("Surv(time, endpoint) ~ ", strata)), data = survData, id = id)
  covariateLevels <- coxFit$xlevels[[1]]
  outcomes <- levels(survData$endpoint)
  
  fullTable <- data.frame(covariateLevels) %>%
    rename(Strata = covariateLevels) 
  for(i in range(length(outcomes))){
    fullTable <- fullTable %>%
      bind_rows(fullTable)
  }
  fullTable <- fullTable %>%
    group_by(Strata) %>%
    mutate(Outcome = row_number()) %>%
    ungroup() %>%
    filter(Outcome <= length(outcomes) & Outcome > 1) %>%
    mutate(Outcome = str_to_title(outcomes[Outcome]))
  
  cshrs <- data.frame(summary(coxFit)$coefficients) %>%
    rownames_to_column() %>%
    rename(Strata = rowname,
           CSHR = `exp.coef.`,
           P_Value = `Pr...z..`
    ) %>%
    mutate(Strata = gsub(strata, '', Strata)) %>% 
    mutate(Outcome = str_to_title(outcomes[as.numeric(str_sub(Strata, -1))])) %>%
    mutate(Strata = str_sub(Strata, end = str_locate(Strata, "_")[1]-1)) %>%
    select(Strata, Outcome, CSHR, CSHR_P_Value = P_Value)
  
    fullTable <- fullTable %>%
      left_join(cshrs, by = c("Strata", "Outcome"))
    
    # Calculate the SDHRs for each outcome type
    fgTables <- NULL
    
    for(outcome in outcomes){
      if(outcome == "censor"){next}
      fgFit <-  tidycmprsk::crr(as.formula(paste0("Surv(time, endpoint) ~ ", strata)), data = survData, failcode = outcome)
      fgTable <- data.frame(fgFit$tidy) %>%
        rename(Strata=term) %>%
        mutate(Strata = str_sub(Strata, start = str_length(strata)+1)) %>%
        mutate(Outcome = str_to_title(outcome)) %>%
        mutate(SDHR = exp(estimate)) %>%
        select(Strata, Outcome, SDHR, SDHR_P_Value = p.value)
      
      fgTables <- bind_rows(fgTables, fgTable)
    }
    
    fullTable <- fullTable %>%
      left_join(fgTables, by = c("Strata", "Outcome"))
  
  
  
  # Calculate the Strata Percentage:
  survFit <- surv_fit(as.formula(paste0("Surv(time, endpoint) ~ ", strata)), data = survData)  
  percentageStrata <- data.frame(survival:::survmean(survFit, rmean=max(survData$time))$matrix) %>%
    rownames_to_column() %>%
    rename(Strata = rowname) %>%
    mutate(totalCount = sum(records),
           Percentage_Strata = records*100 / totalCount) %>%
    select(Strata, Percentage_Strata) %>%
    mutate(Strata = gsub(paste0(strata, "="), '', Strata)) 
  
  fullTable <- fullTable %>%
    left_join(percentageStrata, by = "Strata")
  
  # Calculate the percentage outcomes for each outcome type
  percentageOutcome <- survData %>%
    group_by(endpoint) %>%
    summarize(endpointTotal = n()) %>%
    ungroup() %>%
    mutate(eventTotal = sum(endpointTotal)) %>%
    mutate(Percentage_Outcome = endpointTotal * 100 / eventTotal) %>%
    rename(Outcome = endpoint) %>%
    mutate(Outcome = str_to_title(Outcome)) %>%
    select(Outcome, Percentage_Outcome)
  
  fullTable <- fullTable %>%
    left_join(percentageOutcome, by = "Outcome")
  
  # Tidy up P-values for presentation:
  fullTable <- fullTable %>%
    mutate(CSHR_P_Value  = case_when(is.na(CSHR_P_Value) ~ "Reference",
                                     TRUE ~ as.character(CSHR_P_Value))) %>%
    mutate(SDHR_P_Value = case_when(is.na(SDHR_P_Value) ~ "Reference",
                                    TRUE ~ as.character(SDHR_P_Value))) %>%
    mutate(CSHR = case_when(is.na(CSHR) ~ "Reference",
                            TRUE ~ as.character(round(CSHR, 2)))) %>%
    mutate(SDHR = case_when(is.na(SDHR) ~ "Reference",
                            TRUE ~ as.character(round(SDHR, 2)))) %>%
    mutate(Percentage = (Percentage_Strata * Percentage_Outcome/100)) 
  
  fullTable
  
}
