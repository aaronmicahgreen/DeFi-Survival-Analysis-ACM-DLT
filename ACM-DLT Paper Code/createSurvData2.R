createSurvData2 <- function(indexEvents, outcomeEvents, joinFeatures){
  survivalData <- NULL
  
  # For right-censored events, the final transaction in the data frame will be the end observation:
  timeFinal = max(max(indexEvents$indexTime), max(outcomeEvents$outcomeTime))
  
  
  # Create all possible combinations of index and outcome events that match 
  # the user and reserve. From this we will narrow down the combinations to only 
  # those which are meaningful, i.e. combinations where the outcome events occur 
  # after the index event:
  indexToOutcomes <- left_join(indexEvents, outcomeEvents, by = joinFeatures) %>% 
    group_by(ID) %>% 
    mutate(status = case_when(indexTime >= max(outcomeTime) ~ 0,
                              is.na(outcomeTime) ~ 0,
                              TRUE ~ 1)) %>%
    distinct()
  
  # For right-censored events, we summarise using features from the index event and calculate the 
  # time-to-censorship using the timeFinal from above and the index time:
  rightCensoredEvents <- indexToOutcomes %>%
    filter(status == 0) %>%
    group_by(ID) %>%
    summarise(user, 
              timeDiff = as.double(timeFinal - indexTime), 
              status, 
              reserve, 
              coinType, 
              datetime, 
              amountUSDQuartile, 
              marketTrend) %>%
    distinct()
  
  # For uncensored events, we summarise using features from the index event when necessary and 
  # calculate the time-to-outcome as the outcome event time minus the index event time:
  uncensoredEvents <- indexToOutcomes %>%
    filter(status == 1) %>%
    filter(outcomeTime > indexTime) %>%
    summarise(user, 
              timeDiff = min(outcomeTime) - indexTime, 
              status, 
              reserve, 
              coinType, 
              datetime, 
              amountUSDQuartile, 
              marketTrend) %>%
    distinct()
  
  survivalData <- bind_rows(survivalData, rightCensoredEvents, uncensoredEvents) %>%
    mutate(quarter = paste0(year(datetime), " Q", quarter(datetime))) %>%
    select(-datetime) %>%
    rename(Reserve = reserve, 
           Reserve_Type = coinType,
           USD_Amount_Quartile = amountUSDQuartile,
           Market_Trend = marketTrend,
           Quarter = quarter)
  
  survivalData
}
