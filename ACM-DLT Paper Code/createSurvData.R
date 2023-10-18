
createSurvData <- function(indexEventSet, outcomeEventSet, data, joinFeatures, observationPeriod = c(0, -1), indexCovariates = c(), outcomeCovariates = c()){
  survivalData <- NULL
  
  timeStart = as.double(max(observationPeriod[1], min(data$timestamp)))
  
  if(observationPeriod[2] == -1){
    # For right-censored events, the final transaction in the data frame will be the end observation:
    timeFinal = max(data$timestamp)
  }else{
    timeFinal = as.double(observationPeriod[2])
  }
  
  # Collect the index events and select the relevant features:
  indexEvents <- data %>%
    filter(type %in% indexEventSet,
           between(timestamp, timeStart, timeFinal)) %>%
    select(where(not_all_na)) %>%
    mutate(ID = row_number(),
           indexTime = timestamp) %>%
    select(ID, indexTime, joinFeatures, indexCovariates)
  
  # Collect the outcome events and select the relevant features:
  outcomeEvents <- data %>%
    filter(type %in% outcomeEventSet,
           between(timestamp, timeStart, timeFinal)) %>%
    select(where(not_all_na)) %>%
    mutate(outcomeTime = timestamp) %>%
    select(outcomeTime, joinFeatures, outcomeCovariates)
    
  uncensoredEvents <- left_join(indexEvents, outcomeEvents, by = joinFeatures) %>% 
    filter(outcomeTime > indexTime) %>%
    group_by(ID) %>% 
    slice_min(outcomeTime) %>%
    distinct() %>%
    summarize(ID,
              timeDiff = outcomeTime - indexTime,
              status = 1,
              across(joinFeatures),
              across(names(indexCovariates)),
              across(names(outcomeCovariates)))
  
  # For right-censored events, we summarise using features from the index event and calculate the 
  # time-to-censorship using the timeFinal from above and the index time:
  rightCensoredEvents <- anti_join(indexEvents, uncensoredEvents, by = c("ID")) %>%
    summarise(ID,
              timeDiff = timeFinal - indexTime,
              status = 0,
              across(joinFeatures),
              across(names(indexCovariates))) %>%
    distinct()
  
  leftCensoredEvents <- anti_join(outcomeEvents, indexEvents, by = joinFeatures) %>%
    summarise(timeDiff = as.double(outcomeTime - timeStart), 
              status = 0,
              across(joinFeatures),
              across(names(outcomeCovariates)))
  
  survivalData <- bind_rows(survivalData, rightCensoredEvents, uncensoredEvents, leftCensoredEvents)
  
  survivalData
}
