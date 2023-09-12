library(ggplot2)
library(readr)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(ggpubr)
library(data.table)


# Set the name of the protocol, the version, and the market that you want survival data created for:
protocolName = "Compound"
version = "V2"
market = "Ethereum"


# Load the survival data function:
source("~/DeFi_Data_Collection_And_Processing/Survival_Data_Creation/createSurvData.R")

# These are the relevant file paths to the data that will need to be loaded and where the data will be stored.
appDataPath = paste0("~/data/IDEA_DeFi_Research/App_Data/Survival_Data/Lending_Protocols/", protocolName, "/", version, "/", market, "/")
filePath = paste0("~/DAR_F22/DAR_F22/Data Collection and Processing/Lending_Protocols/", protocolName, "/", version, "/", market, "/")
dataPath = paste0("~/data/IDEA_DeFi_Research/Data/Lending_Protocols/", protocolName, "/", version, "/", market, "/")

## Helper functions
not_all_na <- function(x) any(!is.na(x))
`%notin%` <- Negate(`%in%`)
stableCoins <- read_csv("~/data/IDEA_DeFi_Research/Data/Coin_Info/stablecoins.csv")

# Load transactions file:
transactions <- read_rds(paste0(dataPath, "transactions.rds")) %>%
  mutate(datetime = as_datetime(timestamp),
         quarter = floor_date(datetime, unit = "quarter")) %>%
  mutate(quarter = paste0(year(datetime), " Q", quarter(datetime))) %>%
  group_by(type) %>%
  mutate(amountUSDQuartile = ntile(amountUSD, n = 4)) %>%
  group_by(reserve) %>%
  mutate(amountNativeQuartile = ntile(amountNative, n = 4)) %>%
  ungroup() %>%
  mutate(coinType = case_when(reserve %in% stableCoins$symbol ~ "Stable",
                              TRUE ~ "Non-Stable"))

# These will be the default settings for subjects and the covariates unless otherwise specified:
subjects <- c("user", "reserve")
indexCovariates <- c("market", "quarter", "coinType", "amountUSDQuartile", "amountNativeQuartile")
outcomeCovariates <- c()

#####
# Create survival data for basic transaction types:
#####
# We are filtering out liquidations, as those events are slightly more complicated and need to be handled separately from the basic transaction types:
basicTransactions <- transactions %>%
  filter(type != "liquidation",
         type != "collateral")

basicEventTypes <- basicTransactions %>%
  select(type) %>%
  distinct()

for(indexEvent in basicEventTypes$type){
  # Each index event should have its own directory. We create the appropriate directory
  # here, in case it doesn't already exist:
  
  dir.create(paste0(appDataPath, str_to_title(indexEvent), "/"))
  
  for(outcomeEvent in basicEventTypes$type){

    if(indexEvent == outcomeEvent){
      next # We skip the case when the index and outcome events are the same because this causes issues with the rolling join used to create the survival data
    }
    
    # Each outcome event should have its own directory within the index event's folder:
    dir.create(paste0(appDataPath, str_to_title(indexEvent), "/", str_to_title(outcomeEvent), "/"))
    setwd(paste0(appDataPath, str_to_title(indexEvent), "/", str_to_title(outcomeEvent), "/"))
    
    survData <- createSurvData(indexEventSet = c(indexEvent), 
                               outcomeEventSet = c(outcomeEvent), 
                               basicTransactions, 
                               subjects = c("user", "reserve"),
                               indexCovariates = c("market", "quarter", "coinType", "amountUSDQuartile", "amountNativeQuartile"),
                               outcomeCovariates = c())
    
    
    
      
    saveRDS(survData, paste0("survivalData.rds"))
    
  }
}

#####
# Next we want to compute borrow-to-full-repay survival data:
#####
setwd(paste0(appDataPath, "Borrow/"))
dir.create(paste0(appDataPath, "Borrow/", "Full Repay/"))
setwd(paste0(appDataPath, "Borrow/Full Repay/"))



cumulativeBorrows <- transactions %>%
  filter(type == "borrow") %>%
  group_by(user, reserve) %>%
  arrange(timestamp) %>%
  mutate(totalBorrowedNative = cumsum(amountNative), totalBorrowedUSD = cumsum(amountUSD)) %>%
  ungroup()

cumulativeRepays <- transactions %>%
  filter(type == "repay") %>%
  group_by(user, reserve) %>%
  arrange(timestamp) %>%
  mutate(totalRepaidNative = cumsum(amountNative), totalRepaidUSD = cumsum(amountUSD)) %>%
  ungroup()

survivalData <- NULL

timeStart = min(transactions$timestamp)
timeFinal = max(transactions$timestamp)


indexEvents <- cumulativeBorrows %>%
  filter(between(timestamp, timeStart, timeFinal)) %>%
  select(where(not_all_na)) %>%
  mutate(indexID = row_number(),
         indexTime = timestamp) %>%
  select(indexID, timestamp, indexTime, any_of(subjects), totalBorrowedNative, totalBorrowedUSD, any_of(indexCovariates)) %>%
  data.table()

outcomeEvents <- cumulativeRepays %>%
  filter(between(timestamp, timeStart, timeFinal)) %>%
  select(where(not_all_na)) %>%
  mutate(outcomeID = row_number(),
         outcomeTime = timestamp) %>%
  select(outcomeID, timestamp, outcomeTime, any_of(subjects), totalRepaidNative, totalRepaidUSD, any_of(outcomeCovariates)) %>%
  data.table()

uncensoredEvents <- outcomeEvents[indexEvents, on = c(subjects, "timestamp"), roll = -Inf] %>%
  filter(!is.na(outcomeID),
         totalRepaidNative >= totalBorrowedNative) %>%
  mutate(timeDiff = outcomeTime - indexTime,
         status = 1) %>%
  select(indexID,
         indexTime,
         outcomeTime,
         timeDiff,
         status,
         any_of(subjects),
         any_of(indexCovariates),
         any_of(outcomeCovariates))

rightCensoredEvents <- anti_join(indexEvents, uncensoredEvents, by = c("indexID")) %>%
  mutate(status = 0,
         timeDiff = timeFinal - indexTime) %>%
  select(indexID,
            indexTime,
            timeDiff,
            status,
            any_of(subjects),
            any_of(indexCovariates)) %>%
  distinct()

  survivalData <- bind_rows(uncensoredEvents, rightCensoredEvents)
  
  saveRDS(survivalData, paste0("survivalData.rds"))

source("~/DeFi_Data_Collection_And_Processing/Survival_Data_Creation/createSurvDataWithLiquidations.R")


