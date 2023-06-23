# AaveV3 Optimism Data:
library(ghql)
library(jsonlite)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(urltools)
library(R.utils)
library(data.table)

apiURL = 'https://api.thegraph.com/subgraphs/name/aave/protocol-v3-optimism'

con <- GraphqlClient$new(url = apiURL)

dataPath = "~/data/IDEA_DeFi_Research/Data/Lending_Protocols/Aave/V3/Optimism/Raw/"

getMaxTimestamp <- function(fileName){
  tryCatch({
    max(fread(paste0(dataPath, fileName), select = "timestamp")$timestamp)
  },
  error = function(cond) {
    message("Previous data not found.")
  })
}

filePath = "~/DeFi_Data_Collection/Lending_Protocols/Aave/V3/Optimism"

# Tables not based in time:
source(paste0(filePath, "/getRawTables/getReserves.R"))
source(paste0(filePath, "/getRawTables/getUsers.R"))

# Transaction Tables:
source(paste0(filePath, "/getRawTables/getBorrows.R"))
source(paste0(filePath, "/getRawTables/getDeposits.R"))
source(paste0(filePath, "/getRawTables/getCollaterals.R"))
source(paste0(filePath, "/getRawTables/getRedeems.R"))
source(paste0(filePath, "/getRawTables/getRepays.R"))
source(paste0(filePath, "/getRawTables/getLiquidations.R"))
source(paste0(filePath, "/getRawTables/getSwaps.R"))
source(paste0(filePath, "/getRawTables/getFlashLoans.R"))


source(paste0(filePath, "/getRawTables/getReserveParamsHistory.R"))
source(paste0(filePath, "/getRawTables/getUserReserves.R"))
source(paste0(filePath, "/getRawTables/getPriceHistoryItems.R"))

