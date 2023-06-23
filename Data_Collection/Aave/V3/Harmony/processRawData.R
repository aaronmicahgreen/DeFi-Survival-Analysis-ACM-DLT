library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
aavePath = "~/data/IDEA_DeFi_Research/Data/Lending_Protocols/Aave/"
filePath = "~/DeFi_Data_Collection/Lending_Protocols/Aave/V3/Harmony"
dataPath = "~/data/IDEA_DeFi_Research/Data/Lending_Protocols/Aave/V3/Harmony/"
rawDataPath = paste(dataPath, "Raw/", sep="")

## Helper functions
not_all_na <- function(x) any(!is.na(x))
`%notin%` <- Negate(`%in%`)

# First let's process the users and get them some aliases:
source(paste0(filePath, "/processTables/processUsers.R"))
source(paste0(filePath, "/processTables/processTransactions.R"))


