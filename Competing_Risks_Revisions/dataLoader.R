library(lubridate)
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(tidycmprsk)
library(tidyr)
library(ggsurvfit)
library(cmprsk)
library(readr)
library(data.table)
library(ggpubr)
library(survRM2)
# Standardize values for publication font sizes and color schemes.
pubFontSize = 18
pValSize = 8
pubWidth = 8
pubHeight = 6
pubColorScheme = "Set1"
# Load the AaveV2 Ethereum transaction data and filter out transactions after October 1, 2022 since 
# this is the data we used in the original paper

transactions <- read_rds("/data/IDEA_DeFi_Research/Data/Lending_Protocols/Aave/V2/Mainnet/transactions.rds")
coinTypes <- read_csv("/data/IDEA_DeFi_Research/Data/Coin_Info/stablecoins.csv")
# Run the files to make the functions that create the survival data:
source(".././Survival_Data_Creation/createSurvData.R")
source("./createCRDataTable.R")
source("./createSurvPlotCR.R")

tFinal <- as_datetime(ymd("2022-10-01"))

transactions <- transactions %>%
  filter(timestamp <= tFinal) %>%
  mutate(coinType = case_when(reserve %in% coinTypes$symbol ~ "Stable",
                              TRUE ~ "Non-Stable"))

## Helper functions
not_all_na <- function(x) any(!is.na(x))
`%notin%` <- Negate(`%in%`)
select <- dplyr::select
mutate <- dplyr::mutate
rename <- dplyr::rename