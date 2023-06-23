## First, let's check for the existence of borrowsAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawReserves.csv"
newFile = TRUE

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { reserves(first:1000){
                           id,
                           underlyingAsset,
                           pool{id},
                           symbol,
                           name,
                           decimals,
                           usageAsCollateralEnabled,
                           borrowingEnabled,
                           stableBorrowRateEnabled,
                           isActive,
                           isFrozen,
                           price,
                           reserveInterestRateStrategy,
                           optimalUtilisationRate,
                           variableRateSlope1,
                           variableRateSlope2,
                           stableRateSlope1,
                           stableRateSlope2,
                           baseVariableBorrowRate,
                           baseLTVasCollateral,
                           reserveLiquidationThreshold,
                           reserveLiquidationBonus,
                           utilizationRate,
                           totalLiquidity,
                           totalATokenSupply,
                           totalLiquidityAsCollateral,
                           totalPrincipalStableDebt,
                           totalScaledVariableDebt,
                           totalCurrentVariableDebt,
                           totalDeposits,
                           liquidityRate,
                           averageStableRate,
                           variableBorrowRate,
                           stableBorrowRate,
                           liquidityIndex,
                           variableBorrowIndex,
                           aToken{id},
                           vToken{id},
                           sToken{id},
                           reserveFactor,
                           lastUpdateTimestamp,
                           stableDebtLastUpdateTimestamp,
                           aEmissionPerSecond,
                           vEmissionPerSecond,
                           sEmissionPerSecond,
                           aTokenIncentivesIndex,
                           vTokenIncentivesIndex,
                           sTokenIncentivesIndex,
                           aIncentivesLastUpdateTimestamp,
                           vIncentivesLastUpdateTimestamp,
                           sIncentivesLastUpdateTimestamp,
                           lifetimeLiquidity,
                           lifetimePrincipalStableDebt,
                           lifetimeScaledVariableDebt,
                           lifetimeCurrentVariableDebt,
                           lifetimeRepayments,
                           lifetimeWithdrawals,
                           lifetimeBorrows,
                           lifetimeLiquidated,
                           lifetimeFlashLoans,
                           lifetimeFlashLoanPremium,
                           lifetimeDepositorsInterestEarned,
                           lifetimeReserveFactorAccrued
                           }}',sep = ''))
  
  qry$query("reserves", query)
  
  response <- con$exec(qry$queries$reserves) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$reserves) == 0){
    break
  }
  responseData <- response$data$reserves %>%
    mutate(poolID = as.character(pool$id),
           aTokenID = as.character(aToken$id),
           vTokenID = as.character(vToken$id),
           sTokenID = as.character(sToken$id)) %>%
    dplyr::select(-pool, -aToken, -vToken, -sToken)
  
  responseData[responseData == "NULL"] <- NA
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}