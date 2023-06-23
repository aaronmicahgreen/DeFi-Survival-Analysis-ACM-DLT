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
                           availableLiquidity,
                           totalPrincipalStableDebt,
                           totalScaledVariableDebt,
                           totalCurrentVariableDebt,
                           totalSupplies,
                           liquidityRate,
                           accruedToTreasury,
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
                           isPaused,
                           isDropped,
                           borrowCap,
                           supplyCap,
                           debtCeiling,
                           unbackedMintCap,
                           liquidationProtocolFee,
                           borrowableInIsolation,
                           eMode{id},
                           siloedBorrowing,
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
                           lifetimeFlashLoanLPPremium,
                           lifetimeFlashLoanProtocolPremium,
                           lifetimePortalLPFee,
                           lifetimePortalProtocolFee,
                           lifetimeSuppliersInterestEarned,
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
           sTokenID = as.character(sToken$id),
           eModeID = as.character(eMode$id)) %>%
    select(-pool, -aToken, -vToken, -sToken, -eMode)
  
  responseData[responseData == "NULL"] <- NA
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}