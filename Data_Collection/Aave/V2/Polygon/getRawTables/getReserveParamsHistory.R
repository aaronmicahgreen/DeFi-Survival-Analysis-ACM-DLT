## First, let's check for the existence of borrowsAave.csv and see if we need to get all borrows or just the more recent borrows:
fileName <- "rawReserveParamsHistory.csv"
lastTimestamp = getMaxTimestamp(fileName)
newFile = FALSE

if(is.null(lastTimestamp)){
  lastTimestamp = 0
  newFile = TRUE
}

repeat{
  qry <- Query$new()
  query = str_squish(str_c('query { reserveParamsHistoryItems(first:1000 orderBy: timestamp where: {timestamp_gt:',lastTimestamp,'}){
                           id,
                           reserve{id},
                           variableBorrowRate,
                           variableBorrowIndex,
                           utilizationRate,
                           stableBorrowRate,
                           averageStableBorrowRate,
                           liquidityIndex,
                           liquidityRate,
                           totalLiquidity,
                           totalATokenSupply,
                           totalLiquidityAsCollateral,
                           availableLiquidity,
                           priceInEth,
                           priceInUsd,
                           timestamp,
                           totalScaledVariableDebt,
                           totalCurrentVariableDebt,
                           totalPrincipalStableDebt,
                           lifetimePrincipalStableDebt,
                           lifetimeScaledVariableDebt,
                           lifetimeCurrentVariableDebt,
                           lifetimeLiquidity,
                           lifetimeRepayments,
                           lifetimeWithdrawals,
                           lifetimeBorrows,
                           lifetimeLiquidated,
                           lifetimeFlashLoans,
                           lifetimeFlashLoanPremium,
                           lifetimeReserveFactorAccrued,
                           lifetimeDepositorsInterestEarned
                           }}',sep = ''))
  
  qry$query("reserveParams", query)
  
  response <- con$exec(qry$queries$reserveParams) %>% fromJSON()
  
  if(!is.null(response$errors)){
    next
  }
  if(length(response$data$reserveParamsHistoryItems) == 0){
    break
  }
  responseData <- response$data$reserveParamsHistoryItems %>%
    mutate(reserveID = as.character(reserve$id)) %>%
<<<<<<< HEAD
    dplyr::select(-reserve)
=======
    select(-reserve)
>>>>>>> 46db68c5bd8a0f571a4ebdf5b28bf1adad7a986f
  
  responseData[responseData == "NULL"] <- NA
  
  lastTimestamp = max(responseData$timestamp)
  
  write_csv(responseData, paste0(dataPath, fileName), append = !newFile)
  
  newFile = FALSE
  
  if(length(responseData$id) < 1000){
    break
  } 
}