createSurvPlot = function(data, strata, plotTitle, xLab, yLab, legendTitle, legendLabs, legendPos, xRange =  c(0, -1)){
  if(xRange[2] == -1){
    xRange[2] <- max(data$timeDiff/86400)
  }
  fit <- surv_fit(as.formula(paste0("Surv(timeDiff/86400, status) ~ ", strata)), data)
  ggsurvplot(fit,
             censor = FALSE,
             title = plotTitle,
             xlab = xLab,
             break.x.by = 50,
             ylab = yLab,
             xlim = xRange,
             legend.title = legendTitle,
             conf.int = TRUE,
             legend.labs = legendLabs,
             legend = legendPos,
             subtitle = paste0("p-value (log-rank test): ", case_when(surv_pvalue(fit, data)$pval == 0 ~ "< 1e-16",
                                                                      TRUE ~ as.character(surv_pvalue(fit, data)$pval)))
             )
}
