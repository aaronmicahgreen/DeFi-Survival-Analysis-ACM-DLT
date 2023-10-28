createSurvPlotCR = function(data, strata, plotTitle, xLab, yLab, strataLegendLabs, outcomeLegendLabs, legendPos, xRange =  c(0, -1)){
  if(xRange[2] == -1){
    xRange[2] <- max(data$time)
  }
  fit <-  tidycmprsk::cuminc(as.formula(paste0("Surv(time, endpoint) ~ ", strata)), data = data)
  outcomes <- levels(fit$data$endpoint)
  
  fit %>% 
    ggcuminc(outcome = outcomes[!outcomes == 'censor']) + 
    labs(x = xLab, y = yLab) + 
    add_confidence_interval() + 
    ggtitle(label = plotTitle) + 
    scale_x_continuous(breaks = seq(0, 650, by = 100)) + 
    theme(legend.position = legendPos) + 
    scale_fill_hue(name = str_to_title(strata), labels = strataLegendLabs) +
    scale_linetype(name = 'Outcome', labels = outcomeLegendLabs) +
    guides(color = "none")
}
