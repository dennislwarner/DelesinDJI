f.make_MaxRet<-function(portname,l.PortData,rebalFreq="qurters"){
    # Build a maximum RoR  portfolio
    #names(l.PortData)
    #"df.Eras" ,"df.R.xts","v.dates", "stocks","v.etf.xts","v.etfRor.xts","fSimDate","fgraphDate"  
    portf_maxret <- portfolio.spec(assets=l.PortData$stocks);
    portf_maxret <- add.constraint(portfolio=portf_maxret, type="full_investment")
    portf_maxret <- add.constraint(portfolio=portf_maxret, type="box",
                                   min=rep(0,l.PortData$numvars),
                                   max=rep(0.3,l.PortData$numvars));
    portf_maxret <- add.objective(portfolio=portf_maxret, type="return", name="mean");
    opt_maxret <- optimize.portfolio(R=returns, portfolio=portf_maxret, 
                                     optimize_method="ROI", trace=TRUE)
    # plot(opt_maxret, chart.assets=TRUE, xlim=c(0.02, 0.18));
    # chart.RiskReward(opt_maxret,return.col="mean", risk.col="sd", 
    #              chart.assets=TRUE, xlim=c(0.01, 0.18), main="Maximum Return")
    bt_maxret <- optimize.portfolio.rebalancing(R=returns, portfolio=portf_maxret,
                                                optimize_method="ROI", 
                                                rebalance_on=rebalFreq, 
                                                training_period=36);
    return(bt_maxret$opt_rebalancing)
    
}
f.make_MaxRandomRet<-function(portname,l.PortData,rebalFreq="quarters"){
    # Build a maximum RoR  portfolio
    #names(l.PortData)
    #"df.Eras" ,"df.R.xts","v.dates", "stocks","v.etf.xts","v.etfRor.xts","fSimDate","fgraphDate"  
    portf_maxret <- portfolio.spec(assets=l.PortData$stocks);
   # portf_maxret <- add.constraint(portfolio=portf_maxret, type="full_investment")
    port_maxret<-add.constraint(portfolio=portf_maxret,type="weight_sum",min_sum=0.99,max_sum=1.01)
    portf_maxret <- add.constraint(portfolio=portf_maxret, type="box",
                                   min=rep(0,l.PortData$numvars),
                                   max=rep(0.3,l.PortData$numvars));
    portf_maxret <- add.objective(portfolio=portf_maxret, type="return", name="mean");
    opt_maxret <- optimize.portfolio(R=returns, portfolio=portf_maxret, 
                                     optimize_method="random", trace=TRUE)
    # plot(opt_maxret, chart.assets=TRUE, xlim=c(0.02, 0.18));
    # chart.RiskReward(opt_maxret,return.col="mean", risk.col="sd", 
    #              chart.assets=TRUE, xlim=c(0.01, 0.18), main="Maximum Return")
    bt_maxret <- optimize.portfolio.rebalancing(R=returns, portfolio=portf_maxret,
                                                optimize_method="ROI", 
                                                rebalance_on=rebalFreq);
    return(bt_maxret$opt_rebalancing)
    
}
f.make_Opt<-function(portname,l.PortData,rebalFreq="quarters",objType="risk",objName="var"){
    # Build a maximum RoR  portfolio
    #names(l.PortData)
    #"df.Eras" ,"df.R.xts","v.dates", "stocks","v.etf.xts","v.etfRor.xts","fSimDate","fgraphDate"  

    returns<-l.PortData$df.R.xts
    returns[is.na(returns)]<-0;
    sreturns<-returns[,-ncol(returns)]
    stocks<-l.PortData$stocks;
    
    portf_m <- portfolio.spec(assets=l.PortData$stocks);
   portf_m <- add.constraint(portfolio=portf_m, type="full_investment")
    
    # portf_m<-add.constraint(portfolio=portf_m,
    #                         type="weight_sum",
    #                         min_sum=0.99,
    #                         max_sum=1.01)
    portf_m <- add.constraint(portfolio=portf_m, 
                              type="box",
                              min=rep(0,l.PortData$numvars),
                              max=rep(0.2,l.PortData$numvars));
    #portf_m <- add.constraint(portfolio=portf_m, type="return", return_target=mean(colMeans(returns)))
    portf_m <- add.objective(portfolio=portf_m, 
                             type='quadratic_utility', 
                            risk_aversion=0.750);
    
   #  opt_pROI <- optimize.portfolio(R=returns, portfolio=portf_m, 
   #                                   optimize_method="ROI", trace=TRUE)
   #  print(opt_pROI)
   #  sop_pROI<-summary(opt_pROI)
   #  print(sop_pROI)
   #  extractStats(opt_pROI);
   #  chart.RiskReward(opt_pROI);
   #  chart.Weights(opt_pROI)
   # ivw<- inverse.volatility.weight(sreturns,portf_m)
    # plot(opt_maxret, chart.assets=TRUE, xlim=c(0.02, 0.18));
    # chart.RiskReward(opt_maxret,return.col="mean", risk.col="sd", 
    #              chart.assets=TRUE, xlim=c(0.01, 0.18), main="Maximum Return")
    bt_maxt <- optimize.portfolio.rebalancing(R=returns,
                                                portfolio=portf_m,
                                                optimize_method="ROI",
                                                rebalance_on=rebalFreq,
                                                trace=TRUE);
    return(bt_maxt$opt_rebalancing)
    
}