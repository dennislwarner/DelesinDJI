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
f.make_MaxRandomRet<-function(portname,l.PortData,rebalFreq="qurters"){
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
                                                rebalance_on=rebalFreq, 
                                                training_period=36);
    return(bt_maxret$opt_rebalancing)
    
}