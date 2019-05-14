portreturns<-function(x){
    portreturns<-m.x%*%(x);
    return(portreturns);
}
sharpe<-function(x){
    port.returns<-portreturns(x);
    sha<-mean(port.returns)/sqrt(var(port.returns))
    return(sha)
}
constraint<-function(x){
    boundary_constr<-(sum(x)-1)**2;
    for(i in 1:length(x)){
        boundary_constr=boundary_constr+
            max(c(0,x[i]-1))**2+
            max(c(0,-x[i]))**2
    }
    return(boundary_constr);
}
obj<-function(x){
    return(-sharpe(x)+100*constraint(x))
}
##################################################
f.prepPortData<-function(df.x.xts,freq,`trainyears`,testyears){
  
 
    #Transform to desired frequency----
    obsperyear<-switch(freq,
           "days"=261,
           "weeks"=52,
           "months"=12,
           "quarters"=4,
           0 );
    trainperiods<-obsperyear*trainyears;
    testperiods<-obsperyear*testyears;
    v.eop      <- endpoints(df.x.xts,on=freq)
    df.z.xts   <- df.x.xts[v.eop,];
    df.z       <- f.xts2df(df.z.xts)
    v.dates    <- df.z$Date;
    v.etf.xts  <- df.z.xts[,ncol(df.z.xts)]
    #----
    #Truncate beginning until reach useful target data----
    v.etf         <- df.z[,ncol(df.z)];
    v.nonna       <- !is.na(v.etf);
    nobs<-length(v.dates)
    iera<-0;
    firstSim   <- which(!is.na(v.etf))[1];
    leraS<-list();
    lTsto<-0;
    while(lTsto<nobs){
        iera<-iera+1;
        fTsto<-trainperiods+firstSim+(iera-1)*testperiods;
        lTsto<-min(fTsto+testperiods-1,nobs)
        lTrno<-fTsto-1;
        fTrno<-fTsto-trainperiods;
        
        
        fTstd<-as.character(v.dates[fTsto]);
        lTstd<-as.character(v.dates[lTsto]);
        fTrnd<-as.character(v.dates[fTrno]);
        lTrnd<-as.character(v.dates[lTrno]);
        TrnSpan<-paste(fTrnd,lTrnd,sep="/");
        TstSpan<-paste(fTstd,lTstd,sep="/");
        cat(iera,fTrno,lTrno,"TrnSpan=",TrnSpan,fTsto,lTsto,"TestSpan=",TstSpan,"\n");
        lera<-list(fTrno=fTrno,lTrno=lTrno,fTsto=fTsto,lTsto=lTsto,TrnSpan=TrnSpan,TstSpan=TstSpan)
        leraS[[iera]]<-lera;
    }
    numeras<-iera;
    df.Eras<-data.frame(matrix(0,numeras,6))
    names(df.Eras)<-c("fTrno","lTrno","fTsto","lTsto","TrnSpan","TstSpan")
    for(i in c(1:numeras)){
        df.Eras[i,]<-leraS[[i]];
    }
    r.xts<-Return.calculate(df.z.xts,method='log')
    rcum.xts<-Return.cumulative(r.xts)
    rexcess.xts<-Return.excess(r.xts,Rf=0.02)
    v.etfRor.xts<-r.xts[,ncol(r.xts)];
    df.R.xts<-r.xts[,-ncol(r.xts)];
    stocks<-names(df.R.xts)
    df.R.xts[is.na(df.R.xts)]<-0;
    #----
    #Build Ror matrix for use in portfolio optimizations----
 
    
    firstgraph<-max(firstSim-1,df.Eras$lTrno[[1]]);
    l.Res<-list(df.Eras=df.Eras, 
                df.x.xts=df.x.xts,
                df.R.xts=df.R.xts,
                numEras=nrow(df.Eras),
                v.dates=v.dates, 
                numobs=length(v.dates),
                stocks=stocks,
                numvars=length(stocks),
                v.etf.xts=v.etf,
                v.etfRor.xts=v.etfRor.xts,
                fSimDate=v.dates[firstSim],
                fgraphDate=v.dates[firstgraph])
    #----
    return(l.Res);
}    
#######################################################################################
f.process2<-function(df.x.xts,df.dictentry){
    #----Prepare for PortfolioAnalytics package use----
    freq<-"months";
    trainyears<-2;
    testyears<-1;
    l.PortData   <-   f.prepPortData(df.x.xts,freq,trainyears,testyears);
    v.eop      <- endpoints(l.PortData$df.x.xts,on=freq);
    df.z.xts   <- l.PortData$df.x.xts[v.eop,];
    df.z       <- f.xts2df(df.z.xts)
    v.dates    <- df.z$Date; 
    returns   <-Return.calculate(df.z.xts,method='log');
    returns[is.na(returns)]<-0;
    sreturns<-returns[,-ncol(returns)]
    stocks<-names(sreturns);
    
    
    
    
    
}
f.ProcessEstimates<-function(df.x.xts,df.dictentry){
    #-------Prepare Data----
    freq<-"months";
    trainyears<-2;
    testyears<-1;
    l.PortData   <-   f.prepPortData(df.x.xts,freq,trainyears,testyears);
   
    q_portf <- portfolio.spec(assets=stocks);
    fi_constr <- weight_sum_constraint(type="full_investment");
    lo_constr <- box_constraint(type="long_only", assets=q_portf$assets);
    qu_constr <- list(fi_constr, lo_constr);
    
    ret_obj <- return_objective(name="mean");
    var_obj <- portfolio_risk_objective(name="var", risk_aversion=0.25);
    qu_obj <- list(ret_obj, var_obj);
    
    opt_qu <- optimize.portfolio(R=sreturns, portfolio=q_portf, 
                                 constraints=qu_constr, 
                                 objectives=qu_obj, 
                                 optimize_method="ROI",
                                 trace=TRUE);
    rb_qu <- optimize.portfolio.rebalancing(R=returns, portfolio=q_portf,
                                            constraints=qu_constr, 
                                            objectives=qu_obj, 
                                            optimize_method="ROI", 
                                            rebalance_on="quarters", 
                                            training_period=36)
    
    
###################################################
    l.PortModels<-list();
#    Maximum Returns objective----
    
    # Create portfolio object
    portName<-"MeanVar";

    l.opt_rebalancing<-f.make_Opt(portName,l.PortData,rebalFreq="years",objType="risk",objName="var");
    
    
    
    l.PortRes<-f.DisplayPortOpt(l.opt_rebalancing,l.PortData);
    l.PortModels[[portName]]<-l.PortRes;
    
    portName<-"MaxRetRandom";
    l.opt_rebalancing<-f.make_MaxRandomRet(portName,l.PortData,rebalFreq="years");
    l.PortRes<-f.DisplayPortOpt(l.opt_rebalancing,l.PortData);
    l.PortModels[[portName]]<-l.PortRes;
    charts.PerformanceSummary(l.PortRes$df.xts, Rf = 0/03, main = NULL, geometric = TRUE,
                              methods = "ModifiedVaR", width = 4, event.labels = NULL, ylog = FALSE,
                              wealth.index = TRUE, gap = 12, begin = c("first", "axis"),
                              legend.loc = "topleft", p = 0.95);
PCvbar<-charts.BarVaR(df.xts, main = "Returns", cex.legend = 0.8, colorset = 1:12,
                      ylim = NA,  perpanel = NULL, show.yaxis = c("all", "firstonly",
                                                                  "alternating", "none"))
    plot(l.PortRes$PrfCharts)
    
    
    
    return(l.PortRes)
}   
f.DisplayPortOpt<-function(l.opt_rebalancing,l.PortData){   
    # fsimobs               # number of the observation where simulation can begin 
    # nper                  # of rebalancing periods
    # v.rebalancingdates    # vector of the rebalancing date
    
    nper                <- length(l.opt_rebalancing);
    v.rebalancingdates  <- as.Date(names(l.opt_rebalancing));
    v.varnames          <- names(l.opt_rebalancing[[1]]$weights);
    nvar                <- length(l.opt_rebalancing[[1]]$weights);
    #get matrix of rebalancing weights---
    m.weights     <-matrix(0,nper,nvar)
    iper<-0;
    while(iper<nper){
      iper             <- iper+1;
      m.weights[iper,] <- l.opt_rebalancing[[iper]]$weights
    }
    m.weights[is.na(m.weights)]<-0.0;
    colnames(m.weights) <- v.varnames
    m.weights.xts       <- as.xts(m.weights,order.by = v.rebalancingdates)
    r.xts               <- l.PortData$df.R.xts;
    
    df.x.xts<-l.PortData$df.x.xts[v.rebalancingdates,1:nvar]
    returns<-Return.calculate(df.x.xts,method='log')
    returns[is.na(returns)]<-0
   Return.portfolio(returns,m.weights.xts)
    l.port      <-Return.rebalancing(returns, weights=m.weights.xts,wealth.index=TRUE,
                             verbose=TRUE )
    plot.xts( l.port$wealthindex)
    #l.port$returns
    #[1] "returns"      "contribution" "BOP.Weight"   "EOP.Weight"  
   # [5] "BOP.Value"    "EOP.Value"    "wealthindex" 
    fgraphdate             <-l.PortData$fgraphDate;
    fgraphspan          <-paste(fgraphdate,"/",sep="");
    df.xts<-merge.xts(l.port$returns,l.PortData$v.etfRor.xts,join='left');
    names(df.xts)[[1]]<-"Port";
    PC<-suppressMessages(charts.PerformanceSummary(df.xts, Rf = 0/03, main = NULL, geometric = TRUE,
                              methods = "ModifiedVaR", width = 4, event.labels = NULL, ylog = FALSE,
                              wealth.index = TRUE, gap = 12, begin = c("first", "axis"),
                              legend.loc = "topleft", p = 0.95));
    PCvbar<-charts.BarVaR(df.xts, main = "Returns", cex.legend = 0.8, colorset = 1:12,
                  ylim = NA,  perpanel = NULL, show.yaxis = c("all", "firstonly",
                                                                  "alternating", "none"))
    CR<-table.CalendarReturns(df.xts)
    # textplot(format.df(CR, na.blank=TRUE, numeric.dollar=FALSE, 
    #                    cdec=rep(1,dim(CR)[2])), rmar = 0.8, cmar = 1,  
    #          max.cex=.9, halign = "center", valign = "top", 
    #          row.valign="center", wrap.rownames=20, wrap.colnames=10, 
    #          col.rownames=c( rep("darkgray",12), "black", "blue"), 
    #          mar = c(0,0,3,0)+0.1)
    library(Hmisc)
    TS<-table.Stats(df.xts)
    TO<-table.ProbOutPerformance(df.xts[,1],df.xts[,ncol(df.xts)])
    TD<-table.Distributions(df.xts);
    TDD<-table.Drawdowns(df.xts);
    TRP<-table.RollingPeriods(df.xts);
    TSFM<-table.SFM(df.xts[,1],df.xts[,2])
    l.PortResults<-list(df.xts=df.xts,StatsTable=TS,PrfCharts=PC ,BarChart=PCvbar,ToutPerf=TO,TDrawDowns=TDD,RollPer=TRP,SFM=TSFM);
    return(l.PortResults);
}    
        # 
        # 
        # tq_mutate(df,cx=diff(XLU))
        # 
        
    
    
    # df.ARB<-df.ARB.xts[simSpan]%>%f.xts2df()
    # df.ARB<-df.ARB%>%dplyr::mutate (difRor=Port-ETF,CumETF=cumsum(ETF),CumPort=cumsum(Port),difCumRor=CumPort-CumETF)
    # df.ARB.xts<-df.ARB%>%f.df2xts()
    # 
    # plottitle=paste(df.dictentry$Description,"/3Y|1Q",sep="")
    # plot.xts(df.ARB.xts[graphspan,c("CumETF","CumPort")],legend.loc='left',main=plottitle);
    # 
    # 
    # 
    # 
    # 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
#     
#     portf <- portfolio.spec(stocks)
#     portf <- add.constraint(portf, type="full_investment")
#     portf <- add.constraint(portf, type="long_only")
#     #portf <- add.objective(portf, type="risk", name="StdDev")
#     portf <- add.objective(portf, type="quadratic_utility", 
#                                  risk_aversion=0.50)
#     #portf<-add.constraint(portfolio=portf,type="box", min=0.0, max=.3);
#     #portf<-add.constraint(portfolio=portf, type="leverage",min_sum=0.99, max_sum=1.01)
#     # annual rebalancing with 2 year training period
#     tic();
#     bt.opt1 <- optimize.portfolio.rebalancing(R.xts, portf,
#                                               optimize_method="random",
#                                               rebalance_on="years",
#                                               training_period=24,
#                                               rolling_window=24);
#     
#     # bt.opt2 <- optimize.portfolio.rebalancing(R.xts, portf,
#     #                                           optimize_method="pso",
#     #                                           rebalance_on="years",
#     #                                           training_period=24,
#     #                                           rolling_window=24)
#     bt.vw<-inverse.volatility.weight(R.xts,portf)
#     bt.vw$out
#     toc();
#    
#     #for each rebalancing period compute the projections
#     iper<-0;
#     df.R<-f.xts2df(R.xts)
#     v.rebalancingdates<-names(bt.opt1$opt_rebalancing)
#     pa<-paste(v.rebalancingdates[[1]],"/",sep="")
#     RG.xts<-R.xts[pa]
#     while(iper<nper){
#         iper<-iper+1;
#         l.or            <- bt.opt1$opt_rebalancing[[iper]];
#         fd<-as.Date(v.rebalancingdates[iper]);
#         ld<-as.Date(v.rebalancingdates[iper+1])-1;
#         if(iper<nper){
#             testspan<-paste(fd,ld,sep="/")
#         }else{
#             testspan<-paste(fd,"/",sep="");
#         }
#         RR<-R.xts[testspan,]
#         v.weights<-l.or$weights
#         v.port<-RR%*%v.weights;
#         df.ARB.xts[testspan,"ETF"]<-(v.targetror.xts[testspan])
#         df.ARB.xts[testspan,"Port"]<-v.port
#     }  
#     df.ARB<-df.ARB.xts%>%f.xts2df()
#     df.ARB<-df.ARB%>%dplyr::mutate (difRor=Port-ETF,CumETF=cumsum(ETF),CumPort=cumsum(Port),difCumRor=CumPort-CumETF)
#     df.ARB.xts<-df.ARB%>%f.df2xts()
#    # df.ARB.xts<-as.xts(df.ARB,order.by = v.dates); 
#     plottitle<-paste()
#     plottitle=paste(df.dictentry$Description,"/2Y|1Y",sep="")
#     plot.xts(df.ARB.xts[pa,c("CumETF","CumPort")],legend.loc='left',main=plottitle);
#     l.RES<-list(df.ARB.xts=df.ARB.xts,df.Eras=df.Eras,vplot=vplot);
#     return(l.RES)
# }
