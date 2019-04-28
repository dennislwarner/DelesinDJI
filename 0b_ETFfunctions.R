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
f.prepPortData<-function(df.x.xts,freq,trainyears,testyears){
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
    df.z.xts   <- df.x.xts[v.eop,]
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
    
 
    #----
    #Build Ror matrix for use in portfolio optimizations----
    i<-1
    df.R.xts<-df.z.xts
    for(i in 1:ncol(df.R.xts)){
        p<-df.z.xts[,i];
        plag<-lag(p);
        plag[1]<-p[1]
        
        r<-log(p/plag);
        r[is.na(r)]<-0;
        df.R.xts[,i]<-r
    }
    v.etfRor.xts<-df.R.xts[,ncol(df.R.xts)]
    df.R.xts<-df.R.xts[,-ncol(df.R.xts)]
    stocks<-names(df.R.xts)
    firstgraph<-max(firstSim-1,df.Eras$lTrno[[1]]);
    l.Res<-list(df.Eras=df.Eras, 
                df.R.xts=df.R.xts,
                v.dates=v.dates, 
                stocks=stocks,
                v.etfRor.xts=v.etfRor.xts,
                fSimDate=v.dates[firstSim],
                fgraphDate=v.dates[firstgraph])
    #----
    return(l.Res);
}    
#######################################################################################
f.ProcessEstimates<-function(df.x.xts,df.dictentry){
    #-------Prepare Data----
    freq<-"months";
    trainyears<-2;
    testyears<-1;
    l.Res   <-   f.prepPortData(df.x.xts,freq,trainyears,testyears)
    #names(l.Res)  "df.Eras"  "df.R.xts" "v.dates"
    v.dates    <-l.Res$v.dates;
    R.xts<-l.Res$df.R.xts;
    returns<-R.xts;
    df.Eras<-l.Res$df.Eras;
    stocks<-l.Res$stocks;
    v.etfRor.xts<-l.Res$v.etfRor.xts;
    numEras<-nrow(df.Eras);
    numobs<-length(v.dates)
    numvars<-length(stocks);
    #begin optimization
    #prepare output for simulation
    df.ARB<-data.frame(matrix(0,nobs,6))
    df.ARB.xts<-as.xts(df.ARB,order.by = v.dates)
    names(df.ARB.xts)<-names(df.ARB)<-c("ETF","Port","difRor","CumETF","CumPort","difCumRor")
    iera<-0;
    #----
###################################################
    suppressMessages(library(PortfolioAnalytics));
    suppressMessages(library(foreach));
    suppressMessages(library(iterators));
    suppressMessages(library(ROI));
    suppressMessages(library(ROI.plugin.quadprog));
    suppressMessages(library(ROI.plugin.glpk));
#    Maximum Returns objective----
    
    # Create portfolio object
    portf_maxret <- portfolio.spec(assets=stocks)
    
    # Add constraints to the portfolio object
    portf_maxret <- add.constraint(portfolio=portf_maxret, type="full_investment")
    portf_maxret <- add.constraint(portfolio=portf_maxret, type="box",
                                   min=rep(0,numvars),
                                   max=rep(0.3,numvars))
    # Add objective to the portfolio object
    portf_maxret <- add.objective(portfolio=portf_maxret, type="return", name="mean")
    print(portf_maxret)
    summary(portf_maxret)
    # Run the optimization
    opt_maxret <- optimize.portfolio(R=returns, portfolio=portf_maxret, 
                                     optimize_method="ROI", trace=TRUE)
    print(opt_maxret)
    
    summary(opt_maxret)
    names(opt_maxret)
    extractStats(opt_maxret)
    extractWeights(opt_maxret)
    plot(opt_maxret, chart.assets=TRUE, xlim=c(0.02, 0.18))
    chart.RiskReward(opt_maxret,return.col="mean", risk.col="sd", 
                     chart.assets=TRUE, xlim=c(0.01, 0.18), main="Maximum Return")
    bt_maxret <- optimize.portfolio.rebalancing(R=returns, portfolio=portf_maxret,
                                                optimize_method="ROI", 
                                                rebalance_on="quarters", 
                                              training_period=36)
    names(bt_maxret) 
    bt_maxret[[1]]
    bt_maxret$elapsed_time
    bt_maxret$portfolio
    # bt_maxret$R
    #str(v$opt_rebalancing)
    #number of rebalances---------------
    
  l.opt_rebalancing <- bt_maxret$opt_rebalancing;  
l.RES<-f.DisplayPortOpt(bt_maxret$opt_rebalancing,l.Res)
    
    
f.DisplayPortOpt<-function(l.opt_rebalancing,l.Res){   
    # fsimobs               # number of the observation where simulation can begin 
    # nper                  # of rebalancing periods
    # v.rebalancingdates    # vector of the rebalancing date
    
    nper                <-length(l.opt_rebalancing);
    v.rebalancingdates  <-names(bt_maxret$opt_rebalancing);
    fgraphdate             <-l.Res$fgraphDate;
    fgraphspan          <-paste(fgraphdate,"/",sep="");
    RG.xts<-l.Res$df.R.xts[fgraphspan]

    
    
    
    
}    
    
    
    nper<-length( bt_maxret$opt_rebalancing)
    
    pa<-paste(v.rebalancingdates[[1]],"/",sep="")
    fsimobs<-match(l.Res$fSimDate,v.dates)
    fgraphdate<-v.dates[fsimobs-1]
    
    simSpan<-paste(l.Res$fSimDate,"/",sep="");
    
    iper<-0;
    while(iper<nper){
        iper<-iper+1;
        l.or            <- bt_maxret$opt_rebalancing[[iper]];
        if(iper==1){
            fd<-as.Date(v.rebalancingdates[iper]);
            idate<-match(fd,v.dates)
            fgd<-v.dates[idate-1]
            graphspan<-paste(fgd,"/",sep="");
        }
        fd<-as.Date(v.rebalancingdates[iper]);
        ld<-as.Date(v.rebalancingdates[iper+1])-1;
        if(iper<nper){
            testspan<-paste(fd,ld,sep="/")
        }else{
            testspan<-paste(fd,"/",sep="");
        }
        RR<-R.xts[testspan,]
        v.weights<-l.or$weights
        v.port<-RR%*%v.weights;
        df.ARB.xts[testspan,"ETF"]<-(v.etfRor.xts[testspan])
        df.ARB.xts[testspan,"Port"]<-v.port
    }  
    
    df.ARB<-df.ARB.xts[simSpan]%>%f.xts2df()
    df.ARB<-df.ARB%>%dplyr::mutate (difRor=Port-ETF,CumETF=cumsum(ETF),CumPort=cumsum(Port),difCumRor=CumPort-CumETF)
    df.ARB.xts<-df.ARB%>%f.df2xts()
   
    plottitle=paste(df.dictentry$Description,"/3Y|1Q",sep="")
    plot.xts(df.ARB.xts[graphspan,c("CumETF","CumPort")],legend.loc='left',main=plottitle);
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    portf <- portfolio.spec(stocks)
    portf <- add.constraint(portf, type="full_investment")
    portf <- add.constraint(portf, type="long_only")
    #portf <- add.objective(portf, type="risk", name="StdDev")
    portf <- add.objective(portf, type="quadratic_utility", 
                                 risk_aversion=0.50)
    #portf<-add.constraint(portfolio=portf,type="box", min=0.0, max=.3);
    #portf<-add.constraint(portfolio=portf, type="leverage",min_sum=0.99, max_sum=1.01)
    # annual rebalancing with 2 year training period
    tic();
    bt.opt1 <- optimize.portfolio.rebalancing(R.xts, portf,
                                              optimize_method="random",
                                              rebalance_on="years",
                                              training_period=24,
                                              rolling_window=24);
    
    # bt.opt2 <- optimize.portfolio.rebalancing(R.xts, portf,
    #                                           optimize_method="pso",
    #                                           rebalance_on="years",
    #                                           training_period=24,
    #                                           rolling_window=24)
    bt.vw<-inverse.volatility.weight(R.xts,portf)
    bt.vw$out
    toc();
   
    #for each rebalancing period compute the projections
    iper<-0;
    df.R<-f.xts2df(R.xts)
    v.rebalancingdates<-names(bt.opt1$opt_rebalancing)
    pa<-paste(v.rebalancingdates[[1]],"/",sep="")
    RG.xts<-R.xts[pa]
    while(iper<nper){
        iper<-iper+1;
        l.or            <- bt.opt1$opt_rebalancing[[iper]];
        fd<-as.Date(v.rebalancingdates[iper]);
        ld<-as.Date(v.rebalancingdates[iper+1])-1;
        if(iper<nper){
            testspan<-paste(fd,ld,sep="/")
        }else{
            testspan<-paste(fd,"/",sep="");
        }
        RR<-R.xts[testspan,]
        v.weights<-l.or$weights
        v.port<-RR%*%v.weights;
        df.ARB.xts[testspan,"ETF"]<-(v.targetror.xts[testspan])
        df.ARB.xts[testspan,"Port"]<-v.port
    }  
    df.ARB<-df.ARB.xts%>%f.xts2df()
    df.ARB<-df.ARB%>%dplyr::mutate (difRor=Port-ETF,CumETF=cumsum(ETF),CumPort=cumsum(Port),difCumRor=CumPort-CumETF)
    df.ARB.xts<-df.ARB%>%f.df2xts()
   # df.ARB.xts<-as.xts(df.ARB,order.by = v.dates); 
    plottitle<-paste()
    plottitle=paste(df.dictentry$Description,"/2Y|1Y",sep="")
    plot.xts(df.ARB.xts[pa,c("CumETF","CumPort")],legend.loc='left',main=plottitle);
    l.RES<-list(df.ARB.xts=df.ARB.xts,df.Eras=df.Eras,vplot=vplot);
    return(l.RES)
}
