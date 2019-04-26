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
f.ProcessEstimates<-function(df.x.xts,df.dictentry){
    v.eom<-endpoints(df.x.xts,on="months")
    df.z.xts         <- df.x.xts[v.eom,]
    df.z             <- f.xts2df(df.z.xts)
    v.dates          <- df.z$Date;
    v.etf.xts<-df.z.xts[,ncol(df.z.xts)]
    v.etf<-df.z[,ncol(df.z)]
    #make sure simulation doesn't start before target etf
    v.nonna<-!is.na(v.etf)
    firstsimobs<-which(!is.na(v.etf))[1]
   # v.etf<-df.z[,]
 
    M      <- ncol(df.z)-2
    m.p    <- as.matrix(df.z[,-1])
    m.r    <- m.p*0
  
    i<-2
    for(i in 1:ncol(m.r)){
        p<-m.p[,i];
        plag<-c(p[1],p[1:(length(p)-1)]);
        returns<-log(p/plag);
        m.r[,i]<-returns;
    }
    m.r[is.na(m.r)]<-0
    #begin optimization
    v.targetror<-m.r[,ncol(m.r)]
    v.targetror.xts<-as.xts(v.targetror,order.by = v.dates)
    m.ror<-m.r[,-ncol(m.r)];
    nobs<-nrow(m.ror)
    span<-24
    padding<-0
    if(span<firstsimobs){
        padding<-firstsimobs-span-1
    }
    cspan<-12
    m.x<-m.ror[(padding+1):nobs,];#chop off unneded beginning data that will never be used
   
    #  break i up into rolling chunks, beginning at a 2 year point and rolling by 1 year
    neras<-max(((nobs-padding)-span)/cspan,1)
    #prepare output for simulation
    df.ARB<-data.frame(matrix(0,nobs,6))
    df.ARB.xts<-as.xts(df.ARB,order.by = v.dates)
    df.Eras<-data.frame(matrix(0,neras,6)) #save Room for weights and summaries
    names(df.ARB.xts)<-names(df.ARB)<-c("ETF","Port","difRor","CumETF","CumPort","difCumRor")
    iera<-0;
    v.d<-v.dates[(padding+1):nobs]
    R<-m.r[(padding+1):nobs,-ncol(m.r)]
    R.xts<-as.xts(R,order.by=v.d)
    stocks<-colnames(R);
    # firstdate
    # match(firstdate,v.d)
    portf <- portfolio.spec(stocks)
   # portf <- add.constraint(portf, type="full_investment")
    portf <- add.constraint(portf, type="long_only")
    #portf <- add.objective(portf, type="risk", name="StdDev")
    portf <- add.objective(portf, type="quadratic_utility", 
                                 risk_aversion=0.25)
    portf<-add.constraint(portfolio=portf,type="box", min=0.0, max=.3);
    portf<-add.constraint(portfolio=portf, type="leverage",min_sum=0.99, max_sum=1.01)
    # annual rebalancing with 2 year training period
    tic();
    bt.opt1 <- optimize.portfolio.rebalancing(R.xts, portf,
                                              optimize_method="random",
                                              rebalance_on="years",
                                              training_period=24,
                                              rolling_window=24)
    bt.opt2 <- optimize.portfolio.rebalancing(R.xts, portf,
                                              optimize_method="ROI",
                                              rebalance_on="years",
                                              training_period=24,
                                              rolling_window=24)
    bt.vw<-inverse.volatility.weight(R.xts,portf)
    bt.vw$out
    toc();
    bt.opt1$elapsed_time
    bt.opt1$portfolio
    #bt.opt1$R
   #str(bt.opt1$opt_rebalancing)
    #number of rebalances---------------
    nper<-length(bt.opt1$opt_rebalancing)
    #for each rebalancing period compute the projections
    iper<-0;
    df.R<-f.xts2df(R.xts)
    v.rebalancingdates<-names(bt.opt1$opt_rebalancing)
    pa<-paste(v.rebalancing[[1]],"/",sep="")
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
    df.ARB.xts<-as.xts(df.ARB,order.by = v.dates); 
    plottitle<-paste()
    plottitle=paste(df.dictentry$Description,"/2Y|1Y",sep="")
    vplot<-plot.xts(df.ARB.xts[pa,c("CumETF","CumPort")],legend.loc='left',main=plottitle);
    l.RES<-list(df.ARB.xts=df.ARB.xts,df.Eras=df.Eras,vplot=vplot);
    return(l.RES)
}
