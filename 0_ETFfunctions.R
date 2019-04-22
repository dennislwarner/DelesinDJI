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
    df.z.xts<-df.x.xts[v.eom,]
    df.z<-f.xts2df(df.z.xts)
    v.dates<-df.z$Date;
    v.etf<-df.z[,ncol(df.z)]
    v.etf<-df.z[,]
  
    M<-ncol(df.z)-2
    m.p<-as.matrix(df.z[,-1])
    m.r<-m.p*0
  
    i<-2
    for(i in 1:ncol(m.r)){
        p<-m.p[,i];
        plag<-c(p[1],p[1:(length(p)-1)]);
        returns<-log(p/plag);
        m.r[,i]<-returns;
    }
      m.r[is.na(m.r)]<-0
    #begin optimization
    m.ror<-m.r[,-ncol(m.r)];
    nobs<-nrow(m.ror)
    span<-24
    cspan<-12
    
    
    m.x<-m.ror[1:span,]
    v.etfror<-m.r[,ncol(m.r)];
    #  break i up into rolling chunks, beginning at a 5 year poit and rolling by 1 year
    
    neras<-(nobs-span)/cspan
    
  
    #prepare output for simulation
    df.ARB<-data.frame(matrix(0,nobs,6))
    df.Eras<-data.frame(matrix(0,neras,6)) #save Room for weights and summaries
    names(df.ARB)<-c("ETF","Port","difRor","CumETF","CumPort","difCumRor")
    
    iera<-0;
   
    while(iera<neras){
        iera<-iera+1;
        first<-(iera-1)*cspan+1;
        last<-first+span-1
        #cat(iera,first,last,"\n");
        m.xx<-m.ror[first:last,];
        #find the 30 stocks with best sharpe ratio
        v.rorsums<-colSums(m.xx)
        v.ranks<-rank(v.rorsums)
        dfr<-data.frame(vn=colnames(m.xx),vnc=1:M,RorSum=v.rorsums,RorRank=v.ranks)%>%dplyr::arrange(desc(RorRank))
        top<-min(25,M)
        v.wanted<-dfr$vnc[1:top]
        m.x<-m.xx[,v.wanted]
        tic();
        defaultControl <- gaControl()
        #print( "gareal_Population")
        ga_res<-ga(type="real-valued",
                   function(x){-obj(x)},
                   lower=rep(0,ncol(m.x)),
                   upper=rep(1,ncol(m.x)),
                   maxiter=50000,
                   popSize=100,
                   elitism=25,
                   run=50,
                   parallel=TRUE,
                   monitor=FALSE,
                   seed=1);
        
        v.weights<-ga_res@solution[1,]
        df.res<-data.frame(colnames(m.x),v.weights)
        v.portRor<-portreturns(v.weights)
        summary(v.portRor)
        sd(v.portRor)
        summary(v.etfror)
        sd(v.etfror)
        sharpe(v.weights)
        tseries::sharpe(v.etfror[first:last])
        tseries::sharpe(v.portRor)
        df.Ror<-data.frame(ETF=cumsum(v.etfror[first:last]),PORT=cumsum(v.portRor))
        df.Ror.xts<-as.xts(df.Ror,order.by = v.dates[first:last])
        plottitle=paste(df.dictentry$Description,"/Insample",sep="")
        plot.xts(df.Ror.xts,legend.loc='left',main=plottitle)
        firstout<-last+1;
        lastout<-min(last+cspan,nobs)
        m.xout<-m.ror[firstout:lastout,v.wanted]
        v.portRorout<-m.xout%*%v.weights
        v.datesout<-v.dates[firstout:lastout]
        df.Rorout<-data.frame(ETF=cumsum(v.etfror[firstout:lastout]),PORT=cumsum(v.portRorout))
        df.Rorout.xts<-as.xts(df.Rorout,order.by = v.datesout)
        plottitle=paste(df.dictentry$Description,"/Out of Sample",sep="")
        plot.xts(df.Rorout.xts,legend.loc='left',main=plottitle)
        df.ARB$ETF[firstout:lastout]<-v.etfror[firstout:lastout];
        df.ARB$Port[firstout:lastout]<-v.portRorout;
       
        #df.Eras[iera,1:length(v.weights)]<-v.weights
        
        toc();
    }
    df.ARB<-df.ARB%>%dplyr::mutate(difRor=Port-ETF,CumETF=cumsum(ETF),CumPort=cumsum(Port),difCumRor=CumPort-CumETF)%>%
                     dplyr::select(ETF,Port,difRor,CumETF,CumPort,difCumRor)
    df.ARB.xts<-as.xts(df.ARB[-c(1:span),],order.by = v.dates[-c(1:span)])
    df.Compare.xts<-df.ARB.xts[,c("CumETF","CumPort")]
    plottitle=paste(df.dictentry$Description,"/2Y|1Y",sep="")
    plot.xts(df.Compare.xts,legend.loc='left',main=plottitle)
    vplot<-plot.xts(df.Compare.xts,legend.loc='left',main=plottitle)
    l.RES<-list(df.ARB.xts=df.ARB.xts,df.Eras=df.Eras,vplot=vplot);
    return(l.RES)
}
