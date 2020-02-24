f.delta<-function(x){
    y<-c(0,diff(x,1))
    return(y)
}
f.deltanumeric<-function(x){
    if(!is.numeric(x)){return(x)}
    y<-c(0,diff(x,1))
    y[is.na(y)]<-0.
    return(y)
}
f.df2xts <- function(x) {
    v.d<-as.Date(x$Date);
    
    df.xts<-xts(x[,-1],order.by=v.d);
    names(df.xts)<-names(x)[-1]
    return(df.xts)
    #data.frame(Date=index(x), coredata(x))
}
f.df2xtsD <- function(x,dname) {
    xd<-unlist(x[,dname])
    v.d<-as.Date(xd);
    dpos<-str_which(dname,names(x))
    
    df.xts<-xts(x[-dpos],order.by=v.d);
    names(df.xts)<-names(x)[-dpos]
    return(df.xts)
    #data.frame(Date=index(x), coredata(x))
}
f.diff1<-function(x){
    xx<-c(0,diff(x))
    return(xx)
}
f.dfall2xts<-function(df){
    dfn<-Filter(is.numeric,df)
    df.id<-Filter(is_character,df);
    v.d<-df$Date;
    df.xts<-xts(dfn,order.by = v.d);
    
}
f.divSum<-function(x,y,z){
    v<-x/(y+z);
    vv<-na.locf(v,na.rm=FALSE);
    return(vv)
}
f.getPricesA <- function(v.tickers) {
    it <- 0;
    N<-length(v.tickers);
    l.prices<-list();
    while(it<N){
        it<-it+1;
        itick <- v.tickers[it]
        #get adjusted close..................
        if(str_sub(itick,1,3)=="EOD"){
            fn <- paste(itick, ".11", sep = "")
        }else{
            fn <- paste("EOD/",itick, ".11", sep = "")
        }
        #fn2<-paste("EOD/",itick,  sep = "")
        df.xts<- Quandl(fn,
                        api_key = '1yhZtVwmHpc7qys3iMuJ',
                        type = "xts",
                        start_date = "2003-01-01");
        names(df.xts)=itick;
        l.prices[[itick]]<-df.xts;
    }
    return(l.prices);    
}
f.getSPY <- function() {
    df.SPY <- Quandl('EOD/SPY', api_key = '1yhZtVwmHpc7qys3iMuJ',
                     start_date = "1998-01-02",
                     column_index = '4') %>% arrange(Date)
    df.SPY <- as_tbl_time(df.SPY, index = Date)
    return(df.SPY)
}
f.getSPYRoR<-function(){
    #load a daily history of SPY.  convert to a weekly
    #series of prices, then compoute the 1week, 1 quarter,
    #2 quarter, 1 year, and 2 year forward prices,
    #then compute the compounded rates of return for each horizon
    dt.SPY <- f.getSPY()%>%mutate(CRoR=cumsum(rSPY))
    v.dates<-dt.SPY$Date;
    dt.SPY.xts<-xts(dt.SPY[,-1],order.by =v.dates);
    v.eow<-endpoints(dt.SPY.xts,on="weeks",k=1);
    dt.SPYw.xts<-dt.SPY.xts[v.eow,];
    v.weeks<-index(dt.SPYw.xts)
    df.SPYw<-as.data.frame(dt.SPYw.xts)%>%
        mutate(p1w=lead(Close,n=1),
               p1m=lead(Close,n=4),
               p1q=lead(Close,n=13),
               p2q=lead(Close,n=26),
               p1y=lead(Close,n=52),
               p2y=lead(Close,n=104),
               r1w=log(p1w/Close),
               r1m=log(p1m/Close),
               r1q=log(p1q/Close),
               r2q=log(p2q/Close),
               r1y=log(p1y/Close),
               r2y=log(p2y/Close));
    dt.SPYw.xts<-as.xts(df.SPYw,order.by=v.weeks)
    return(dt.SPYw.xts)
    
}
f.huxFormatPerfTable<-function(df.Perf,tag){
    ht<-as_hux(df.Perf,add_colnames=TRUE,autoformat = TRUE)%>%
        set_font_size(8)%>%
        set_bottom_border(row=1,col=c(1:ncol(df.Perf)),value=1)%>%
        set_outer_borders(value=2)%>%
        set_number_format('%5.2f')%>%
        set_caption(tag)
    return(ht)
}
f.internalFill<-function(x){
    v.obs<-1:length(x);
    
    v.goodobs<-v.obs[is.finite(x)]
    if(length(v.goodobs)<3){
        return(x)
    }else{
        firstgood<-min(v.goodobs);
        lastgood<-max(v.goodobs);
        y<-x;
        y[firstgood:lastgood]<-na.locf(x[firstgood:lastgood])
        return(y)
    }
}
f.OneYearChange<-function(Q){
    N<-length(Q)
    if(length(Q)>=4){
        #z<-c(0,0,0,0,Q[-c(1:4)]-Q[-c((N-3):N)]);
        #qcur<-q[-c(1:4)];
        ##qlag<-q[-c((N-3):N)]
        #z<-log(f.safeDiv(qcur,qlag))
        z<-c(0,0,0,0,diff(Q,lag=4))
    }else{
        z<-0*Q;
    }
    return(z)
}
f.OneYearPChange<-function(Q){
    N<-length(Q)
    if(length(Q)>=4){
        #z<-c(0,0,0,0,Q[-c(1:4)]-Q[-c((N-3):N)]);
        #qcur<-q[-c(1:4)];
        ##qlag<-q[-c((N-3):N)]
        #z<-log(f.safeDiv(qcur,qlag))
        z<-c(0,0,0,0,diff(Q,lag=4))
    }else{
        z<-0*Q;
    }
    return(z)
}
f.performance<-function(x.xts){
    v.perf<-numeric(15);
    
    x.xts[!is.finite(x.xts)]<-0
    
    #x.xts<-100*x.xts
    v.perf[1]<-nquarters(x.xts);  #count the quarters
    v.perf[2]<-sum(x.xts>0)       #count the winning quarters
    v.perf[3]<-sum(x.xts);        #total profit(loss)
    v.perf[4]<-PerformanceAnalytics::maxDrawdown(x.xts)
    v.perf[5]<-round(mean(x.xts) ,digits=3);
    v.perf[6]<-round(sd(x.xts[x.xts<0]),digits=3);
    v.perf[7]<-PerformanceAnalytics::UpsidePotentialRatio(x.xts,MAR=0.05/4)
    v.perf[8]<-round(100*sum(x.xts>0)/length(x.xts),digits=2);
    v.perf[9]<-sum(x.xts)>0
    v.perf[10]<-1;
    v.perf[11]<-round(min(x.xts),digits=3);
    v.perf[12]<-mean(x.xts['1999/2003'],na.rm=TRUE)
    v.perf[13]<-mean(x.xts['2004/2008'])
    v.perf[14]<-mean(x.xts['2009/2013'])
    v.perf[15]<-mean(x.xts['2014/2019'])
    v.perf[!is.finite(v.perf)]<-0
    
    names(v.perf)<-c("nQ","nWin","Prof","DrwDn","Mean","dsStd",
                     "UPR","%Wins","Winner","Company","minPer",
                     "y99-03","y04-08","y09-13","y14-19");
    return(v.perf)
    
}
f.pgrat<-function(x){
    proc=f.safeDiv(x,lag(x))
    
    return(proc)
}
f.PriceToCumRor<-function(v.xts){
    r.xts<-as.xts(cumsum(c(0,log(as.numeric(v.xts[-1])/as.numeric(v.xts[-length(v.xts)])))),order.by=index(v.xts))
}
f.proc<-function(x){
    x.lag<-lag(x);
    x.lag[1]<-x[1];
    x.diff<-c(0,diff(x))
    proc<-f.safeDiv(x.diff,x.lag);
    return(proc)
}
f.pseudoLog10 <- function(x) { asinh(x/2)/log(10) }
f.pseudoROC<-function(x,nlag){
    if(!is.numeric(x)){
        return(x)
    }
    if(length(x)>=nlag){
        xlag<-lag(x,nlag)
        
        xx<-f.pseudoLog10(xlag);
        
        y<-f.pseudoLog10(x)
        roc<-y-xx
        roc[1:nlag]<-0;
    }else{
        roc<-numeric(length=length(x))
    }
    return(roc)
}
f.LagpseudoROC<-function(x,nlag){
    if(length(x)>=nlag){
        xlag<-lag(x,nlag)
        
        xx<-f.pseudoLog10(xlag);
        
        y<-f.pseudoLog10(x)
        roc<-y-xx
        roc[1:nlag]<-0;
    }else{
        roc<-numeric(length=length(x))
    }
    return(roc[-1])
}
f.roc <- function(x) {
    xlag <- lag(x, 1);
    xlag[1] <- x[1];
    xratio <- f.safeDiv(x, xlag);
    roc <- ifelse(xratio > 0, log(xratio), 0);
    return(roc)
}
f.safeDiv<-function(x,y){
    z<-ifelse(y!=0,x/y,NA)
    return(z)
}
f.SmoothedSeries<-function(x,w){
    #apply exponential smoothing to series x
    #    where   w is the decay parameter
    #   computed from formula  w = .5^(1/halflife)
    #   and halflife is the target halflife number of periods
    #
    N<-length(x)
    y<-x
    omw<-1-w
    for(tt in (2:N))
    {
        y[tt]<-y[tt-1]*w + omw*x[tt]
    }
    y
}
f.SmoothedSeriesHalfLife<-function(x,H=5){
    #apply exponential smoothing to series x
    #    where   w is the decay parameter
    #   computed from formula  w = .5^(1/halflife)
    #   and halflife is the target halflife number of periods
    #
    w<-.5^(1.0/H)
    N<-length(x)
    y<-x
    omw<-1-w
    for(tt in (2:N))
    {
        y[tt]<-y[tt-1]*w + omw*x[tt]
    }
    y
}
f.sortino<-function(x){
    dsd<-sd(x<0)
    sr<-f.safeDiv(mean(x,na.rm=TRUE),dsd)
    sr[is.na(sr)]<-0;
    sr
}
f.xts2df <- function(x) {
    data.frame(Date=index(x), coredata(x))
}
ulc<-function(X,n=5){X[1:n,1:n]}
llc<-function(X,n=5){X[(nrow(X)-(n-1)):nrow(X),1:n]}
lrc<-function(X,n=5){X[(nrow(X)-(n-1)):nrow(X),(ncol(X)-n+1):ncol(X)]}

