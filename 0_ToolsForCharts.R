f.getPricesA <- function(v.tickers) {
    # retrieve data from Quandl's ShaRadar
    # adjust names to replicate that required by the quantmod package routines
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
        fn2<-paste("EOD/",itick,  sep = "")
        df.xts<-Quandl(fn2,api_key = '1yhZtVwmHpc7qys3iMuJ',
                       type = "xts", start_date = "2003-01-01");
        df.xts<-df.xts[,c("Adj_Open","Adj_High","Adj_Low","Adj_Close")];
        #rename columns using the quantmod conventions
        newnames<-paste(itick,c("Open","High","Low","Close"),sep=".")
        names(df.xts)<-newnames;
        l.prices[[itick]]<-df.xts;
    }
    return(l.prices);    
}
f.getAdjustedClose <- function(symb){
    # retrieve data from Quandl's ShaRadar
    # adjust names to replicate that required by the quantmod package routines
  
    #get adjusted close..................
    if(str_sub(symb,1,3)=="EOD"){
        fn <- paste(symb, ".11", sep = "")
    }else{
        fn <- paste("EOD/",symb, ".11", sep = "")
    }
    #fn2<-paste("EOD/",symb,  sep = "")
    df.xts<-Quandl(fn,api_key = '1yhZtVwmHpc7qys3iMuJ',
                   type = "xts", start_date = "2003-01-01");
   #df.xts<-df.xts[,c("Adj_Open","Adj_High","Adj_Low","Adj_Close")];
    #rename columns using the quantmod conventions
    #
    names(df.xts)<-symb;
        
    
    return(df.xts);    
}
mapXts <- function(Xts, cFUN) {
    if(!is.xts(Xts)) stop("Must supply function with xts object")
    Z <- Xts
    for (j in 1:ncol(Xts)) {
        Z[,j] <- do.call(cFUN, list(Xts[,j]))
    }
    Z
}
f.periodFromDaily<-function(x.xts,HP){
    z<-as.numeric(coredata(x.xts));
    z[is.na(z)]<-0.;
    cs<-cumsum(z);
    pr<-diff(cs,lag=HP);
    pr[is.na(pr)]<-0;
    pr.xts<-xts(z,order.by=index(x.xts));
    return(pr.xts);
}
f.myAror<-function(x){
    lx<-log(x)
    dlx<-c(0,diff(lx));
    dlx[is.na(dlx)]<-0;
    
    xsum <- cumsum(dlx);
    ar<-diff(xsum,lag=252)
}
f.myHPror<-function(x.xts,HP){
    x<-as.numeric(coredata(x.xts));
    lx<-log(x);
    dlx<-c(0,diff(lx));
    dlx[is.na(dlx)]<-0;
    xsum<-cumsum(dlx);
    df<-data.frame(lx,dlx,xsum);
    
    periodRor<-c(rep(NA,HP),diff(xsum,lag=HP));
    return(periodRor);
}
f.myxtsHPror<-function(p.xts,HP,cname="DailyROr"){
    #determine the time series of holding period yields for
    #each column of p.xts, zero fill the leadup data
    X<-(coredata(p.xts));
    for(j in c(1:ncol(X))){
        lx<-log(X[,j]);
        dlx<-c(0,diff(lx));
        dlx[is.na(dlx)]<-0;
        xsum<-cumsum(dlx);
        periodRor<-c(rep(NA,HP),diff(xsum,lag=HP))
        X[,j]<-periodRor;
    }
    #reconstitute the .xts type
    df.ROR.xts<-xts(X,order.by = index(p.xts));
    names(df.ROR.xts)<-cname;
    ;
    return(df.ROR.xts);
}
f.extractPeriodic<-function(x){
    y<-x[!is.na(x)];
    return(y)
}