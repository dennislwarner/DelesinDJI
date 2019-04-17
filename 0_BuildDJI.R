f.buildDJIdata<-function(){
    #original data from MH----
    dirPriceData   <- paste(dirData,"/AdjOpens/Sectors",sep="");
    fnin         <- "DJ_OrigData.csv";
    df.data      <- read_csv(fnin,);
    df.odata.xts<-f.df2xts(df.data); # Get mike's original data in xts form
    df.odata<-df.odata.xts%>%f.xts2df();
    v.DIA<-df.odata$DIA;
    v.Divisor<-df.odata$Divisor;
    v.INDU<-df.odata$INDU
    firstdate    <- index(first(df.odata.xts));
    lastdate     <- index(last(df.odata.xts));
    origspan     <- paste(firstdate,"/",lastdate,sep="");
    
    #we must  find all the split dates for every dow component----
    df.DJIAComposition<-read.xlsx(paste(dirDocs,"/DJIAComposition.xlsx",sep=""),sheetName="DJIAComposition");
    v.numstocks<-df.DJIAComposition$NumStocks;
    df.DJ<-df.DJIAComposition%>%dplyr::select(-NumStocks)
    v.djnames<-c("Dates",sort(names(df.DJ)[-1]));
    df.DJ<-df.DJIAComposition[,v.djnames]%>%dplyr::select(-DOW)
    df.DJ.xts<-f.df2xts(df.DJ)
    firstj<-index(first(df.DJ.xts))-1095
    lastj<-index(last(df.DJ.xts))
    djspan<-paste(firstj,"/",lastj,sep="");
    df.SPY.xts<-f.getSPY()%>%f.df2xts()
    df.SPY.xts<-df.SPY.xts[djspan]
    df.Y.xts<-merge.xts(df.SPY.xts,df.DJ.xts)%>%na.locf()%>%f.xts2df()%>%dplyr::select(-Close)%>%f.df2xts()
    df.X.xts<-merge.xts(df.odata.xts,df.Y.xts,join='left')[origspan]
    m.membership<-coredata(df.X.xts)[,-c(1:3)]
    
    df.P.xts<-df.SPY.xts[origspan]
    df.UP.xts<-df.P.xts
    df.AP.xts<-df.P.xts
    v.tickers<-names(df.DJ.xts)
    
    iticker<-0;
    while(iticker < length(v.tickers)){
        iticker<-iticker+1;
        symb<-v.tickers[iticker];
        #cat(iticker,symb,"\n");
        df.cc<-dt.Common%>%dplyr::filter(ticker==symb)
        
        fnin<-paste(dirPriceData,"/",df.cc$sector,"/",symb,".RDATA",sep="");
        ss_dt.p<-load(fnin)
        dt.p<-dplyr::select(dt.p,date,close,closeunadj); #silently load the dt.p array for this company
        dt.p<-dt.p%>%dplyr::select(date,close,closeunadj)
        dt.p.xts<-f.df2xtsD(dt.p,"date")
        v.u.xts<-dt.p.xts$closeunadj;
        v.a.xts<-dt.p.xts$close;
        df.UP.xts<-merge.xts(df.UP.xts,v.u.xts,join='left');
        df.AP.xts<-merge.xts(df.AP.xts,v.a.xts,join='left');
    }
    names(df.UP.xts)[-1]<-v.tickers
    names(df.AP.xts)[-1]<-v.tickers
    df.UP<-df.UP.xts%>%f.xts2df()
    
    m.p<-coredata(df.UP.xts)[,-1]
    m.p[is.na(m.p)]<-0
    
    #We want to construct a near-twin of the DJIA
    #when we are missing the requisite historical data, we
    #construct a mimic, using the data we have
    #at each change point compute a new divisor and walk backward through time
    numobs<-nrow(m.membership)
    t<-numobs+1;
    v.mprior<-m.membership[t-1,]
    thisdivisor<-v.Divisor[t-1]
    while(t>1){
        t<-t-1
        v.m<-m.membership[t,]
        sm<-v.m%*%m.p[t,];
        implieddivisor<-sm/v.INDU[t]
        #cat(t,index(df.UP.xts[t]),sm,implieddivisor,v.INDU[t],"\n");
    }
    v.changedates<-df.DJIAComposition$Dates;
    m.membership.xts<-xts(m.membership,order.by = index(df.UP.xts))
 l.X<-list(m.membership.xts=m.membership.xts,
           v.changedates=v.changedates,
           v.dates=index(df.UP.xts),
            df.UP.xts=df.UP.xts,
            df.AP.xts=df.AP.xts,
            df.odata.xts=df.odata.xts
           );
 return(l.X)   
}
    