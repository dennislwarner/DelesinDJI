f.tristate<-function(x,ub,lb){
    v<-rep(NA,length(x))
    v[x>=ub]<-1;
    v[x<=lb]<-(-1);
    v<-na.locf(v,na.rm=FALSE);
    v[is.na(v)]<-0;
    return(v)
}
f.GetTrades<-function(df.xts,method="Aroon",ub,lb){
    if(method=="Aroon20"){
        Aroon20<-aroon(df.xts[,2:3],15)
        
        v.indicator<-Aroon20$oscillator;
        v.indicator[is.na(v.indicator)]<-0;
    }
    # the trading pos will be a tristate value -1,NA,1
    v.pos<-f.tristate(v.indicator,ub,lb)
    #Trades occur when there is a change in the desired position
    v.trades<-sign(c(v.pos[1],diff(v.pos)));
    return(v.trades)
}
f.BuildRecords<-function(df.xts,v.trades,CASH){
    # take positions at the next opening price
    dfr.xts             <-df.xts;
    dfr.xts$trades      <-v.trades;
    
    
    
    
    v.tr<-v.trades;v.tr[v.tr==0]<-NA
    v.pos               <-na.locf(v.tr,na.rm=FALSE);
    v.pos[is.na(v.pos)] <-0;
    dfr.xts$pos         <-v.pos;
    v.nop               <-c(coredata(dfr.xts$Open)[-1],last(dfr.xts$Open))
    dfr.xts$nop         <-v.nop;
    dfr.xts$chnop       <-c(0,diff(dfr.xts$nop,na.pad=FALSE));
    dfr.xts$prof        <-dfr.xts$chnop*dfr.xts$pos;
    dfr.xts$bal         <-cumsum(dfr.xts$prof)
    return(dfr.xts)
}
f.BuildRecords2<-function(df.xts,v.trades,CASH){
    # take positions at the next opening price
    # do real $accounting
    
    dfr.xts             <-df.xts;
    dfr.xts$trades      <-v.trades;
    v.tr<-v.trades;v.tr[v.tr==0]<-NA
    v.pos               <-na.locf(v.tr,na.rm=FALSE);
    v.pos[is.na(v.pos)] <-0;
    dfr.xts$pos         <-v.pos;
    v.nop               <-c(coredata(dfr.xts$Open)[-1],last(dfr.xts$Open))
    dfr.xts$nop         <-v.nop;
    dfr.xts$chnop       <-c(0,diff(dfr.xts$nop,na.pad=FALSE));
    dfr.xts$prof        <-dfr.xts$chnop*dfr.xts$pos;
    dfr.xts$bal         <-cumsum(dfr.xts$prof);
    #add the cash trading
    t<-0;
    m.rec<-coredata(dfr.xts)
    dfr<-f.xts2df(dfr.xts)
    v.shares<-numeric(nrow(dfr))
    while(t<nrow(dfr.xts)){
        t<-t+1;
        if(dfr$trades[t]!=0){
            if(dfr$trades[t]<0){
                shares<-
                dfr$pos[t]<-1
            }else{
                
            }
        }else{
            
        }
    }
    return(dfr.xts)
}
f.makeTransactions<-function(dfr.xts,symb){
    # Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount	CurrencyCode	Commission	Description	Order ID
    # 17255155	Margin	T	Buy	50	38259P508	GOOG  				0	5/15/2012	5/18/2012	5/15/2012	608.6445	30433.23	USD	1.01	GOOGLE INC     	312455732
    
}
f.searchTrades<-function(df.xts,symb,cname,CASH){
    cat(symb,cname,nrow(df.xts),"\n");
    #----Try the Stochatic Momentum 
    #names(df.xts)<-c("Open","High","Low","Close","Volume")
    v.signals<-f.GetTrades(df.xts,method="Aroon20",50,-50);
    df<-f.TradeSim(df.xts,v.signals,symb,cname,CASH)
    
    
  
    dfTrading.xts<-f.BuildRecords(df.xts,v.trades,CASH);
    df.Trans.xts<-f.makeTransactions()
    head(dfTrading.xts[])
    SMI3MA <- SMI(df.xts[,2:4],
                  nSlowD=21,nFastK=5,nFastK=4,
                  maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)) );
    
    
    
    
    return()
}