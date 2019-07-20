f.initTradeRecord<-function(){
    #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	
    #UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount
    #CurrencyCode	Commission	Description	Order ID
    l.trec<-list(Account_Number=123456,Type="",TradeInd="T",Transaction="Buy/Sell",Quantity=0,
                 Cusip="11111t='",Symbol="AAPL",CallPut="",underlyingSymbol="",ExpireDate="",
                 StrikePrice="",TD="",SD="",ActivityDate="",price=0.0,Amount=0.0,
                 CurrencyCode="USD",Commission="5.00",Description="Apple Systems",Order_ID=12345);
    return(l.trec);
}
f.initPosition<-function(){
    l.pos<-list(Symbol="AAPL",shares=0,dayon=0,priceon=0,curprice=0,pandl=0,Commissions=0.0,
                 dayoff =0,age=0);
    return(l.pos);
}
f.TradeSim<- function(df.xts,v.signals,symb,cname,CASH) {
    #df.xts  columns Date.Open,High,Low,Close,Volume,Trades
        v.nop               <-c(coredata(df.xts$Open)[-1],last(df.xts$Open))
        df.xts$nop         <-v.nop;
        df.xts$chnop       <-c(0,diff(df.xts$nop,na.pad=FALSE));
        dfr<-f.xts2df(df.xts)
        names(dfr)
        dfr$signals<-v.signals;
        
        curpos<-list(symbol="",shares=0,onday=0,onprice=0.0,offday=0,offprice=0.0,pandl=0.0,curpice=0.0,commiss=0.0);
        curtrade<-list(symbol="",Quantity=0,tradeprice=0.0,activityday=0,commission=0,)
        #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount	CurrencyCode	Commission	Description	Order ID

        
        
        df.t <- dfr%>%
                 dplyr::mutate(pgrat = f.safeDiv(nop, lag(nop)),
                 curpos = 0, posage = 0, closedpos = 0, ror = 0)
        #df.t[is.na(df.t)] <- 0;
        df.t$index <- seq_len(nrow(df.t));
        initpos <- CASH;
        N <- nrow(df.t);
        v.posage <- v.closedpos <- v.curpos <- numeric(N);
        v.close <- df.t$close
        v.shares <- numeric(N);
        #v.trades <- numeric(N);
        v.holding <- numeric(N);
        v.pon <- numeric(N);
        v.nopen <- df.t$nop;
        #reduce the data frame to only those records where a trade is initiated
        #df.t<-filter(df.t, Trades != 0);
        startpos <- 0;
        #Do the sequential accounting by stepping through the data loop
        #v.trades<-df.t$Trades;
        t <- 0;
        t<-15
        isShort<-isLong<-FALSE;
        isFLAT<-TRUE;
        curpos<-0
        while(t<N){
            t<-t+1;
            if(curpos!=0){
                
                
            signal<-v.trades[t];
            if{(signal!=0)}
            {
                
            }
            
        }
        
        
        
        while (t < N) {
            t <- t + 1;
            #cat(t,v.shares[t],v.holding[t],v.posage[],v.pon[t])
            if (t > 1) {
                v.shares[t] <- v.shares[t - 1];
                v.pon[t] <- v.pon[t - 1]
                v.curpos[t] <- v.shares[t] * (v.close[t]);
                v.closedpos[t] <- v.closedpos[t - 1];
            }
            cat("t=",t,v.shares[t],v.pon[t],v.curpos[t],v.closedpos[t],"\n");
            if (v.shares[t] != 0) {
                v.holding[t] <- 1;
                v.posage[t] <- v.posage[t - 1] + 1;
                
                if ((v.posage[t] >= 504) || (t == N) ||
                    (v.curpos[t] >= 1.5 * initpos)) {
                    v.closedpos[t] = v.closedpos[t] + v.curpos[t] - startpos;
                    v.posage[t] <- 0;
                    v.curpos[t] <- 0;
                    startpos <- 0;
                    v.pon[t] <- v.nopen[t]
                    v.shares[t] <- 0;
                }
            }
            # test for being flat
            if (v.curpos[t] == 0) {
                if (t != N) {
                    if (v.trades[t]!=0) {
                        v.shares[t] <- floor(initpos / v.nopen[t]);
                        #v.trades[t] <- 1;
                        v.pon[t] <- v.nopen[t];
                        
                        v.curpos[t] <- v.shares[t] * v.nopen[t];
                        
                        startpos <- v.curpos[t];
                        cat("---------EVENT--------",
                              t,df.t$date[t],v.trades[t], v.shares[t],"@",v.pon[t],"\n");
                    }
                }
            }
            #cat(t,v.date[t],"Shares=",v.shares[t],v.close[t],
            #        v.curpos[t],v.closedpos[t],"\n");    
        }
        profit <- tail(v.closedpos, n = 1);
        investment <- initpos * sum(v.holding) / 252.0;
        ror <- profit / investment;
        v.ror <- f.roc(v.curpos);
        df.t$shares <- v.shares;
        df.t$curpos <- v.curpos;
        df.t$closedpos <- v.closedpos;
        df.t$trades <- v.trades;
        df.t$holding <- v.holding;
        df.t$posage <- v.posage;
        df.t$ror <- f.roc(v.curpos);
        return(df.t)
    }
    #end dtp
