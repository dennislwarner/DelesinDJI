f.initTradeRecord<-function(symb){
    #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	
    #UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount
    #CurrencyCode	Commission	Description	Order ID
    l.trec<-list(Account_Number=123456,Type="",TradeInd="T",Transaction="Buy/Sell",Quantity=0,
                 Cusip="11111t='",Symbol=symb,CallPut="",underlyingSymbol="",ExpireDate="",
                 StrikePrice="",TD="",SD="",ActivityDate="",price=0.0,Amount=0.0,
                 CurrencyCode="USD",Commission="5.00",Description="Apple Systems",Order_ID=12345);
    return(l.trec);
}
f.initPosition<-function(symb,cashin){
    l.pos<-list(Symbol=symb,shares=0,dayon=0,priceon=0,curprice=0,pandl=0,balannce=0.0,Commissions=0.0,
                 dayoff =0,age=0,Cash=cashin);
    return(l.pos);
}
f.closepos<-function(curpos,newprice,t){
    curpos$dayoff<-t;
    curpos$priceoff<-newprice
    curpos$Commissions<-curpos$Commissions+1;
    curpos$pandl<-curpos$shares*(newprice-curpos$priceon);
    curpos$Cash<-curpos$shares*curpos$priceoff;
    curpos$Balance<-curpos$shares*newprice;
    return(curpos)
}
f.openpos<-function(curpos,newps,newprice,t){
    curpos$shares<-newps*floor(curpos$Cash/newprice)
    curpos$priceon<-curpos$curprice<-newprice;
    curpos$dayon<-t;
    curpos$Commissions<-5;
    
    curpos$Cash<-curpos$Cash-abs((curpos$shares*newprice));
    return(curpos)
}
f.markToMarket<-function(curpos,newprice){
    curpos$age<-curpos$age+1;
    curpos$priceoff<-newprice;
    curpos$pandl<-curpos$shares*(newprice-curpos$priceon);
    return(curpos)
}
f.TradeSim<- function(df.xts,v.signals,symb,cname,CASH) {
    #df.xts  columns Date.Open,High,Low,Close,Volume,Trades
    #augment the data
        v.nop               <-c(coredata(df.xts$Open)[-1],last(df.xts$Open))
        df.xts$nop         <-v.nop;
        df.xts$chnop       <-c(0,diff(df.xts$nop,na.pad=FALSE));
        dfr<-f.xts2df(df.xts)
        names(dfr)
        dfr$signals<-v.signals;
        
        curpos<-f.initPosition(symb,CASH);
        curtrade<-f.initTradeRecord(symb);
        
        #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount	CurrencyCode	Commission	Description	Order ID
#------------Loop through periods
        t<-15;
        N<-nrow(dfr);
        while(t<N){
            t<-t+1;
            #mark current position to market
            #first accomodate splits,adjust the current position to hold the new amount of shares
            if(dfr$Split[t]!=1){curpos$shares<-curpos$shares*dfr$split[t]}
            if(curpos$shares!=0){curpos<-f.markToMarket(curpos,dfr$nop[t])}
            #if current position <> signals
            # are we long?
            newps<-dfr$signals[t];
            curps<-sign(curpos$shares);
            if(newps==0){
                if(curps!=0){
                    #close position
                }
            }else{
                if(curps==0){
                    newprice<-dfr$nop[t];
                    curpos<-f.openpos(curpos,newps,newprice,t)
                }else{
                    #close position
                    #open position
                }
            }
           
            
            cat(t,"\n");
        }
        
        
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
            if(signal!=0)
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
