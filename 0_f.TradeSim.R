f.initTradeRecord<-function(symb){
    #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	
    #UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount
    #CurrencyCode	Commission	Description	Order ID
    l.trec<-list(Account_Number=123456,Type="",TradeInd="T",Transaction="Buy/Sell",Quantity=0,
                 Cusip="11111t='",Symbol="AAPL",CallPut="",underlyingSymbol="",ExpireDate="",
                 Strike21qPrice="",TD="",SD="",ActivityDate="",price=0.0,Amount=0.0,
                 CurrencyCode="USD",Commission="5.00",Description="Apple Systems",Order_ID=12345);
    return(l.trec);
}
f.initPosition<-function(symb,cashin){
    l.pos<-list(Symbol=symb,position=0,shares=0,dayon=0,priceon=0,curprice=0,pandl=0,
                val=0,Commissions=0.0,
                 dayoff =0,age=0,Cash=cashin)
}
f.markToMarket<-function(curpos,curprice,t){
    vpos               <- curpos;
    vpos$val           <- vpos$shares*(curprice);
    vpos$pandl         <- vpos$position*vpos$shares*(curprice-vpos$priceon)
    vpos$curprice      <- curprice;
    vpos$age           <- t-vpos$dayon
    vpos$commissions   <- vpos$commissions+5;
    return(vpos);
}
f.closePosition<-function(curpos,curprice,t){
    curpos$dayoff<-t;
    curpos$cash<-curpos$cash+curpos$pandl-curpos$commissions;
    curpos$shares<-0;
    curpos$position<-0;
    curpos$pandl<-0;
    return(curpos);
}
f.makeTrade<-function(curtrade,symb,adate,desiredpos,shares,curprice,cname,idcode){
    curtrade$Symbol<-symb;
    curtrade$ActivityDate<-adate;
    if(desiredpos<0){curtrade$Type<-"Short"}else{curtrade$Type<-"Margin"};
    curtrade$Quantity<-shares;
    curtrade$price<-curprice;
    curtrade$amount<-shares*curprice;
    curtrade$Commission<-5;
    
}
f.openPosition<-function(curpos,desiredpos,curprice,t,Amt){
    vpos<-curpos;
    vpos$position <- desiredpos;
    vpos$Cash<-Amt;
    #how many shares to buy or sell?
    vpos$dayon<-t;
    vpos$priceon<-curprice;
    vpos$shares<-floor(curpos$Cash/curprice);
    f.makeTrade(curtrade,symb,adate,desiredpos,shares,curprice,cname,t+iticker)
    vpos$Commissions<-5;
    vpos$Cash<-vpos$Cash - vpos$shares * curprice;
    return(vpos);
}
f.makeTrade()
f.TradeSim<- function(df.xts,v.signals,symb,cname,CASH) {
    #df.xts  columns Date.Open,High,Low,Close,Volume,Trades
    #augment the data
        v.nop               <-c(coredata(df.xts$Open)[-1],last(df.xts$Open))
        df.xts$nop         <-v.nop;
        df.xts$chnop       <-c(0,diff(df.xts$nop,na.pad=FALSE));
        dfr<-f.xts2df(df.xts)
        names(dfr)
        dfr$signals<-v.signals;
        
        curpos<-f.initPosition(symb,100000);
        curtrade<-f.initTradeRecord();
     
        t<-15;N<-nrow(dfr);
        CumulativeProfits<-0;
        while(t<N){
            t<-t+1;
            desiredpos<-dfr$signals[t];
            currentpos<-curpos$position;
            curprice<-dfr$nop[t];
            if(currentpos!=0){
                
                curpos<-f.markToMarket(curpos,curprice,t)
            };
            
            if(desiredpos!=currentpos){
                cat("Trade ",currentpos,desiredpos,t,"\n")
                if(currentpos!=0){
                    
                    CumulativeProfits<-CumulativeProfits+curpos$pandl;
                    cat("Closing Position",currentpos,t,CumulativeProfits,"\n");
                    curpos<-f.closePosition(curpos,curprice,t);
                    
                }
                if(desiredpos!=0){
                    #open pos
                    curpos<-f.openPosition(curpos,desiredpos,curprice,t,100000);
                    cat("Opening Position",desiredpos, "at",curprice,t,"\n");
                }
            }
            cat(t,as.Date(dfr$Date[t]),curpos$position," Shares ",curpos$shares," PandL ",curpos$pandl,"CumProf=",CumulativeProfits,"\n");
        }
}           
        
        
        #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount	CurrencyCode	Commission	Description	Order ID

        
        
    #     df.t <- dfr%>%
    #              dplyr::mutate(pgrat = f.safeDiv(nop, lag(nop)),
    #              curpos = 0, posage = 0, closedpos = 0, ror = 0)
    #     #df.t[is.na(df.t)] <- 0;
    #     df.t$index <- seq_len(nrow(df.t));
    #     initpos <- CASH;
    #     N <- nrow(df.t);
    #     v.posage <- v.closedpos <- v.curpos <- numeric(N);
    #     v.close <- df.t$close
    #     v.shares <- numeric(N);
    #     #v.trades <- numeric(N);
    #     v.holding <- numeric(N);
    #     v.pon <- numeric(N);
    #     v.nopen <- df.t$nop;
    #     #reduce the data frame to only those records where a trade is initiated
    #     #df.t<-filter(df.t, Trades != 0);
    #     startpos <- 0;
    #     #Do the sequential accounting by stepping through the data loop
    #     #v.trades<-df.t$Trades;
    #     t <- 0;
    #     t<-15
    #     isShort<-isLong<-FALSE;
    #     isFLAT<-TRUE;
    #     curpos<-0
    #     while(t<N){
    #         t<-t+1;
    #         if(curpos!=0){
    #             
    #             
    #         signal<-v.trades[t];
    #         if{(signal!=0)}
    #         {
    #             
    #         }
    #         
    #     }
    #     
    #     
    #     
    #     while (t < N) {
    #         t <- t + 1;
    #         #cat(t,v.shares[t],v.holding[t],v.posage[],v.pon[t])
    #         if (t > 1) {
    #             v.shares[t] <- v.shares[t - 1];
    #             v.pon[t] <- v.pon[t - 1]
    #             v.curpos[t] <- v.shares[t] * (v.close[t]);
    #             v.closedpos[t] <- v.closedpos[t - 1];
    #         }
    #         cat("t=",t,v.shares[t],v.pon[t],v.curpos[t],v.closedpos[t],"\n");
    #         if (v.shares[t] != 0) {
    #             v.holding[t] <- 1;
    #             v.posage[t] <- v.posage[t - 1] + 1;
    #             
    #             if ((v.posage[t] >= 504) || (t == N) ||
    #                 (v.curpos[t] >= 1.5 * initpos)) {
    #                 v.closedpos[t] = v.closedpos[t] + v.curpos[t] - startpos;
    #                 v.posage[t] <- 0;
    #                 v.curpos[t] <- 0;
    #                 startpos <- 0;
    #                 v.pon[t] <- v.nopen[t]
    #                 v.shares[t] <- 0;
    #             }
    #         }
    #         # test for being flat
    #         if (v.curpos[t] == 0) {
    #             if (t != N) {
    #                 if (v.trades[t]!=0) {
    #                     v.shares[t] <- floor(initpos / v.nopen[t]);
    #                     #v.trades[t] <- 1;
    #                     v.pon[t] <- v.nopen[t];
    #                     
    #                     v.curpos[t] <- v.shares[t] * v.nopen[t];
    #                     
    #                     startpos <- v.curpos[t];
    #                     cat("---------EVENT--------",
    #                           t,df.t$date[t],v.trades[t], v.shares[t],"@",v.pon[t],"\n");
    #                 }
    #             }
    #         }
    #         #cat(t,v.date[t],"Shares=",v.shares[t],v.close[t],
    #         #        v.curpos[t],v.closedpos[t],"\n");    
    #     }
    #     profit <- tail(v.closedpos, n = 1);
    #     investment <- initpos * sum(v.holding) / 252.0;
    #     ror <- profit / investment;
    #     v.ror <- f.roc(v.curpos);
    #     df.t$shares <- v.shares;
    #     df.t$curpos <- v.curpos;
    #     df.t$closedpos <- v.closedpos;
    #     df.t$trades <- v.trades;
    #     df.t$holding <- v.holding;
    #     df.t$posage <- v.posage;
    #     df.t$ror <- f.roc(v.curpos);
    #     return(df.t)
    # }
    # #end dtp
