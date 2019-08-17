f.initTradeRecord<-function(sym,cname){
    #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	
    #UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount
    #CurrencyCode	Commission	Description	Order ID
    df.traderecord<-data.frame(matrix(0,1,18));
    names(df.traderecord)<-c(   "Account_Number",
                                "Type",
                                "TradeInd",
                                "Transaction",
                                "Quantity",
                                "Cusip","Symbol","CallPut","underlyingSymbol",
                                "ExpireDate","Strike_Price","TD","ActivityDate",
                                "Amount","CurrencyCode","Commission","Description","Order_ID");
    df.traderecord$Account_Number    <-"123456";
    df.traderecord$Type              <-"";
    df.traderecordTradeInd           <-"T";
    df.traderecord$Transaction       <-"Buy/Sell";
    df.traderecord$Quantity          <-0;
    df.traderecord$Cusip             <-"11111t";
    df.traderecord$Symbol            <-"AAPL";
    df.traderecord$CallPut           <-"";
    df.traderecord$underlyingSymbol  <-"";
    df.traderecord$ExpireDate        <-"";
    df.traderecord$Strike_Price      <-"";
    df.traderecord$TD                <-"";
    df.traderecord$SD                <-"";
    df.traderecord$ActivityDate      <-"";
    df.traderecord$price             <-0.0;
    df.traderecord$Amount            <-0.0;
    df.traderecord$CurrencyCode      <-"USD";
    df.traderecord$Commission        <-"5.00";
    df.traderecord$Description       <-cname;
    df.traderecord$Order_ID          <-12345;
    return(df.traderecord);
}
f.initPosition<-function(symb,cashin){
    df.position<-data.frame(matrix(0,1,12))
    names(df.position)<-c("symbol","position","shares","dayon","priceon","curprice","pandl","val","commissions","dayoff","age","Cash");
    df.position$symbol<-symb;
    df.position$Cash<-cashin;
    return(df.position);
   
}
f.markToMarket<-function(df.position,curprice,t){
    df<-df.position;
    df$val           <- df$shares*(curprice);
    df$pandl         <- df$position*df$shares*(curprice-df$priceon)
    df$curprice      <- curprice;
    df$age           <- t-df$dayon
    df$commissions   <- df$commissions+5;
    return(df);
}
f.closePosition<-function(df.position,curprice,t){
    df.position$dayoff<-t;
    df.position$Cash<-df.position$Cash+df.position$pandl-df.position$commissions;
    
    df.position$shares<-0;
    df.position$position<-0;
    df.position$pandl<-0;
    return(df.position);
}
f.makeTrade<-function(df.traderecord,symb,adate,desiredpos,shares,curprice,cname,idcode){
    df.traderecord$Symbol        <- symb;
    df.traderecord$ActivityDate  <- adate;
    if(desiredpos<0){
            df.traderecord$Type<-"Short"
        }else{
            df.traderecord$Type<-"Margin"
        }
    df.traderecord$Quantity    <- shares;
    df.traderecord$price       <- curprice;
    df.traderecord$Amount      <- shares*curprice;
    df.traderecord$Commission  <- 5;
    return(df.traderecord); 
}
f.openPosition<-function(df.position,desiredpos,curprice,t,Amt,adate){
    df          <- df.position;
    df$position <- desiredpos;
    df$Cash     <- Amt;
    #how many shares to buy or sell?
    df$dayon    <- t;
    df$priceon  <- curprice;
    df$curprice <- curprice;
    df$shares   <- floor(df$Cash/df$priceon);
    #f.makeTrade<-function(df.traderecord,symb,adate,desiredpos,shares,curprice,cname,idcode){
    idcode<-t+iticker;
    
    df$commissions<-5;
    df$Cash<-df$Cash - df$shares * curprice;
    df.position<-df;
    
    return(df.position);
}

f.TradeSim<- function(df.xts,v.signals,symb,cname,CASH) {
    #df.xts  columns Date.Open,High,Low,Close,Volume,Trades
    #augment the data
        v.nop               <-c(coredata(df.xts$Open)[-1],last(df.xts$Open))
        df.xts$nop         <-v.nop;
        df.xts$chnop       <-c(0,diff(df.xts$nop,na.pad=FALSE));
        dfr         <- f.xts2df(df.xts)
        names(dfr)
        dfr$signals     <- v.signals;
        l.positions     <- list();
        l.trades        <- list();
        df.position     <- f.initPosition(symb,100000);
        df.traderecord  <- f.initTradeRecord(symb,cname);
        numtrades       <- 0;
        numpositions    <- 0;
        t<-0; N<-nrow(dfr); CumulativeProfits<-0;
        firstpos    <- min(which(v.signals!=0));
        t           <- firstpos-1;
        while(t<N){
            #---New day,   Gather the variables needed
            t          <- t+1;
            adate      <- dfr$Date[t];
            desiredpos <- dfr$signals[t];
            currentpos <- df.position$position;
            curprice   <- dfr$nop[t];
            #  If holding a current position mark it to market
            if(currentpos!=0){
                
                df.position<-f.markToMarket(df.position,curprice,t)
                numpositions<-numpositions+1;
            };
            #  Should the position be changed?----------------------------------------------------------------
            if(desiredpos !=currentpos){
                cat("Trade ",t,as.character(adate),currentpos,desiredpos,"\n");
                #----if there is an open position close it out---------
                if(currentpos!=0){
                    
                    CumulativeProfits<-CumulativeProfits+df.position$pandl;
                   cat("Closing Position",as.character(adate),currentpos,df.position$pandl,t,CumulativeProfits,"\n");
                    df.traderecord<-f.makeTrade(df.traderecord,symb,adate,desiredpos,df.position$shares,curprice,cname,t+iticker);
                    numtrades<-numtrades+1;
                    l.trades[[numtrades]]<-df.traderecord;
                    df.position<-f.closePosition(df.position,curprice,t);
                    
                }
                # open the new position------------------------------------------------------------------------
                if( desiredpos!=0){
                    #open pos
                    Amt<-100000;
                    df.position           <- f.openPosition(df.position ,desiredpos,curprice,t,100000,adate);
                    shares<-df$shares;
                    
                    df.traderecord        <- f.makeTrade(df.traderecord,symb,adate,desiredpos,df.position$shares,curprice,cname,t+iticker);
                    numtrades             <- numtrades+1;
                    l.trades[[numtrades]] <- df.traderecord;
                    
                    cat("Opening Position",desiredpos,"Shares ",df.position$shares, "at",curprice,t,"\n");
                }
            }
            if(numpositions>0){
                l.positions[[numpositions]]<-df.position;
            }
            
            #cat(t,as.Date(dfr$Date[t]),df.position$position," Shares ",df.position$shares," PandL ",df.position$pandl,"CumProf=",CumulativeProfits,"\n");
        }
        #The trading day loop has ended.  Close up the ledger for this stock
        if(currentpos!=0){
            CumulativeProfits<-CumulativeProfits+df.position$pandl;
            #cat("Closing Position",currentpos,df.position$pandl,t,CumulativeProfits,"\n");
            df.traderecord<-f.makeTrade(df.traderecord,symb,adate,desiredpos,df.position$shares,curprice,cname,t+iticker);
            numtrades<-numtrades+1;
            l.trades[[numtrades]]<-df.traderecord;
            df.position<-f.closePosition(df.position,curprice,t);
            l.positions[[numpositions]]<-df.position
        }
        df.positionHistory<-list_df2df(l.positions);
        df.tradeHistory<-list_df2df(l.trades);
        #examine the trae history
        write.csv(df.positionHistory,"PositionHistory.csv")
        write.csv(df.tradeHistory,"TradeHistory.csv");
        #cat("Closing Position",currentpos,df.position$pandl,t,CumulativeProfits,"\n");
        l.Results<-list(Trades=df.tradeHistory,Positions=df.positionHistory,CumulativeProfits=CumulativeProfits);
        return(l.Results);
        
}           
        
        
        #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount	CurrencyCode	Commission	Description	Order ID

        
        
    #     df.t <- dfr%>%
    #              dplyr::mutate(pgrat = f.safeDiv(nop, lag(nop)),
    #              df.position = 0, posage = 0, closedpos = 0, ror = 0)
    #     #df.t[is.na(df.t)] <- 0;
    #     df.t$index <- seq_len(nrow(df.t));
    #     initpos <- CASH;
    #     N <- nrow(df.t);
    #     v.posage <- v.closedpos <- v.df.position <- numeric(N);
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
    #     df.position<-0
    #     while(t<N){
    #         t<-t+1;
    #         if(df.position!=0){
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
    #             v.df.position[t] <- v.shares[t] * (v.close[t]);
    #             v.closedpos[t] <- v.closedpos[t - 1];
    #         }
    #         cat("t=",t,v.shares[t],v.pon[t],v.df.position[t],v.closedpos[t],"\n");
    #         if (v.shares[t] != 0) {
    #             v.holding[t] <- 1;
    #             v.posage[t] <- v.posage[t - 1] + 1;
    #             
    #             if ((v.posage[t] >= 504) || (t == N) ||
    #                 (v.df.position[t] >= 1.5 * initpos)) {
    #                 v.closedpos[t] = v.closedpos[t] + v.df.position[t] - startpos;
    #                 v.posage[t] <- 0;
    #                 v.df.position[t] <- 0;
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
