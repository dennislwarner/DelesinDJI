f.lagfill <- function(x) {
    xlag <- stats::lag(x,k=-11)
    
    xlag[1] <- x[1]
    
    return(xlag)
}
f.myrank <- function(x) {
    y <- t(rank(x, na.last = NA))
}
f.deriveNRor<-function(df.xts,cname){
    df.xts$nop<-na.locf(lag.xts(df.xts$Adj_Open,k=-1));
    df.xts$rornop<-na.locf(log(lag.xts(df.xts$nop,k=-1)/df.xts$nop));
    v.NRor.xts<-df.xts$rornop;
    names(v.NRor.xts)<-cname;
    return(v.NRor.xts);
}
f.deriveCloseRor<-function(df.xts,cname){
    #the ror of the daily adjusted closing prices
    df.xts$Clag<-lag.xts(df.xts$Adj_Close,k=1);
    df.xts$Cror<-log(df.xts$Adj_Close/df.xts$Clag);
    df.xts[is.na(df.xts)]<-0;
    df.xts$CumCror<-cumsum(df.xts$Cror);
    names(df.xts)[ncol(df.xts)]<-cname;
    v.CumCror.xts<-df.xts[,ncol(df.xts)];
    names(v.CumCror.xts)<-cname;
    return(v.CumCror.xts);
}
f.derivedEtfRor<-function(df.etf.xts,cname){
    df.etf.xts$clag<-lag.xts(df.etf.xts$Adj_Close,k=1);
    df.etf.xts$ror<-log(df.etf.xts$Adj_Close/df.etf.xts$clag);
    df.etf.xts[is.na(df.etf.xts)]<-0;
    v.etfror.xts<-df.etf.xts$ror;
    names(v.etfror.xts)<-cname
    return(v.etfror.xts)
}
f.bridge<-function(x){
    nt<-sum(abs(diff(x)));  # of trades entering;
    n<-length(x);
    for(t in 3:length(x)){
        if(x[t-2]==1){
            if(x[t-1]==0){
                if(x[t]==1){
                    x[t-1]<-1;
                    # cat(t,x[t-2],x[t-1],x[t],"\n");
                }
            }
        }else{
            if(x[t-2]==(-1)){
                if(x[t-1]==0){
                    if(x[t]==(-1)){
                        x[t-1]<-(-1);
                        #cat(t,x[t-2],x[t-1],x[t],"\n");
                    }
                }
            }
            #cat(t,"\n")
        }
    }
    return(x);
}
f.delay<-function(x){
    nt<-sum(abs(diff(x)));  # of trades entering;
    n<-length(x);
    yage<-0*x;
    yage[1]<-1
    for(t in 2:length(x)){
        if(x[t]==x[t-1]){
            yage[t]<-yage[t-1]+1;
        }else{
            yage[t]<-1;
        }
    }
    for(t in 2:length(x)){
        if(yage[t]<2){
            x[t]<-x[t-1]
        }
    }       
    
    return(x);
}     
f.initTrades<-function(){
    df.trades<-data.frame(matrix(0,0,20));
    names(df.trades)<-c(   "Account_Number",
                           "Type",
                           "TradeInd",
                           "Transaction",
                           "Quantity",
                           "Cusip","Symbol","CallPut","underlyingSymbol",
                           "ExpireDate","Strike_Price","TD","SD","ActivityDate","price",
                           "Amount","CurrencyCode","Commission","Description","Order_ID");
    return(df.trades);
}
f.initTranches<-function(){
    df.tranches<-as.data.frame(matrix(0,0,5));
    names(df.tranches)<-c("Symbol","OpenDate","onPrice","Shares","RemainingShares");
    return(df.tranches)
}
f.initRealizations<-function(){
    df.realizations<-as.data.frame(matrix(0,0,7));
    names(df.realizations<-c("Symbol","OnDate","OnPrice","OffDate","OffPrice","PandL","Age"));
    return(df.realizations);
}
f.initPositions<-function(symb,N){
    df.position<-data.frame(matrix(0,N,12))
    names(df.position)<-c("Date","symbol","position","quantity","AvePrice","Last","OpenPandL","OpenPerShare","OpenPLpct","TotalCost","MarketValue","Acct");
    df.position$symbol<-symb;
    # df.position$Cash<-cashin;
    return(df.position);
    
}
f.traderecstart<-function(sym,cname){
    #Account Number	Type	TradeInd	Transaction	Quantity	Cusip	Symbol	CallPut	
    #UnderlyingSymbol	ExpireDate	StrikePrice	TD	SD	Activity Date	Price	Amount
    #CurrencyCode	Commission	Description	Order ID
    df.traderecord<-data.frame(matrix(0,1,20));
    names(df.traderecord)<-c(   "Account_Number",
                                "Type",
                                "TradeInd",
                                "Transaction",
                                "Quantity",
                                "Cusip","Symbol","CallPut","underlyingSymbol",
                                "ExpireDate","Strike_Price","TD","SD","ActivityDate","price",
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
f.makeTrade<-function(df.trades,symb,adate,sharestotrade,desiredshares,curprice,cname,idcode){
    #f.makeTrade(df.traderecord,symb,adate,sharestotrade,desiredshares,curprice,cname,t+iticker);
    # add a record to the current df.trades
    df.trades[nrow(df.trades)+1,]<-f.traderecstart(symb,cname);
    
    
    N                          <- nrow(df.trades);
    df.trades$Symbol[N]        <- symb;
    df.trades$ActivityDate[N]  <- as.character(as.Date(adate));
    
    startingshares<-desiredshares-sharestotrade;
    if((startingshares<0)||(desiredshares<0)){
        df.trades$Type[N]<-"Short"
    }else{
        df.trades$Type[N]<-"Margin"
    }
   # df.trades$Type[N]          <- ifelse(desiredshares>=0,"Margin","Short");
    df.trades$Transaction[N]   <-ifelse(sharestotrade>0,"Buy","Sell");
    
    df.trades$Quantity[N]      <- abs(sharestotrade);
    df.trades$price[N]         <- curprice;
    df.trades$Amount[N]        <- abs(sharestotrade)*curprice;
    df.trades$Commission[N]    <- 5;
    df.trades$Order_ID[N]      <- idcode;
    return(df.trades );   #return to f.tradesim
}
f.TradeSim<- function(df.xts,v.signals,symb,cname,CASH) {
    #called from f.execute trades
    #df.xts  columns Date.Open,High,Low,Close,Volume,Trades
    #augment the data
    #build return structures------------------------
    #initialize the accounting bundle for this stock
    v.signals[!is.finite(v.signals)] <-0;
    df.trades       <- f.initTrades();
    # df.tranches     <- f.initTranches();
    # df.realizations <- f.initRealizations();
    N<-nrow(df.xts)
    df.positions    <- f.initPositions(symb,N);
    #l.Z<-list(df.trades=df.trades,df.tranches=df.tranches,df.realizations=df.realizations);
    #----
    v.nop               <-c(coredata(df.xts$Open)[-1],last(df.xts$Open))
    df.xts$nop         <-v.nop;
    df.xts$chnop       <-c(0,diff(df.xts$nop,na.pad=FALSE));
    dfr         <- f.xts2df(df.xts)
    names(dfr)
    dfr$signals     <- v.signals;
    
    numtrades       <- 0;
    numpositions    <- 0;
    t<-0;
    N<-nrow(dfr); 
    CumulativeProfits<-0;
    firstpos    <- min(which(v.signals!=0));
    t           <- firstpos-1;
    #  Create all the transanctions first
    curshares<-0;
    t<-0;
    while(t<N){
        t<-t+1;
        adate      <- dfr$Date[t];
        desiredpos <- dfr$signals[t];
        curprice   <- curprice   <- dfr$nop[t];
        curshares<-curshares*dfr$Split[t];      #account for splits
        desiredvalue<-desiredpos*CASH;
        curvalue<-curprice*curshares;
        desiredshares<-floor(desiredvalue/curprice);
        #check for reversals between long and short, in which cases we must make two transactions
        valuetotrade<-desiredvalue-curvalue;
        if(abs(valuetotrade)>(.20*CASH)){
            if((sign(curshares)*sign(desiredshares))<0){
                # its a reversal, break into two transactions
                valuetotrade<--curvalue;
                # Close out the existing position-----
                startshares<-curshares;
                df.trades  <- f.makeTrade(df.trades,symb,adate,
                                          sharestotrade = -curshares,
                                          desiredshares=0,curprice,cname,t+iticker);
                
                sharestotrade<--curshares;
                endshares<-0;
                cat(t,as.character(as.Date(adate)),startshares,endshares,"\n")
                #  now establish the desired position with a second transaction.
                startshares<-0;
                endshares<-desiredshares;
                df.trades  <- f.makeTrade(df.trades,symb,adate,
                                          sharestotrade = desiredshares,
                                          desiredshares=desiredshares,curprice,cname,t+iticker);
                valuetotrade<-desiredvalue;
                cat(t,as.character(as.Date(adate)),startshares,endshares,"\n");
                curshares<-desiredshares;
                #f.makeTrade<-function(df.trades,symb,adate,sharestotrade,desiredshares,curprice,cname,idcode){
            }else{
                startshares<-curshares;
                sharestotrade  <- desiredshares-curshares;
                df.trades      <- f.makeTrade(df.trades,symb,adate,sharestotrade,desiredshares,curprice,cname,t+iticker);
                curshares      <- curshares+sharestotrade;
                endshares<-curshares;
                cat(t,as.character(as.Date(adate)),startshares,endshares,"\n");
            }
             
        }
        
    }
        
    cat((nrow(dfr)/nrow(df.trades)),"\n");
    return(df.trades);
}
    
    
    
#     
#     
#     
#     
#     
#     
#     
#     
#     while(t<N){
#         #---New day,   Gather the variables needed
#         t          <- t+1;
#         
#         currentpos <- df.position$position;
#         
#         #  If holding a current position mark it to market
#         if(df.position$shares!=0){
#             df.position<-f.markToMarket(df.position,curprice,t);
#         }
#         #Transactions are determined by the difference between desired shares and current shares
#         currentshares<-df.position$shares;
#         
#         
#        
#             #adjust the cumulative position for this stock
#             df.position   <-f.adjustPosition(df.position,df.traderecord);
#             f.adjustPosition<-function(df.position,df.traderecord){
#                 
#             }
#         }
#         
#         
#         
#     }
#     #----if there is an open position close it out---------
#     if(currentshares!=0){
#         
#         CumulativeProfits<-CumulativeProfits+df.position$pandl;
#         cat("Closing Position",as.character(adate),currentshares,df.position$pandl,t,CumulativeProfits,"\n");
#         df.traderecord<-
#             numtrades<-numtrades+1;
#         l.trades[[numtrades]]<-df.traderecord;
#         df.position<-f.closePosition(df.position,curprice,t);
#         
#     }
#     # open the new position------------------------------------------------------------------------
#     if( desiredshares!=0){
#         #open pos
#         
#         df.position           <- f.openPosition(df.position ,desiredshares,curprice,t,CASH,adate);
#         shares<-df.position$shares;
#         
#         df.traderecord        <- f.makeTrade(df.traderecord,symb,adate,desiredshares,df.position$shares,curprice,cname,t+iticker);
#         numtrades             <- numtrades+1;
#         l.trades[[numtrades]] <- df.traderecord;
#         
#         cat("Opening Position",desiredshares, "at",curprice,t,"\n");
#     }
# }#end f.tradesim
#------------------------------------------------------------------------------------------------
# if(numpositions>0){
#     l.positions[[numpositions]]<-df.position;
# }
# 
# #cat(t,as.Date(dfr$Date[t]),df.position$position," Shares ",df.position$shares," PandL ",df.position$pandl,"CumProf=",CumulativeProfits,"\n");
# }
# #The trading day loop has ended.  Close up the ledger for this stock
# if(currentpos!=0){
#     CumulativeProfits<-CumulativeProfits+df.position$pandl;
#     #cat("Closing Position",currentpos,df.position$pandl,t,CumulativeProfits,"\n");
#     df.traderecord<-f.makeTrade(df.traderecord,symb,adate,desiredpos,df.position$shares,curprice,cname,t+iticker);
#     numtrades<-numtrades+1;
#     l.trades[[numtrades]]<-df.traderecord;
#     df.position<-f.closePosition(df.position,curprice,t);
#     l.positions[[numpositions]]<-df.position
# }
# df.positionHistory<-list_df2df(l.positions);
# df.tradeHistory<-list_df2df(l.trades);
# #examine the trae history
# write.csv(df.positionHistory,"PositionHistory.csv")
# write.csv(df.tradeHistory,"TradeHistory.csv");
# #cat("Closing Position",currentpos,df.position$pandl,t,CumulativeProfits,"\n");
# l.Results<-list(Trades=df.tradeHistory,Positions=df.positionHistory,CumulativeProfits=CumulativeProfits);
# return(l.Results);
# 
# }           
f.DistinctTrade <- function(l.Prices,df.etf.xts,v.windows) {
    
    #  1  df.Cumror.xts    cumulative adjusted close Rors
    #  2  df.Nror.xts      daily rates of return for next opening
    #  3  df.Ror.xts       daily adjusted close Rors
    #  4  m.beswtpos       matrix N x M  positions giving best results
    nstocks<-length(l.Prices);
    # initiate values of tracking structures
    v.ibestWindow       <- numeric(nstocks);
    v.bestsharpe        <- rep(-100,nstocks);
    m.bestpos           <-matrix(0,nrow(df.etf.xts),nstocks)
    #----Prepare the required data frames----
    df.xts    <- l.Prices[[1]];
    cname     <- names(l.Prices)[1];
    #add the next opening variable, derived as a "negative lag in the lag.xts function)
    df.CumRor.xts   <- f.deriveCloseRor(df.xts,cname);
    df.NRor.xts     <- f.deriveNRor(df.xts,cname);
    ico <- 1
    #Build a data frame of all cumulative rates of return for every stock
    while (ico < nstocks) {
        ico             <- ico + 1;
        symb            <- names(l.Prices)[[ico]];
        df.xts          <- l.Prices[[ico]];
        v.CumRor.xts    <- f.deriveCloseRor(df.xts,symb);
        df.CumRor.xts   <- merge.xts(df.CumRor.xts,v.CumRor.xts,join='left');
        v.NRor.xts      <- f.deriveNRor(df.xts,symb);
        df.NRor.xts     <- merge.xts(df.NRor.xts,v.NRor.xts);
    }
    #----Compute the daily ror for each stock
    df.Ror.xts    <- diff.xts(df.CumRor.xts);
    v.cnames      <- names(df.Ror.xts)
    etfname       <- names(df.etf.xts)[1];
    v.etfror.xts  <- f.derivedEtfRor(df.etf.xts,etfname);
    #----
    #illustrative
    #plot.xts(df.CumRor.xts[, 2:8])
    #assume risk free rate is .03
    Rfree      <- 0.03;
    RfreeDaily <- Rfree/252;
    #----Loop across the candidate data window lengths----
    #    
    iwindow <- 0;
    l.TradeRes<-list();
    #-------------
    #  Here calculate the best method for each stock
    while (iwindow < length(v.windows)) {
        iwindow <- iwindow + 1
        window <- v.windows[iwindow]
        cat(iwindow, window, "\n")
        #----For each stock Ror over a moving window compute its rank each day----
        dfkR.xts                  <- diff.xts(df.CumRor.xts, lag = window, na.pad = TRUE)
        dfkR.xts[is.na(dfkR.xts)] <- 0
        # find the cohort each stock lie in each period
        m.seg<-t(apply(dfkR.xts,1,function(x) cut(x,20,labels=FALSE)))
        #----
        m.signr<-sign(dfkR.xts)
        m.pos            <- 0 * m.seg;   
        m.pos[m.seg<4]   <- -1;
        m.pos[m.seg<2]   <- -2;
        m.pos[m.seg>9]   <-  1;
        m.pos[m.seg>14]  <-  2;
        m.pos[m.seg>16 ] <-  3;
        # x<-m.pos[,1]
        # sum(abs(diff(x)))
        # y<-f.delay(x)
        # sum(abs(diff(y)))
        # v.posy<-apply(m.pos,2 ,function(x) sum(abs(diff(x))))
        m.pos<-apply(m.pos,2,function(x) f.delay(x)); #delay changing position , wait for confirumation
        # v.posz<-apply(m.pos,2,function(x) sum(abs(diff(x))));
        # v.posy;
        # v.posz;
        # v.rs<-rowSums(abs(m.pos))
        #  m.signs <- sign(m.pos);
        #  mabs <- abs(m.pos)
        # m.w <- prop.table(mabs, 1)
        # m.weights <- m.signs * m.w
        m.nror <- coredata(df.NRor.xts)
        #m.prof <- m.weights * m.nror
        m.prof<-m.pos*m.nror
        m.prof[is.na(m.prof)] <- 0
        
        m.cumprof <- apply(m.prof, 2, cumsum)
        
        v.totalprof <- rowSums(m.cumprof)
        m.cumprof <- cbind(m.cumprof, v.totalprof)
        
        colnames(m.cumprof)[ncol(m.cumprof)] <- "Total"
        
        df.cumprof.xts <-
            as.xts(m.cumprof, order.by = index(df.NRor.xts))
        df.cumprof.xts$etf<-cumsum(v.etfror.xts);
        prof <- round(coredata(last(df.cumprof.xts$Total)), digits = 2)
        
        maintitle <-
            paste("W=",
                  window,
                  
                  sep = " ")
        plot.xts(df.cumprof.xts[,31:32], main = maintitle,legend.loc='topleft')
        df.prof.xts <- diff.xts(df.cumprof.xts)
        #build the performance analytics
        # charts.PerformanceSummary(df.prof.xts[,1:5],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
        # charts.PerformanceSummary(df.prof.xts[,6:10],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
        # charts.PerformanceSummary(df.prof.xts[,11:15],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
        # charts.PerformanceSummary(df.prof.xts[,16:20],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
        # charts.PerformanceSummary(df.prof.xts[,21:25],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
        # charts.PerformanceSummary(df.prof.xts[,26:30],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
        tabcapm<-table.CAPM(df.prof.xts[,1:30],Rb= v.etfror.xts,scale=252,Rf=RfreeDaily);
        tabstats<-table.AnnualizedReturns(df.prof.xts[,1:30],scale=252,Rf=RfreeDaily);
        #tabstats<-table.CalendarReturns(df.prof.xts[,31:32])
        idd<-paste("capm ",window,sep=" ")
        l.TradeRes[[idd]]<-tabstats;
        
        #consider performance of each stock
        istock<-0;
        while(istock<nstocks){
            istock<-istock+1;
            ret<-tabstats[1,istock];
            st<-tabstats[2,istock];
            trey<-tabcapm[12,istock];
            sharp<-tabstats[3,istock];
            prof <- round(coredata(last(df.cumprof.xts[,istock])), digits = 2);
            better<-sharp>v.bestsharpe[istock];
            #cat(istock,v.cnames[istock],sharp,v.bestsharpe[istock],better,prof,"\n");
            if(better){
                v.bestsharpe[istock]<-sharp;
                
                v.ibestWindow[istock]<-iwindow;
                m.bestpos[,istock]<-m.pos[,istock];
                cat(v.cnames[istock],istock,idd,prof,"Ann Ror=",ret,"Stdev=", st, "Sharpe = ",sharp,"Treynor=",trey,"\n")
            }
        }
        #---
        #tail(df.cumprof.xts)
        m.pos.xts<-as.xts(m.pos,order.by = index(df.NRor.xts));
        #m.weights.xts<-as.xts(m.weights,order.by = index(df.NRor.xts));
        m.prof.xts<-as.xts(m.prof,order.by = index(df.NRor.xts));
        m.seg.xts<-as.xts(m.seg,order.by = index(df.NRor.xts));
        dff.xts<-merge.xts(df.Ror.xts[,1],df.CumRor.xts[,1],m.seg.xts[,1],m.pos.xts[,1],df.NRor.xts[,1],m.prof.xts[,1],df.cumprof.xts[,1])
        names(dff.xts)<-c(cname,"CumRor","Seg","Pos","NRor","Prof","CumProf");
        #----
        
    }
    # The best windows have been determined for each stock
    #
    #m.signs <- sign(m.pos);
    # mabs <- abs(m.pos)
    # m.w <- prop.table(mabs, 1)
    # m.weights <- m.signs * m.w;
    m.nror <- coredata(df.NRor.xts)
    m.prof <- m.pos * m.nror
    
    m.prof[is.na(m.prof)] <- 0
    
    m.cumprof <- apply(m.prof, 2, cumsum)
    
    v.totalprof <- rowSums(m.cumprof)
    m.cumprof <- cbind(m.cumprof, v.totalprof)
    
    colnames(m.cumprof)[ncol(m.cumprof)] <- "Total"
    
    df.cumprof.xts <-
        as.xts(m.cumprof, order.by = index(df.NRor.xts))
    df.cumprof.xts$etf<-cumsum(v.etfror.xts);
    prof <-
        round(coredata(last(df.cumprof.xts$Total)), digits = 2)
    
    maintitle <-
        paste(
            "W=",
            window,
            
            sep = " ")
    plot.xts(df.cumprof.xts[,31:32], main = maintitle,legend.loc='topleft')
    return(m.pos)
}
f.derivePositions<-function(df.AllTrades,l.Prices){
    # construct position reports for each day given the trading record and the daily prices
    # there is a different position each day
    #create sorage structures
    l.DailyPositions           <- list();
    df.SymTrades               <- dplyr::arrange(df.AllTrades,Symbol,ActivityDate);
    df.AllTrades$Date          <- as.Date(df.AllTrades$ActivityDate);
    # df.SymTrades.xts<-f.df2xts(df.SymTrades)
    v.tickers<-sort(unique(df.SymTrades$Symbol));
    #loop through each ticker symbol, build a running position,  
    #for any day where there is a non-zero position,add it to the positions for that day
    
    iticker<-0;
    #   df.P.xts    Closing prices for this security
    #   df.T        transactions records for security
    #   df.Pos      working data frame containing daily positions and all trades aligned
    #
    #
    #----Loop through every day of positions
    #----the challenge comes when there is a reversal
    #table(df.SymTrades$ActivityDate)
    while(iticker<length(v.tickers)){
        iticker       <- iticker+1;
        symb          <- v.tickers[[iticker]];
        df.P.xts      <- l.Prices[[symb]];
        N             <- nrow(df.P.xts)
        df.T <- dplyr::filter(df.AllTrades,Symbol==symb)%>%dplyr::select(TradeType=Type,Transaction,TQ=Quantity,price,Amount,Date);
        fnout<-"df.T.csv";write.csv(df.T,file=fnout);
        df.tdates<-as.data.frame(table(df.T$Date));
        names(df.tdates)[1]<-"Date";
        df.tdates$Date<-as.Date(df.tdates$Date)
        v.seq<-seq_along(index(df.P.xts))
        df.td<-data.frame(Date=index(df.P.xts),v.seq);
        df.Trades<-merge(df.td,df.tdates,by="Date",all=TRUE);
        df.J<-merge(df.td,df.T,by="Date",all=TRUE);
        cat(iticker,symb,nrow(df.Trades),"\n");
        summary(df.Trades)
        #fnout<-"df.Trades.csv";write.csv(df.Trades,file=fnout)
        #initialize daily positions data frame for this symbol
        df.positions         <- f.initPositions(symb,N);
        df.positions$Date    <- index(df.P.xts);
        df.positions$Last    <- as.numeric(df.P.xts$Close);
        
        df.Pos              <- merge.data.frame(df.positions, df.T, by="Date", all=TRUE);
        df.Pos[is.na(df.Pos)]<-0
        fnout<-"df.Pos.csv";write.csv(df.Pos,file=fnout);
        #-----------------------------------------------------------------------------------------------------------
        #     Dexcription of df.Positions data frame
        #-----------------------------------------------------------------------------------------------------------
        #  Column             |    Long Positions    |   Short Positions |
        #                     |                      |                   |
        #    Date             |                      |                   |
        #   Symbol            |                      |                   |
        #   Position          |    Long              |     Short         |
        #   quantity          |    +20               |     -20           |
        #   AvePrice          |    8.50              |      8.50         |
        #   Last              |    11.50             |     11.50         |
        #   OpenPandL         |    60.00             |     -60.00        |
        #   OpenPerShare      |     3.00             |      -3.00        |
        #   OpenPLpct         |                      |                   |
        #   TotalCost         |    8.50 x 20 = 170   | 8.50 x -20 = -170 |                    |                   |
        #   MarketValue       |    11.50 x 20= 230   |11.50 x -20 = -230 |
        #   Acct
        #  
        # Columns in the df.Position data frames
        #loop through every row, updating the daiy positions for this stock
        #Quantity     # shares held,   positive for long positions, negative for Short
        #
        j<-0;
        curpos<-"";
        curcost<-0;
        curquantity<-0;
        realized<-0;
        while(j<nrow(df.Pos)){
            j<-j+1;
            curprice            <- df.Pos$Last[j];
            adate               <- as.character(df.Pos$Date[j]);
            ttype               <- df.Pos$TradeType[j];
            price               <- df.Pos$price[j];
            last                <- df.Pos$Last[j];
            pandl               <- 0;
            
            if(ttype != 0){
                curpos            <- ifelse(ttype=="Margin","Long","Short");
                tdirection        <- ifelse(df.Pos$Transaction[j]=="Buy",1,-1);
                quantity          <- tdirection * df.Pos$TQ[j];
                if(sign(curquantity)!=sign(quantity)){
                    pandl<-quantity*(last-aveprice)
                    realized <- realized+pandl;
                }
                curquantity       <- curquantity + quantity;
                cost              <- price * quantity;
                curcost           <- curcost + cost;
                aveprice          <- curcost/curquantity;
            }   
                
            marketvalue       <- last*curquantity;
            openpandl         <- marketvalue - curcost;
            openpandlpershare <- openpandl/curquantity;
            cat(j,adate,curpos,curquantity,aveprice,last,openpandl,openpandlpershare,curcost,marketvalue,curpos,pandl,realized,"\n");
            
        }
            
            
            
            df,Pos$quantity[j]  <= quantity;
            curcost
            # a transaction has occurred....update the position
            tq          <- df.Pos$TQ[j];        #quantity traded
            tp<-df.Pos$price[j];
            df.Pos$position[j]
            curpos<-curpos+tdirection*tq;
            curvalue<-curpos*curprice;
            tamt<-df.Pos$Amount[j];
            #----keep track of total cost of position
            ecost<-ifelse(j>1,df.Pos$TotalCost[j-1],0);#eexisting cost
            tcost<-ecost+tamt;
            avecost<-tcost/curpos;
            
            posdir<-sign(curpos);
            df.Pos$quantity[j]<-abs(curpos);
            df.Pos$position[j]<-ifelse(curpos>0,"Long","Short");
            df.Pos$MarketValue[j]<-posdir*df.Pos$Last[j]*df.Pos$quantity[j];
            
            df.Pos$AvePrice[j]<-avecost;
            df.Pos$TotalCost[j]<-tcost;
            df.Pos$OpenPandL[j]<-df.Pos$MarketValue[j]-df.Pos$TotalCost[j];
            df.Pos$OpenPerShare[j]<-df.Pos$OpenPandL[j]/abs(curpos)
            # }else{
            #     
            #}
            
            #updat the df.Pos data.frame
            # df.Pos$MarketValue[j]<-curvalue;
            # df.Pos$OpenPandL[j]<- df.Pos$MarketValue[j]-df.Pos$TotalCost[j];
            # df.Pos$OpenPerShare[j]<-df.Pos$OpenPandL[j]/df.Pos$quantity[j];
            # df.Pos$
            cat(j,symb,adate,curpos,curprice,curvalue,"\n");
        }
        # fnout               <-paste("D:/Projects/DDJIOutPut/Pos_",symb,".csv",sep="");
        # write.csv(df.Pos,file=fnout);
        l.DailyPositions[[symb]]<-df.Pos;
    }   
    df.AllPos<-list_df2df(l.DailyPositions);
    return(df.AllPos);
} 
    