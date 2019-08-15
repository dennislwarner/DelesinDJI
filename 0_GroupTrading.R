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
    df.etf.xts$clag<-lag.xts(df.etf.xts,k=1);
    df.etf.xts$ror<-log(df.etf.xts[,1]/df.etf.xts$clag);
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
f.GroupTrade <- function(l.Prices,df.etf.xts, m.bounds, v.windows) {
    
    #  1  df.Cumror.xts    cumulative adjusted close Rors
    #  2  df.Nror.xts      daily rates of return for next opening
    #  3  df.Ror.xts       dail adjusted close Rors
    # initiate values of tracking structures
    ibestWindow<-0;
    ibestboundary<-0
    bestsharpe<--100;
    bestm.pos<-NULL;
    #----Prepare the required data frames----
    df.xts <- l.Prices[[1]];
    cname<-names(l.Prices)[1];
    #add the next opening variable, derived as a "negative lag in the lag.xts function)
    df.CumRor.xts<-f.deriveCloseRor(df.xts,cname);
    df.NRor.xts  <-f.deriveNRor(df.xts,cname);
   ico <- 1
   #Build a data frame of all cumulative rates of return for every stock
    while (ico < length(l.Prices)) {
        ico <- ico + 1
        symb <- names(l.Prices)[[ico]]
        df.xts <- l.Prices[[ico]]
        v.CumRor.xts<-f.deriveCloseRor(df.xts,symb);
        df.CumRor.xts<-merge.xts(df.CumRor.xts,v.CumRor.xts,join='left');
        v.NRor.xts<-f.deriveNRor(df.xts,symb);
        df.NRor.xts<-merge.xts(df.NRor.xts,v.NRor.xts);
    }
   df.Ror.xts<-diff.xts(df.CumRor.xts);
 
   etfname<-names(df.etf.xts)[1];
   v.etfror.xts<-f.derivedEtfRor(df.etf.xts,etfname);
    #----
    #----Rank the Nday performance----
    plot.xts(df.CumRor.xts[, 2:8])
   #assume risk free rate is .03
   Rfree<-0.03;
   RfreeDaily<-Rfree/252;
   #----Loop across the candidate data window lengths----
   #    
    iwindow <- 0;
    l.TradeRes<-list();
    while (iwindow < length(v.windows)) {
        iwindow <- iwindow + 1
        window <- v.windows[iwindow]
        cat(iwindow, window, "\n")
       #----For each stock Ror over a moving window compute its rank each day----
        dfkR.xts <- diff.xts(df.CumRor.xts, lag = window, na.pad = TRUE)
        dfkR.xts[is.na(dfkR.xts)] <- 0
        m.rank                   <- t(apply(dfkR.xts, 1, rank))
        m.rank[is.na(dfkR.xts)]   <- 0
        #----
        #----For each upper and lower bound pairs compute the trade profitability----
        #    
        m.signr<-sign(dfkR.xts)
        m.pos                <- 0 * m.rank
        ibest <- 0
        while (ibest < nrow(m.bounds)) {
          ibest <- ibest + 1
            #ibest<-6;   # the best run so far
            m.pos <- 0 * m.rank
            m.pos[m.rank > m.bounds[ibest,1]]     <- 1;
            #tougher short criterion
            shortable<-(m.rank<m.bounds[ibest,2])*1+(m.signr<0)*1
            
            m.pos[shortable==2]     <- -1;
            #bridge small gaps in positions
            x<-m.pos[,1]
            m.pos<-apply(m.pos,2,function(x) f.delay(x))
            
            m.signs <- sign(m.pos);
            mabs <- abs(m.pos)
            m.w <- prop.table(mabs, 1)
            m.weights <- m.signs * m.w
            
            
            m.nror <- coredata(df.NRor.xts)
            m.prof <- m.weights * m.nror
            
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
                paste("Total Prof= ",
                      prof,
                      "W=",
                      window,
                      "UB=",
                      m.bounds[ibest,1],
                      "LB=",
                      m.bounds[ibest,2],
                      sep = " ")
            plot.xts(df.cumprof.xts[,31:32], main = maintitle,legend.loc='topleft')
            df.prof.xts <- diff.xts(df.cumprof.xts)
            #build the performance analytics
            charts.PerformanceSummary(df.prof.xts[,31:32],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
            
            tabcapm<-table.CAPM(df.prof.xts[,31:32],Rb= v.etfror.xts,scale=252,Rf=RfreeDaily);
            tabstats<-table.AnnualizedReturns(df.prof.xts[,31:32],scale=252,Rf=RfreeDaily);
            #tabstats<-table.CalendarReturns(df.prof.xts[,31:32])
            idd<-paste("capm ",window,m.bounds[ibest,1], m.bounds[ibest,2],sep=" ")
            l.TradeRes[[idd]]<-tabstats;
            ret<-tabstats[1,1];
            st<-tabstats[2,1];
            trey<-tabcapm[12,1];
            sharp<-tabstats[3,1];
            if(sharp>bestsharp){
                bestsharp<-sharp;
                ibestboundary<-ibest;
                ibestWindow<-iwindow;
                bestm.pos<-m.pos
            }
            cat(idd,prof,"Ann Ror=",ret,"Stdev=", st, "Sharpe = ",tabstats[3,1],"Treynor=",trey,"\n")
            
     }
        #---
        tail(df.cumprof.xts)
        m.pos.xts<-as.xts(m.pos,order.by = index(df.NRor.xts));
        m.weights.xts<-as.xts(m.weights,order.by = index(df.NRor.xts));
        m.prof.xts<-as.xts(m.prof,order.by = index(df.NRor.xts));
        m.rank.xts<-as.xts(m.rank,order.by = index(df.NRor.xts));
        dff.xts<-merge.xts(df.Ror.xts[,1],df.CumRor.xts[,1],m.rank.xts[,1],m.pos.xts[,1],m.weights.xts[,1],df.NRor.xts[,1],m.prof.xts[,1],df.cumprof.xts[,1])
        names(dff.xts)<-c(cname,"CumRor","Rank","Pos","Weight","NRor","Prof","CumProf");
        #----
        
    }
    return(m.pos)
}
f.DistinctTrade <- function(l.Prices,df.etf.xts, m.bounds, v.windows) {
    
    #  1  df.Cumror.xts    cumulative adjusted close Rors
    #  2  df.Nror.xts      daily rates of return for next opening
    #  3  df.Ror.xts       daily adjusted close Rors
    nstocks<-length(l.Prices);
    # initiate values of tracking structures
    v.ibestWindow       <- numeric(nstocks);
    v.ibestboundary     <- (nstocks);
    v.bestsharpe        <- rep(-100,nstocks);
    
    m.bestpos<-matrix(0,nrow(df.etf.xts),nstocks)
    #----Prepare the required data frames----
    df.xts <- l.Prices[[1]];
    cname<-names(l.Prices)[1];
    #add the next opening variable, derived as a "negative lag in the lag.xts function)
    df.CumRor.xts<-f.deriveCloseRor(df.xts,cname);
    df.NRor.xts  <-f.deriveNRor(df.xts,cname);
    ico <- 1
    #Build a data frame of all cumulative rates of return for every stock
    while (ico < length(l.Prices)) {
        ico <- ico + 1
        symb <- names(l.Prices)[[ico]]
        df.xts <- l.Prices[[ico]]
        v.CumRor.xts<-f.deriveCloseRor(df.xts,symb);
        df.CumRor.xts<-merge.xts(df.CumRor.xts,v.CumRor.xts,join='left');
        v.NRor.xts<-f.deriveNRor(df.xts,symb);
        df.NRor.xts<-merge.xts(df.NRor.xts,v.NRor.xts);
    }
    df.Ror.xts<-diff.xts(df.CumRor.xts);
    v.cnames<-names(df.Ror.xts)
    etfname<-names(df.etf.xts)[1];
    v.etfror.xts<-f.derivedEtfRor(df.etf.xts,etfname);
    #----
    #----Rank the Nday performance----
    plot.xts(df.CumRor.xts[, 2:8])
    #assume risk free rate is .03
    Rfree<-0.03;
    RfreeDaily<-Rfree/252;
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
        dfkR.xts <- diff.xts(df.CumRor.xts, lag = window, na.pad = TRUE)
        dfkR.xts[is.na(dfkR.xts)] <- 0
        #don't use ranking when selecting signals methods for each stock
        m.rank                   <- t(apply(dfkR.xts, 1, rank))
        m.rank[is.na(dfkR.xts)]   <- 0
        #----
        #----For each upper and lower bound pairs compute the trade profitability----
        #    
        m.signr<-sign(dfkR.xts)
        m.pos                <- 0 * m.rank
        ibest <- 0
        while (ibest < nrow(m.bounds)) {    #loop through each set of upper and lower bounds
            ibest <- ibest + 1
            #ibest<-6;   # the best run so far
            m.pos <- 0 * m.rank
            m.pos[m.rank > m.bounds[ibest,1]]     <- 1;
            #tougher short criterion
            shortable<-(m.rank<m.bounds[ibest,2])*1+(m.signr<0)*1
            
            m.pos[shortable==2]     <- -1;
            #bridge small gaps in positions
            x<-m.pos[,1]
            m.pos<-apply(m.pos,2,function(x) f.delay(x))
            
            m.signs <- sign(m.pos);
            mabs <- abs(m.pos)
            m.w <- prop.table(mabs, 1)
            m.weights <- m.signs * m.w
            
            
            m.nror <- coredata(df.NRor.xts)
            m.prof <- m.weights * m.nror
            
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
                      "UB=",
                      m.bounds[ibest,1],
                      "LB=",
                      m.bounds[ibest,2],
                      sep = " ")
            plot.xts(df.cumprof.xts[,31:32], main = maintitle,legend.loc='topleft')
            df.prof.xts <- diff.xts(df.cumprof.xts)
            #build the performance analytics
            charts.PerformanceSummary(df.prof.xts[,1:5],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
            charts.PerformanceSummary(df.prof.xts[,6:10],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
            charts.PerformanceSummary(df.prof.xts[,11:15],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
            charts.PerformanceSummary(df.prof.xts[,16:20],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
            charts.PerformanceSummary(df.prof.xts[,21:25],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
            charts.PerformanceSummary(df.prof.xts[,26:30],Rf=Rfree,main=maintitle,legend.loc="topleft",p=0.95);
            tabcapm<-table.CAPM(df.prof.xts[,1:30],Rb= v.etfror.xts,scale=252,Rf=RfreeDaily);
            tabstats<-table.AnnualizedReturns(df.prof.xts[,1:30],scale=252,Rf=RfreeDaily);
            #tabstats<-table.CalendarReturns(df.prof.xts[,31:32])
            idd<-paste("capm ",window,m.bounds[ibest,1], m.bounds[ibest,2],sep=" ")
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
                    v.ibestboundary[istock]<-ibest;
                    v.ibestWindow[istock]<-iwindow;
                    m.bestpos[,istock]<-m.pos[,istock];
                    cat(v.cnames[istock],istock,idd,prof,"Ann Ror=",ret,"Stdev=", st, "Sharpe = ",sharp,"Treynor=",trey,"\n")
                }
                
                
            }
           
            
            
        }
        #---
        tail(df.cumprof.xts)
        m.pos.xts<-as.xts(m.pos,order.by = index(df.NRor.xts));
        m.weights.xts<-as.xts(m.weights,order.by = index(df.NRor.xts));
        m.prof.xts<-as.xts(m.prof,order.by = index(df.NRor.xts));
        m.rank.xts<-as.xts(m.rank,order.by = index(df.NRor.xts));
        dff.xts<-merge.xts(df.Ror.xts[,1],df.CumRor.xts[,1],m.rank.xts[,1],m.pos.xts[,1],m.weights.xts[,1],df.NRor.xts[,1],m.prof.xts[,1],df.cumprof.xts[,1])
        names(dff.xts)<-c(cname,"CumRor","Rank","Pos","Weight","NRor","Prof","CumProf");
        #----
        
    }
    m.signs <- sign(m.pos);
    mabs <- abs(m.pos)
    m.w <- prop.table(mabs, 1)
    m.weights <- m.signs * m.w;
     m.nror <- coredata(df.NRor.xts)
            m.prof <- m.weights * m.nror
            
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
                      "UB=",
                      m.bounds[ibest,1],
                      "LB=",
                      m.bounds[ibest,2],
                      sep = " ")
            plot.xts(df.cumprof.xts[,31:32], main = maintitle,legend.loc='topleft')
    return(m.weights)
}
