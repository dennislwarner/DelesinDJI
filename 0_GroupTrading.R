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
f.GroupTrade <- function(l.Prices,df.etf.xts, horizon, ub, lb) {
    #  Prepare the group Ror tables
    #  1  df.Cumror.xts    cumulative adjusted close Rors
    #  2  df.Nror.xts      daily rates of return for next opening
    
    df.xts <- l.Prices[[1]];
    cname<-names(l.Prices)[1];
    #add the next opening variable, derived as a "negative lag in the lag.xts function)
    df.CumRor.xts<-f.deriveCloseRor(df.xts,cname);
    df.NRor.xts  <-f.deriveNRor(df.xts,cname);
   ico <- 1
   #Build a matrix of all cumulative rates of return for every stock
    while (ico < length(l.Prices)) {
        ico <- ico + 1
        symb <- names(l.Prices)[[ico]]
        df.xts <- l.Prices[[ico]]
        v.CumRor.xts<-f.deriveCloseRor(df.xts,symb);
        df.CumRor.xts<-merge.xts(df.CumRor.xts,v.CumRor.xts,join='left');
        v.NRor.xts<-f.deriveNRor(df.xts,symb);
        df.NRor.xts<-merge.xts(df.NRor.xts,v.NRor.xts);
    }
   df.Ror.xts<-diff.xts(df.CumRor.xts)
 
   etfname<-names(df.etf.xts)[1];
   v.etfror.xts<-f.derivedEtfRor(df.etf.xts,etfname);
    #----
    #----Rank the Nday performance----
    plot.xts(df.CumRor.xts[, 2:8])
    iwindow <- 0;
    v.windows <- c(5, 20, 65, 130, 261);
    while (iwindow < length(v.windows)) {
        iwindow <- iwindow + 1
        window <- v.windows[ihorizon]
        cat(iwindow, window, "\n")
       
        dfkR.xts <- diff.xts(df.CumRor.xts, lag = window, na.pad = TRUE)
        dfkR.xts[is.na(dfkR.xts)] <- 0
        m.rank                   <- t(apply(dfkR.xts, 1, rank))
        m.rank[is.na(dfkR.xts)]   <- 0
        m.pos                <- 0 * m.rank
        ibest <- 0
        
        #specify a matrix of ub,lb pairs
        m.bounds<-matrix(0,10,2)
        m.bounds[1,]<-c(0,0);
        m.bounds[2,]<-c(10,0);
        m.bounds[3,]<-c(15,0);
        m.bounds[4,]<-c(20,0);
        m.bounds[5,]<-c(25,0);
        m.bounds[6,]<-c(15,5);
        m.bounds[7,]<-c(15,10);
        m.bounds[8,]<-c(15,15);
        m.bounds[9,]<-c(20,10);
        m.bounds[10,]<-c(25,5);
        colnames(m.bounds)<-c("UB","LB");
        
        while (ibest < length(v.bests)) {
            ibest <- ibest + 1
            m.pos <- 0 * m.rank
            m.pos[m.rank > m.bounds[ibest,1]]     <- 1;
            m.pos[m.rank < m.bounds[ibest,2]]     <- -1;
            m.signs <- sign(m.pos)
            mabs <- abs(m.pos)
            m.w <- prop.table(mabs, 1)
            m.weights <- m.signs * m.w
            
            #normalize the investments shares
            
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
                      "H=",
                      horizon,
                      "UB=",
                      ub,
                      "LB=",
                      lb,
                      sep = " ")
            plot.xts(df.cumprof.xts[,31:32], main = maintitle)
            df.prof.xts <- diff.xts(df.cumprof.xts)
            
            
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
        return(df.cumprof.xts)
    }
}
