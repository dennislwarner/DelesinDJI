setMethod("lowess", "timeSeries", function(x, y = NULL, f = 2/3,
                                           iter = 3) {
    stopifnot(isUnivariate(x))
    ans <- stats::lowess(x = as.vector(x), y, f, iter)
    series(x) <- matrix(ans$y, ncol = 1)
    return(x);
})
setMethod("turnpoints", "timeSeries",
          function(x)
          {
              stopifnot(isUnivariate(x))
              tp <- suppressWarnings(pastecs::turnpoints(as.ts(x)))
              recordIDs <- data.frame(tp$peaks, tp$pits)
              rownames(recordIDs) <- rownames(x)
              colnames(recordIDs) <- c("peaks", "pits")
              timeSeries(data = x, charvec = time(x),
                         units = colnames(x), zone = finCenter(x),
                         FinCenter = finCenter(x),
                         recordIDs = recordIDs, title = x@title,
                         documentation = x@documentation)
          }
)
f.fPortfolioEstimates<-function(df.x.xts,df.dictentry){
    #optimize portfolio using fPortfolio package
    #convert xts to time series as used in fPortfolio package
    df.x.xts<-na.locf(df.x.xts,na.rm=FALSE,fromLast = TRUE)
    df.xx.ts<-as.timeSeries(df.x.xts)
    #convert to returns
    df.r.ts<-returns(df.xx.ts,trim=FALSE)
    df.rr.ts<-df.r.ts;
    df.rr.ts[1,]<-0;
    #and to cumulative returns
    df.cr.ts<-cumulated(df.rr.ts)
    # drawdowns----
    dd<-drawdowns(df.r.ts);
    #drawdownstats
    EQIX<-df.cr.ts[,"EQIX"];
    dd<-drawdowns(EQIX)
    ds<-drawdownsStats(EQIX);
    EQIX.LW<-lowess(EQIX,f=0.03);
    plot(EQIX)
    lines(EQIX.LW,col="brown",lwd=2);
    #locate turning points-------
    suppressMessages(library(pastecs));
    EQIX.TP<-turnpoints(EQIX.LW)
    EQIX.PEAKS<-EQIX.TP[EQIX.TP@recordIDs[,"peaks"]==TRUE,]
    EQIX.PITS <- EQIX.TP[EQIX.TP@recordIDs[, "pits"] == TRUE, ]
    plot(EQIX);
    lines(EQIX.LW,col="brown",lwd=2);
    points(EQIX.PEAKS, col = "green3", pch = 24)
    #----------------------------------------------
    #Basoc Statistics of Financial Assets----
    df.stats<-basicStats(df.r.ts[, 1:6])
    colMeans(100 * df.r.ts,na.rm=TRUE);
    df.ra.ts<-df.r.ts;df.ra.ts[is.na(df.ra.ts)]<-0;
    Covariance <- round(cov(100 * df.ra.ts), digits = 4)
    skewness(EQIX)
    kurtosis(EQIX)
    #compute quantiles for all columns jointly, using method recommended for CVAR
    quantile(df.r.ts, probs = seq(0, 1, 0.25), type = 1,na.rm=TRUE)
    #a particular quantile for each column separately
    colQuantiles(df.r.ts, prob = 0.05, type = 1)
    # 3 risk measures in fPortfolio
    # first sample covariance risk of a simple, equally weighted 3 component portfolio
    covRisk(100*df.ra.ts[,1:3], weights = c(1, 1, 1)/3)
    # for value at risk
    varRisk(100*df.ra.ts[,1:3], weights = c(1, 1, 1)/3, alpha = 0.05)
    VaR.5%
    # for conditional value at risk
    -0.56351
    cvarRisk(100*df.ra.ts[,1:3], weights = c(1, 1, 1)/3, alpha = 0.05)
    CVaR.5%
    #can apply anty function across the columns, e.g.
    
    round(colStats(df.ra.ts, FUN = "median"),
          digits = 4)
    
    # there are a number of function that can be applied to the cumulants of columns
    #
    #--------------------------------------------------------------------------------
    #  Plotting
    plot(df.ra.ts[,1:10], main = "XLU components", col = "steelblue")
    
    myset<-df.cr.ts[,1:5]
    plot(myset, plot.type = "single", , col = 1:5, xlab = "Date",
           ylab = "XLU components")
    ttl<-paste(names(df.cr.ts)[1:5],sep="+")
    title(main = ttl)
    hgrid()
   plot(myset[,2],myset[,5]) #scatter plots
    #There are 3 preconfigured plots
   
   boxPlot(returns(myset[,1:4]))
   boxPercentilePlot(returns(myset[,1:4]))
   
   histPlot(df.ra.ts[,2])
   densityPlot(df.ra.ts[,2]);
   logDensityPlot(df.ra.ts[,2])
   
   qqnormPlot(df.ra.ts[,2])
   
  
   qqnigPlot(df.ra.ts[,2])
   qqghtPlot(df.ra.ts[,2])
   
   #Customizing Plots
   seriesPlot(df.cr.ts[,2])
   
   
   
    l.Res<-list();
    return(l.Res)
}

> SPI.TP <- turnpoints(SPI.LW)
> SPI.PEAKS <- SPI.TP[SPI.TP@recordIDs[, "peaks"] == TRUE, ]
> SPI.PITS <- SPI.TP[SPI.TP@recordIDs[, "pits"] == TRUE, ]
> plot(SPI)
> lines(SPI.LW, col = "brown", lwd = 2)
> points(SPI.PEAKS, col = "green3", pch = 24)
> points(SPI.PITS, col = "red", pch = 25)




> SPI <- SWX[, "SPI"]
> SPI.LW <- lowess(SPI, f = 0.08)
> plot(SPI)
> lines(SPI.LW, col = "brown", lwd = 2)
How