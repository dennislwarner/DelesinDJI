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
f.fPortFullSim<-function(df.x.xts,df.dictentry){
    freq<-"months";
    trainyears<-2;
    testyears<-1;
    l.PortData   <-   f.prepPortData(df.x.xts,freq,trainyears,testyears);
    names(l.PortData)
    # begin the era loop
    
    df.Eras<-l.PortData$df.Eras;
    df.x.xts<-l.PortData$df.x.xts
    df.R.xts<-l.PortData$df.R.xts
    df.R.ts<-as.timeSeries(df.R.xts);
    v.dates<-l.PortData$v.dates
    iera<-0;
    while(iera<l.PortData$numEras){
        iera<-iera+1;
        ftrain<-df.Eras$fTrno[iera];
        ltrain<-df.Eras$lTrno[iera];
        ftest<-df.Eras$fTsto[iera];
        ltest<-df.Eras$lTsto[iera];
        Rtrain<-df.R.ts[ftrain:ltrain,];
        Rtest<-df.R.ts[ftest:ltest,];
        #the data have been extracted now derive the optimum weights

        cat(iera,as.Date(v.dates[df.Eras$fTrno[iera]]),
            df.Eras$lTrno[iera],
            df.Eras$fTsto[iera],
            df.Eras$lTsto[iera],
            df.Eras$TstSpan[iera],"\n")
    }
    
    
    
    
    
}
f.fPoertfolioEstimates2<-function(df.x.xts,df.dictentry){
    #convert xts to time series as used in fPortfolio package
    df.x.xts<-na.locf(df.x.xts,na.rm=FALSE,fromLast = TRUE)
    df.xx.ts<-as.timeSeries(df.x.xts)
    #convert to returns
    df.r.ts<-returns(df.xx.ts,trim=FALSE)
    df.rr.ts<-df.r.ts;
    df.rr.ts[1,]<-0;
    #and to cumulative returns
    df.cr.ts<-cumulated(df.rr.ts)
    etfname<-str_sub(df.dictentry$ETF,1,-3)
    #Basic Statistics of Financial Assets----
    df.stats<-basicStats(df.r.ts[, 1:6])
    colMeans(100 * df.r.ts,na.rm=TRUE);
    df.ra.ts<-df.r.ts;df.ra.ts[is.na(df.ra.ts)]<-0;
    Covariance <- round(cov(100 * df.ra.ts), digits = 4);
    #  Plotting
    stitle<-paste(etfname," Components")
    plot(df.ra.ts[,1:10], main = stitle, col = "steelblue")
    
    myset<-df.cr.ts[,1:5]
    plot(myset, plot.type = "single", , col = 1:5, xlab = "Date",
         ylab = "stitle")
    ttl<-paste(names(df.cr.ts)[1:5],sep="+")
    title(main = ttl)
    hgrid()
    plot(myset[,2],myset[,4]) #scatter plots
    
    myData<-df.rr.ts;
    v.etf.ts<-myData[,ncol(myData)]
    myStocks<-myData[,-ncol(myData)];
    v.stocks<-names(myStocks);
    nStocks<-ncol(myStocks);
    hclustComplete <- assetsSelect(myStocks, method = "hclust")
    hclustComplete
    
    plot(hclustComplete, xlab=stitle)
    #---------grouping using kmeans analysis
    kmeans <- assetsSelect(myStocks, method = "kmeans", control <- c(centers = 10,
                                                                   algorithm = "Hartigan-Wong"))
    sort(kmeans$cluster)
    library(fMultivar)
    v.stocks   <- assetsArrange(myStocks, method = "hclust"); #arrange by hierarchical cluistering
    myStocks2<-100*myStocks[,v.stocks];
    myData2<-100*myData;
    library(fPortfolio);
    
    #First dow equally weighted portfolio
    ewSpec<-portfolioSpec();  #create default (MV) portfoliospec
    nAssets<-ncol(myData2)-1
    setWeights(ewSpec) <- rep(1/nAssets, times = nAssets)
    ewPortfolio <- feasiblePortfolio(
        data = myStocks2,
        spec = ewSpec,
        constraints = "LongOnly")
    print(ewPortfolio)
    col <- divPalette(ncol(myStocks2), "RdBu")
    weightsPie(ewPortfolio, radius = 0.7, col = col)
    mtext(text = "Equally Weighted MV Portfolio", side = 3, line = 1.5,
          font = 2, cex = 0.7, adj = 0)
    weightedReturnsPie(ewPortfolio, radius = 0.7, col = col)
    covRiskBudgetsPie(ewPortfolio, radius = 0.7, col = col)
    #----------------------------------------------------------
    # Minimum risk portfolio given a target return
    #----------------------------------------------------------
    minriskSpec <- portfolioSpec();
    targetReturn <- getTargetReturn(myPortfolio@portfolio)["mean"]
    setTargetReturn(minriskSpec) <- targetReturn
    minriskPortfolio <- efficientPortfolio(
        data = myStocks2,
        spec = minriskSpec,
        constraints = "LongOnly")
    print(minriskPortfolio)
    weightsPie(minriskPortfolio, radius = 0.7, col = col)
    mtext(text = "Min Risk MV Portfolio", side = 3, line = 1.5,
          font = 2, cex = 0.7, adj = 0)
    weightedReturnsPie(minriskPortfolio, radius = 0.7, col = col)
    covRiskBudgetsPie(minriskPortfolio, radius = 0.7, col = col)
    
    
    col <- qualiPalette(ncol(myStocks2), "Dark2");
                        
    weightsPie(minriskPortfolio, radius = 0.7, col = col)
    mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
            font = 2, cex = 0.7, adj = 0)
    
    weightedReturnsPie(minriskPortfolio, radius = 0.7, col = col)
    mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
            font = 2, cex = 0.7, adj = 0)
    covRiskBudgetsPie(minriskPortfolio, radius = 0.7, col = col)
    mtext(text = "Minimal Risk MV Portfolio", side = 3, line = 1.5,
            font = 2, cex = 0.7, adj = 0)
    #----------------------------------------------------------
    # Global Minimum risk portfolio 
    #----------------------------------------------------------
    globminSpec <- portfolioSpec()
    globminPortfolio <- minvariancePortfolio(
        data = myStocks2,
        spec = globminSpec,
        constraints = "LongOnly")
    print(globminPortfolio)
    
    
    
    
   col <- seqPalette(ncol(myStocks2), "YlGn")
   weightsPie(globminPortfolio, box = FALSE, col = col)
   mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
            line = 1.5, font = 2, cex = 0.7, adj = 0)
   weightedReturnsPie(globminPortfolio, box = FALSE, col = col)
   mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
            line = 1.5, font = 2, cex = 0.7, adj = 0)
   covRiskBudgetsPie(globminPortfolio, box = FALSE, col = col)
    mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
            line = 1.5, font = 2, cex = 0.7, adj = 0)
    #----------------------------------------------------------
    # Tangency portfolio 
    #----------------------------------------------------------
    tgSpec <- portfolioSpec()
    setRiskFreeRate(tgSpec) <- 0.02
    box.1<-paste("minW[1:",nStocks,"]=0.0",sep="");
    box.2<-paste("maxW[1:",nStocks,"]=0.2",sep="");
    boxConstraints<-c(box.1,box.2)
    tgPortfolio <- tangencyPortfolio(
        data = myStocks2,
        spec = tgSpec,
        constraints=boxConstraints)
    print(tgPortfolio);    
        
    col <- seqPalette(ncol(myStocks2), "BuPu") 
    weightsPie(tgPortfolio, box = FALSE, col = col)
    mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
       font = 2, cex = 0.7, adj = 0)
    weightedReturnsPie(tgPortfolio, box = FALSE, col = col)
    mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
       font = 2, cex = 0.7, adj = 0)
    covRiskBudgetsPie(tgPortfolio, box = FALSE, col = col)
    mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
       font = 2, cex = 0.7, adj = 0) 

  
    for (i in 1:3) plot(myStocks2[, (10 * i - 9):(10 * i)])
    for (i in 1:3) plot(myStocks2[, (10 * i - 9):(10 * i)])
    assetsCorImagePlot(myStocks2)
    plot(assetsSelect(myStocks2))
    frontier <- portfolioFrontier(myStocks2)
    tailoredFrontierPlot(frontier)
    weightsPlot(frontier)
    #showClass("fPFOLIOBACKTEST")
   
    
    #The backtester is hard to debug.  Try it manually
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    myBacktest <- portfolioBacktest()
    setWindowsHorizon(myBacktest) <- "12m"
    equidistWindows(data = myData2, backtest = myBacktest)   
    
  
    mySpec <- tgSpec;#portfolioSpec()
    myConstraints <- boxConstraints
    myBacktest <- portfolioBacktest()
    setWindowsHorizon(myBacktest) <- "18m"
    setSmootherLambda(myBacktest) <- "3m";
    myFormula<-paste(etfname,"=",paste(v.stocks,collapse="+"),sep="");
    myFormula<-paste(etfname,"=",paste(v.stocks[1:9],collapse="+"),sep="");
    myPortfolios <- portfolioBacktesting(formula = myFormula,
                                            data = myData2, spec = mySpec, constraints = myConstraints,
                                            backtest = myBacktest, trace = TRUE)
    
    }    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 
