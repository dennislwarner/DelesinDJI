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
##################################################
trainyears<-5;
testmonths<-3
f.getMyData<-function(df.x.xts){
    df.x.xts<-na.locf(df.x.xts,na.rm=FALSE,fromLast = TRUE)
    df.xx.ts<-as.timeSeries(df.x.xts)
    #convert to returns
    df.r.ts<-returns(df.xx.ts,trim=FALSE)
    df.r.xts<-as.xts(df.r.ts)
    df.r.xts[1,]<-0;
    df.rr.ts<-df.r.ts;
    df.rr.ts[1,]<-0;
    myData<-100*df.rr.ts;
    return(df.r.xts)
}
f.prepfPortData<-function(df.x.xts,freq,`trainyears`,testmonths){
    #fPortfolio works with daily frequency
    #  find the era points, where the training period has a given number of years and the test period a number of months
    v.meop<-endpoints(df.x.xts,on="months");
    nmonths<-length(v.meop)-1
    ntrainmonths<-trainyears*12
    numeras<-ceiling((nmonths-ntrainmonths)/testmonths);
    iera<-0;
    v.dates<-index(df.x.xts)
    df.era<-data.frame(matrix(0,numeras,4))
    names(df.era)<-c("TrainSpan","TestSpan","n3","n4")
    while(iera<numeras){
        iera<-iera+1
        ftrain<-(iera-1)*testmonths+1;
        ltrain<-ftrain+ntrainmonths-1;
        ftd<-v.dates[v.meop[ftrain]+1]
        ltd<-v.dates[v.meop[ltrain+1]]
        ftestd<-v.dates[v.meop[ltrain+1]+1];
        ltesto<-min(length(v.meop),ltrain+1+testmonths)
        ltestd<-v.dates[v.meop[ltesto]]
        
        trainspan<-paste(ftd,"/",ltd,sep="");
        testspan<-   paste(ftestd,"/",ltestd,sep="");
        rt<-         df.x.xts[trainspan,]
        df.era$TrainSpan[iera]  <- trainspan;
        df.era$TestSpan[iera]   <- testspan;
     }
    df.r.xts<-f.getMyData(df.x.xts);
    l.Res<-list(df.Eras= df.era,
                df.x.xts=df.x.xts,
                df.r.xts=df.r.xts,
                #R.ts = R.ts,
                v.dates=v.dates,
                trainyears=trainyears,
                testmonths=testmonths,
                v.stocks=names(df.r.xts),
                nStocks=length(v.stocks)
                )
    return(l.Res)
}
    
   
f.fPortFullSim<-function(df.x.xts,df.dictentry){
    freq<-"months";
    trainyears<-6;
    testmonths<-6;
    l.P   <-   f.prepfPortData(df.x.xts,freq,trainyears,testmonths);
    names(l.P)
    # begin the era loop
    
    df.Eras<-l.P$df.Eras;
    df.x.xts<-l.P$df.x.xts
    df.r.xts<-l.P$df.r.xts
    df.r.ts<-as.timeSeries(df.r.xts)
    
    v.dates<-l.P$v.dates
    nStocks<-l.P$nStocks;
    
      iera<-0;
    while(iera<l.P$numEras){
        iera<-iera+1;
        Trainspan<-df.Eras$TrainSpan[iera];
        Testspan<-df.Eras$TestSpan[iera];
        RTrain.ts<-as.timeSeries(df.r.xts[df.Eras$TrainSpan[iera],])
        RTest.ts<-as.timeSeries(df.r.xts[df.Eras$TestSpan[iera],])
        #both include the target etf in the last column,  separate them
        TrainStocks.ts<-RTrain.ts[,-ncol(RTrain.ts)]
        TrainETF.ts<-RTrain.ts[,ncol(RTrain.ts)]
        
        TestStocks.ts<-RTest.ts[,-ncol(RTest.ts)]
        TestETF.ts<-RTest.ts[,ncol(RTest.ts)]
        
        
       #the data have been extracted now derive the optimum weights

        cat(iera,Trainspan,"---",Testspan,"\n");
          
        #----------------------------------------------------------
        # Tangency portfolio 
        #----------------------------------------------------------
        tgSpec <- portfolioSpec()
        setRiskFreeRate(tgSpec) <- 0.02
        box.1<-paste("minW[1:",nStocks,"]=0.0",sep="");
        box.2<-paste("maxW[1:",nStocks,"]=0.2",sep="");
        boxConstraints<-c(box.1,box.2)
        tgPortfolio <- tangencyPortfolio(
            data = TrainStocks.ts,
            spec = tgSpec,
            constraints=boxConstraints)
        print(tgPortfolio);    
        v.weights<-tgPortfolio@portfolio$weights
        
       
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 
