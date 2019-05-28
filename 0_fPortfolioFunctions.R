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
    #ds<-drawdownsStats(dd);
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
    
    # for conditional value at risk
   
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
   SBAC<-df.cr.ts[,2];
   seriesPlot(SBAC, title = FALSE)
   title(main = "SBACt", xlab = "", ylab = "Index")
   text(as.POSIXct("2006-11-25"), rev(SBAC)[1], as.character(rev(SBAC)[1]),
          font = 2)
   mtext("Source: SWX", side = 4, col = "grey", adj = 0, cex = 0.7); #"Margin text strings"
   #Decorating the plot
  seriesPlot(SBAC, title = FALSE)
  title(main = "SBAC Performance Index", xlab = "", ylab = "SBAC Index")
  text(as.POSIXct("2006-11-25"), rev(SBAC)[1], as.character(rev(SBAC)[1]),
          font = 2)
   mtext("Source: SBC", side = 4, col = "grey", adj = 0, cex = 0.7)
   #more decorations
   seriesPlot(SBAC, grid = FALSE, box = FALSE, rug = FALSE)
   hgrid()
   boxL()
   copyright()
   abline(h = 350, col = "orange")
   #------------------------------------------------------------------------------------------
   #   Modelling Asset Returns
   #------------------------------------------------------------------------------------------
   # Test whether returns arenormal
   Sr<-df.rr.ts[,2]
   shapiroTest <- assetsTest(df.rr.ts[, 1:3], method = "shapiro")
  print(shapiroTest)
     #slotNames(shapiroTest)
     names(shapiroTest)
   
   #7.2 FITTING ASSET RETURNS
     # first try the student's T distribuion
     fit <- assetsFit(df.rr.ts[, 1:3], method = "st")
     print(fit)
   #Can also use the copula estimation approach, use pckge fCopula
    #---------------------------------------------------------------------------------------------
    #
    # Selecting similar or dissimilar assets
     #
     # 
    
   
     XLUData<-df.rr.ts
     hclustComplete <- assetsSelect(XLUData, method = "hclust")
    hclustComplete
    plot(hclustComplete, xlab="XLU compnent assets")
     Call:
         hclust(d = dist(t(x), method = measure), method = method)
     #Cluster method : complete
     #Distance : euclidean
     #Number of objects: 9
  plot(hclustComplete, xlab = "XLU Components")
    mtext("Distance Metric: Euclidean", side = 3) 
     
     
     
    hclustWard <- assetsSelect(XLUData, method = "hclust", control = c(measure = "euclidean",
                                                                         method = "ward.D"))
    hclustWard
   plot(hclustWard)
  mtext("Distance Metric: Euclidean", side = 3)
  #---------------------------Grouping using eigenvalues
  
  assetsCorEigenPlot(XLUData, method = "kendall")
  #8.5
  #---------grouping using kmeans analysis
 kmeans <- assetsSelect(XLUData, method = "kmeans", control <- c(centers = 10,
                                                                    algorithm = "Hartigan-Wong"))
 sort(kmeans$cluster)
     
  #--------------------------------------------------------------------------------------------
  # COMPARING MULTIVARIATE RETURN AND RISK STATISTICS
 assetsBasicStatsPlot(XLUData[, -8], title = "", description = "")
 assetsMomentsPlot(XLUData[, -8], title = "", description = "")    
 assetsBoxStatsPlot(XLUData[, -8], title = "", description = "")   
   #---------------------------------------------------------------------------------------------
 #  PAIRWISE DEPENDENCIES OF ASSETS
 library(fMultivar)
 #Function:
 #     pairs displays pairs of scatterplots of assets
 # assetsPairsPlot displays pairs of scatterplots of assets
 # assetsCorgramPlot displays correlations between assets
 # assetsCorTestPlot displays and tests pairwise correlations
 # assetsCorImagePlot displays an image plot of correlations
 # squareBinning does a square binning of data points,
 # hexBinning does a hexagonal binning of data points
     #SIMPLE PAIRWISE SCATTER PLOTS OF ASSETS
 #     We will rearrange the assets as suggested by hierarchical clustering. This
 # yields a nicer arrangement and view of the off-diagonal scatterplot panels
 # of the graph.
 
 v.assets   <- assetsArrange(XLUData, method = "hclust");
 XLUData2<-100*XLUData[,v.assets];
 assetsPairsPlot(XLUData2,pch=19,cex=0.5,col="royalblue4");#Not useful for more than 10 variables
 v.sc<-seq(from=1,to=length(v.assets),by=4)
 assetsPairsPlot(XLUData2[,v.sc],pch=19,cex=0.5,col="royalblue4");
 assetsCorImagePlot(XLUData2[,v.sc])
 #----------------------------------------------------------------------------
 # Portfolio Framework
 #------------------------------------------------------------------------------
 library(fPortfolio);
 mySpec<-portfolioSpec();  #create default (MV) portfoliospec
 getWeights(mySpec)
 setWeights(mySpec) <- rep(1.0/28.0,28) 
 getTargetReturn(mySpec)
 getTargetRisk(mySpec)
 getOptimize(mySpec)
 setTargetReturn(mySpec) <- 0.025
 setTargetRisk(mySpec) <- 0.3
 setRiskFreeRate(mySpec)<-0.02
 
 showClass("fPFOLIODATA")
 
 myAssets<-100*XLUData[,v.assets[v.sc]];
 
 myData<-portfolioData(data = myAssets, spec = portfolioSpec())
 str(myData)
 print(myData)
 getStatistics(myData)
 showClass("fPFOLIOCON")
 setTargetReturn(mySpec) <- mean(myAssets);
 Constraints <- "LongOnly"
 defaultConstraints <- portfolioConstraints(myData, mySpec, Constraints)
 str(defaultConstraints, width = 65, strict.width = "cut")
 print(defaultConstraints)
 showClass("fPORTFOLIO")
 tgPortfolio <- tangencyPortfolio( myAssets)
 str(tgPortfolio, width = 65, strict.width = "cut") 
 print(tgPortfolio);
 tgPortfolio@portfolio
 #mean-var portfolios
 
 
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
