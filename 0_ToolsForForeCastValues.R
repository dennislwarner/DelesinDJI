f.makeRates<-function(df.xts){
    df.AllRates.xts<-allReturns(df.xts,type='log',leading=FALSE);  # Quantmod's answer
    df.AllRates.xts$myWeekly<-f.myxtsHPror(df.xts$Adj_Close,5,"myWeekly");
    df.AllRates.xts$myMonthly<-f.myxtsHPror(df.xts$Adj_Close,21,"myMonthly");
    df.AllRates.xts$myQuarterly<-f.myxtsHPror(df.xts$Adj_Close,63,"myQuarterly");
    df.AllRates.xts$myYearly<-f.myxtsHPror(df.xts$Adj_Close,252,"myYearly");
    mydfs.xts<-df.AllRates.xts[,c("daily","myWeekly","myMonthly","myQuarterly","myYearly")]
    return(mydfs.xts)
}
f.mysd<-function(mydfs,symb){
    # descriptive routine to calculate, and create a display table
    # showing the different answers given by 3 ways of calculating
    # standard Deviations.
    #  Nomenclature   " D  Direct" = COmputing non-overlapping Periodic Rates of Return, then computing moments of these
    #                 " J Project" = Use inverse square law,  i.e. multiply the daily sd by SQRET(periodLength)
    #                 " V  Overlapping"= Compute rolling periodic rates of return, the computeing sd of these
    
    
    # find standard deviations of overlapping periods of Ror 
    VsdD<-sd(mydfs$Daily,na.rm=TRUE);
    VsdW<-sd(mydfs$Weekly,na.rm=TRUE);
    VsdM<-sd(mydfs$Monthly,na.rm=TRUE);
    VsdQ<-sd(mydfs$Quarterly,na.rm=TRUE);
    VsdY<-sd(mydfs$Yearly,na.rm=TRUE);
    # find standard deviation of independant periods of Ror
    #First create independent aggregations for each period
    DRW<-apply.weekly(mydfs$Daily,sum);
    DRM<-apply.monthly(mydfs$Daily,sum);
    DRQ<-apply.quarterly(mydfs$Daily,sum);
    DRY<-apply.yearly(mydfs$Daily,sum);
    DsdW<-sd(DRW,na.rm=TRUE)
    DsdM<-sd(DRM,na.rm=TRUE);
    DsdQ<-sd(DRQ,na.rm=TRUE);
    DsdY<-sd(DRY,na.rm=FALSE);
    # Project the Daily
    
    
    JsdW<-sqrt(5)*VsdD;
    JsdM<-sqrt(21)*VsdD;
    JsdQ<-sqrt(63)*VsdD;
    JsdY<-sqrt(252)*VsdD;
    m.SD<-matrix(0,3,6);
    
    m.SD[1,]<-t(c(0,VsdD,VsdW,VsdM,VsdQ,VsdY));   #enter the overlapping values
    m.SD[2,]<-t(c(0,NA,DsdW,DsdM,DsdQ,DsdY));      #enter the non-overlapping values
    m.SD[3,]<-t(c(0,NA,JsdW,JsdM,JsdQ,JsdY));      #enter the projected values
    fm<-function(x) round(100.0*x,digits=2);
    m.SDD<-apply(m.SD,2,function(x) fm(x))
    df.SD<-data.frame(m.SDD);
    names(df.SD)<-c("Method","Daily","Weekly","Monthly","Quarterly","Yearly");
    df.SD$Method<-c("Overlapping","Independent","Projected");
    cap<-paste("Various StDev Calculation Methods for ",symb,sep="");
    dt<-DT::datatable(df.SD,
                  options = list(
                      scrollX = FALSE
                  ),caption=cap);
    return(dt);
} 
f.Multimysd<-function(l.R){
    # descriptive routine to calculate, and create a display table
    # showing the different answers given by 3 ways of calculating
    # standard Deviations.
    #  Nomenclature   " D  Direct" = COmputing non-overlapping Periodic Rates of Return, then computing moments of these
    #                 " J Project" = Use inverse square law,  i.e. multiply the daily sd by SQRET(periodLength)
    #                 " V  Overlapping"= Compute rolling periodic rates of return, the computeing sd of these
    isymb<-0;
    while(isymb < length(l.R))
        {             
        isymb<-isymb+1;
        #cat(isymb,names(l.R)[isymb],"\n");          
        symb<-v.tickers[isymb];
        
        mydfs<-l.R[[symb]];
        
        # find standard deviations of overlapping periods of Ror 
        VsdD<-sd(mydfs$Daily,na.rm=TRUE);
        VsdW<-sd(mydfs$Weekly,na.rm=TRUE);
        VsdM<-sd(mydfs$Monthly,na.rm=TRUE);
        VsdQ<-sd(mydfs$Quarterly,na.rm=TRUE);
        VsdY<-sd(mydfs$Yearly,na.rm=TRUE);
        # find standard deviation of independant periods of Ror
        #First create independent aggregations for each period
        DRW<-apply.weekly(mydfs$Daily,sum);
        DRM<-apply.monthly(mydfs$Daily,sum);
        DRQ<-apply.quarterly(mydfs$Daily,sum);
        DRY<-apply.yearly(mydfs$Daily,sum);
        DsdW<-sd(DRW,na.rm=TRUE)
        DsdM<-sd(DRM,na.rm=TRUE);
        DsdQ<-sd(DRQ,na.rm=TRUE);
        DsdY<-sd(DRY,na.rm=FALSE);
        # Project the Daily
        JsdW<-sqrt(5)*VsdD;
        JsdM<-sqrt(21)*VsdD;
        JsdQ<-sqrt(63)*VsdD;
        JsdY<-sqrt(252)*VsdD;
        m.SD<-matrix(0,3,7);
    
        m.SD[1,]<-t(c(0,0,VsdD,VsdW,VsdM,VsdQ,VsdY));   #enter the overlapping values
        m.SD[2,]<-t(c(0,0,NA,DsdW,DsdM,DsdQ,DsdY));      #enter the non-overlapping values
        m.SD[3,]<-t(c(0,0,NA,JsdW,JsdM,JsdQ,JsdY));      #enter the projected values
        fm<-function(x) round(100.0*x,digits=2);
        m.SDD<-apply(m.SD,2,function(x) fm(x));
        df.SDD<-data.frame(m.SDD)
        names(df.SDD)<-c("Symbol","Method","Daily","Weekly","Monthly","Quarterly","Yearly");
        df.SDD$Method<-(c("Overlapping","Independent","Projected"));
        df.SDD$Symbol<-symb;
        
        if(isymb==1){
            df.SD<-df.SDD;
        }else{
            
            df.SD<-rbind(df.SD,df.SDD);
        }
        
    }
    
    
    cap<-"Various StDev Calculation Methods";
    dt<-DT::datatable(df.SD,
                      options = list(
                          scrollX = FALSE,
                          scrollY = FALSE,
                          paging=FALSE,
                          searching=FALSE
                      ),caption=cap);
    return(dt);
} 

f.CompareStats<-function(l.R){
    
    # Compute the means and standard deviations for a variety of holding periods
    # Split the series
    isymb<-0;
    df.Stats<-data.frame(matrix(0,3,7));
    m.Stats<-matrix(0,3,7);
    m.StatsA<-m.Stats;
    names(df.Stats)<-c("Symbol","Mu1st","sd1st","Mu2nd","sd2nd","MuTotal","sdTotal");
    colnames(m.Stats)<-c("Symbol","Mu1st","sd1st","Mu2nd","sd2nd","MuTotal","sdTotal");
    colnames(m.StatsA)<-c("Symbol","Mu1st","sd1st","Mu2nd","sd2nd","MuTotal","sdTotal");
    while(isymb<length(l.R)){
        isymb<-isymb+1;
        symb       <- names(l.R)[isymb];
        df.R.xts   <- l.R[[symb]][-1,];  #extract this symbols rates
        df.R       <- f.xts2df(df.R.xts)%>%dplyr::select(Date,Daily,Quarterly)
        df.R.xts   <- 100.0*f.df2xts(df.R);
        #find the beginning and the ened of the Quarterly returns
        df.nz.xts   <- df.R.xts[!is.na(df.R.xts$Quarterly),]
        startQ    <- as.character(first(index(df.nz.xts)));
        nz        <- nrow(df.nz.xts)
        endQ      <- as.character(last(index(df.nz.xts)));
        cat("Quarterly Holding Period data are from ",startQ," to ",endQ,"\n");
        qrange    <- paste(startQ,"/",endQ,sep="");
        if((nz %% 2)==0){
            
            firstStart<-1;
            
        }else{
            firstStart<-2;
        }
        breadth         <- nz%/%2;
        secondStart     <- firstStart+breadth;
        firstEnd        <- secondStart-1;
        secondEnd       <- nz;
        v.dat<-index(df.nz.xts);
        
        firstStartDate<-v.dat[firstStart];
        firstEndDate<-v.dat[firstEnd];
        secondStartDate<-v.dat[secondStart];
        secondEndDate<-v.dat[secondEnd];
        firstSpan       <- paste(firstStartDate,firstEndDate,sep="/");
        secondSpan       <- paste(secondStartDate,secondEndDate,sep="/");
        totalSpan      <-paste(firstStartDate,secondEndDate,sep="/");
        
        df.First.xts   <- df.nz.xts[firstSpan,];
        df.Second.xts  <- df.nz.xts[secondSpan,];
        df.Total.xts   <- df.nz.xts[totalSpan,];
        
        #muFirstQ    <-mean(df.First.xts$Quarterly);
        #muSecondQ   <-mean(df.Second.xts$Quarterly);
        #muTotalQ    <-mean(df.Total.xts$Quarterly);
        
        muFirstD<-mean(df.First.xts$Daily);
        muSecondD<-mean(df.Second.xts$Daily);
        muTotalD<-mean(df.Total.xts$Daily);
        
        muFirstQ    <-63*muFirstD;
        muSecondQ   <-63*muSecondD;
        muTotalQ    <-63*muTotalD;
        
        multfactor<-sqrt(63)
        sdFirstQ<-multfactor*sd(df.First.xts$Daily);
        sdSecondQ<-multfactor*sd(df.Second.xts$Daily);
        sdTotalQ<-multfactor*sd(df.Total.xts$Daily);
        
        MUAnnualizer <- 252/63;
        SDAnnualizer <- sqrt(MUAnnualizer)
        muFirstA<-MUAnnualizer*muFirstQ;
        muSecondA<-MUAnnualizer*muSecondQ;
        muTotalA<-MUAnnualizer*muTotalQ;
        sdFirstA<-SDAnnualizer*sdFirstQ;
        sdSecondA<-SDAnnualizer*sdSecondQ;
        sdTotalA<-SDAnnualizer*sdTotalQ;
        
        
        
        
        
        m.Stats[isymb,]<-c(0,
                           round(muFirstQ,digits=2),
                           round(sdFirstQ,digits=2),
                           round(muSecondQ,digits=2),
                           round(sdSecondQ,digits=2),
                           round(muTotalQ,digits=2),
                           round(sdTotalQ,digits=2)); 
        m.StatsA[isymb,]<-c(0,
                            round(muFirstA,digits=2),
                            round(sdFirstA,digits=2),
                            round(muSecondA,digits=2),
                            round(sdSecondA,digits=2),
                            round(muTotalA,digits=2),
                            round(sdTotalA,digits=2)); 
        
    }
    ####Quarterly Values
    df.StatsQ<-data.frame(m.Stats);
    df.StatsQ$Symbol<-(v.tickers);
    df.StatsA<-data.frame(m.StatsA);
    df.StatsA$Symbol<-v.tickers;
    cap<-"Quarterly Holding Period Values";
    dtQ<-DT::datatable(df.StatsQ,
                       options = list(
                           scrollX = FALSE,
                           scrollY = FALSE,
                           paging=FALSE,
                           searching=FALSE
                           
                       ),caption=cap);
    #dtQ
    
    
    cap<-"Annualized Values";
    dtA<-DT::datatable(df.StatsA,
                       options = list(
                           scrollX = FALSE,
                           scrollY = FALSE,
                           paging=FALSE,
                           searching=FALSE
                           
                       ),caption=cap);
    #dtA
    l.dt<-list(dtQ=dtQ,dtA=dtA);
    
    return(l.dt);
}
f.MakeDistributions<-function(v.tickers,m.Stats)
{
    l.res<-list();
    numbsymb<-nrow(m.Stats);
    isymb<-0;
    while(isymb<numbsymb){
        isymb<-isymb+1;
        symb<-v.tickers[isymb];
        df.FDVEst<-data.frame(matrix(0,51,10))
        names(df.FDVEst)<-c("Index",
                            "XFirst","YFirst","YFirstC",
                            "XSecond","YSecond","YSecondC",
                            "XTotal","YTotal","YTotalC")
            
        pseq1<-seq(.01,.49,by=.02);
        pseq2<-seq(.51,.99,by=.02); 
        pseq<-c(pseq1,0.50,pseq2); 
        df.FDVEst$YFirstC<-pseq;
        df.FDVEst$YSecondC<-pseq;
        df.FDVEst$YTotalC<-pseq;
        mu1st<-m.Stats[isymb,"Mu1st"]
        sd1st<-m.Stats[isymb,"sd1st"]
        mu2nd<-m.Stats[isymb,"Mu2nd"]
        sd2nd<-m.Stats[isymb,"sd2nd"]
        muTotal<-m.Stats[isymb,"MuTotal"]
        sdTotal<-m.Stats[isymb,"sdTotal"]
        #using the probabilities (pseq) calculate the inverse of the cumulative density function
        df.FDVEst$XFirst<-qnorm(pseq,mu1st, sd1st );
        df.FDVEst$XSecond<-qnorm(pseq,mu2nd , sd2nd );
        df.FDVEst$XTotal<-qnorm(pseq,muTotal , sdTotal );
        
        
        #using these x values compute the cumulative density function
        df.FDVEst$YFirstC<-pnorm(df.FDVEst$XFirst,mu1st,sd1st);
        plot(df.FDVEst$XFirst,df.FDVEst$YFirstC);
        df.FDVEst$YSecondC<-pnorm(df.FDVEst$XSecond,mu2nd,sd2nd);
        plot(df.FDVEst$XSecond,df.FDVEst$YSecondC);
        df.FDVEst$YTotalC<-pnorm(df.FDVEst$XTotal,muTotal,sdTotal);
        plot(df.FDVEst$XTotal,df.FDVEst$YTotalC);
        #  using the same x values plot the pdfs
        
        df.FDVEst$YFirst<-dnorm(df.FDVEst$XFirst,mu1st,sd1st);
        plot(df.FDVEst$XFirst,df.FDVEst$YFirst);
        
        df.FDVEst$YSecond<-dnorm(df.FDVEst$XSecond,mu2nd,sd2nd);
        plot(df.FDVEst$XSecond,df.FDVEst$YSecond);
        
        df.FDVEst$YTotal<-dnorm(df.FDVEst$XTotal,muTotal,sdTotal);
        plot(df.FDVEst$XTotal,df.FDVEst$YTotal);
        l.res[[symb]]<-df.FDVEst;
    }
    return(l.res)
}
f.MakeDistributions2<-function(v.tickers,df.Stats)
{
    m.stats<-as.matrix(df.Stats)
    l.res<-list();
    numbsymb<-nrow(df.Stats);
    isymb<-0;
    while(isymb<numbsymb){
        isymb<-isymb+1;
        symb<-v.tickers[isymb];
        dfV<-data.frame(matrix(0,153,5));
       
        names(dfV)<-c("Index","Period","X","Y","YC")
                            
        pseq1<-seq(.01,.49,by=.02);
        pseq2<-seq(.51,.99,by=.02); 
        pseq<-c(pseq1,0.50,pseq2); 
        dfV$YC<-rep(pseq,3)
        dfV$Period<-c(rep("First",51),rep("Second",51),rep("Total",51));
        
        mu1st     <-as.numeric(df.Stats[isymb,"Mu1st"]);
        sd1st     <-as.numeric(df.Stats[isymb,"sd1st"]);
        mu2nd     <-as.numeric(df.Stats[isymb,"Mu2nd"]);
        sd2nd     <-as.numeric(df.Stats[isymb,"sd2nd"]);
        muTotal   <-as.numeric(df.Stats[isymb,"MuTotal"]);
        sdTotal   <-as.numeric(df.Stats[isymb,"sdTotal"]);
        #using the probabilities (pseq) calculate the inverse of the cumulative density function
        dfV[dfV$Period=="First","X"]  <- qnorm(pseq,mu1st, sd1st );
        dfV[dfV$Period=="Second","X"] <- qnorm(pseq,mu2nd, sd2nd );
        dfV[dfV$Period=="Total","X"]  <- qnorm(pseq,muTotal, sdTotal );
        x1st<-dfV[dfV$Period=="First","X"];
        x2nd<-dfV[dfV$Period=="Second","X"];
        xTotal<-dfV[dfV$Period=="Total","X"];
        
        #using these x values compute the cumulative density function
        dfV[dfV$Period=="First","YC"]   <- pnorm(x1st,mu1st,sd1st);
        dfV[dfV$Period=="Second","YC"]  <- pnorm(x2nd,mu2nd,sd2nd);
        dfV[dfV$Period=="Total","YC"]   <- pnorm(xTotal,muTotal,sdTotal);

        #  using the same x values compute the pdfs
        dfV[dfV$Period=="First","Y"]   <- dnorm(x1st,mu1st,sd1st);
        dfV[dfV$Period=="Second","Y"]  <- dnorm(x2nd,mu2nd,sd2nd);
        dfV[dfV$Period=="Total","Y"]   <- dnorm(xTotal,muTotal,sdTotal);
        
        
        l.res[[symb]]<-dfV;
    }
    return(l.res)
}
f.MakeDistributions3<-function(df.stats)
{
 
    dfV<-data.frame(matrix(0,153,5));
    names(dfV)<-c("Index","Period","X","Y","YC")
        
    pseq1<-seq(.01,.49,by=.02);
    pseq2<-seq(.51,.99,by=.02); 
    pseq<-c(pseq1,0.50,pseq2); 
    dfV$YC<-rep(pseq,3)
    dfV$Period<-c(rep("First",51),rep("Second",51),rep("Total",51));
        
    mu1st     <-df.stats$AnnualMean[1];
    sd1st     <-df.stats$AnnualSigma[1]
    mu2nd     <-df.stats$AnnualMean[2];
    sd2nd     <-df.stats$AnnualSigma[2]
    muTotal   <-df.stats$AnnualMean[3];
    sdTotal   <-df.stats$AnnualSigma[3];  #de-annualize


    #using the probabilities (pseq) calculate the inverse of the cumulative density function
    dfV[dfV$Period=="First","X"]  <- qnorm(pseq,mu1st, sd1st );
    dfV[dfV$Period=="Second","X"] <- qnorm(pseq,mu2nd, sd2nd );
    dfV[dfV$Period=="Total","X"]  <- qnorm(pseq,muTotal, sdTotal );
    x1st<-dfV[dfV$Period=="First","X"];
    x2nd<-dfV[dfV$Period=="Second","X"];
    xTotal<-dfV[dfV$Period=="Total","X"];
    
    #using these x values compute the cumulative density function
    dfV[dfV$Period=="First","YC"]   <- pnorm(x1st,mu1st,sd1st);
    dfV[dfV$Period=="Second","YC"]  <- pnorm(x2nd,mu2nd,sd2nd);
    dfV[dfV$Period=="Total","YC"]   <- pnorm(xTotal,muTotal,sdTotal);
    
    #  using the same x values compute the pdfs
    dfV[dfV$Period=="First","Y"]   <- dnorm(x1st,mu1st,sd1st);
    dfV[dfV$Period=="Second","Y"]  <- dnorm(x2nd,mu2nd,sd2nd);
    dfV[dfV$Period=="Total","Y"]   <- dnorm(xTotal,muTotal,sdTotal);
        
   
    return(dfV);
}
f.MakeDistributions2WB<-function(l.WB){
    l.resWB<-list();
    v.tickers<-names(l.WB);
    numbsymb<-length(v.tickers);
    isymb<-0;
    while(isymb<numbsymb){
        isymb<-isymb+1;
        symb<-v.tickers[isymb];
        
        dfWB<-l.WB[[symb]]$df.FDVEst;  #Get the Estimated 
        v.rx<-c(dfWB$XFirst,dfWB$XSecond,dfWB$XTotal);
        v.ry<-c(dfWB$YFirst,dfWB$YSecond,dfWB$YTotal);
        v.ryc<-c(dfWB$YFirstC,dfWB$YSecondC,dfWB$YTotalC);
        dfV<-data.frame(matrix(0,153,5));
        
        names(dfV)<-c("Index","Period","X","Y","YC")
        dfV$X<-v.rx;
        dfV$Y<-v.ry;
        dfV$YC<-v.ryc;
        
        # pseq1<-seq(.01,.49,by=.02);
        # pseq2<-seq(.51,.99,by=.02); 
        # pseq<-c(pseq1,0.50,pseq2); 
        # dfV$YC<-rep(pseq,3)
        dfV$Period<-c(rep("First",51),rep("Second",51),rep("Total",51));
        #extract the series from the Workbench Structures
       l.resWB[[symb]]<-dfV;
    }
    return(l.resWB);
}
f.GetWorkBenchSeries<-function(v.tickers){
    M<-length(v.tickers);
    v.tickers[1]<-"GSPC";
    i<-0;
    l.WB<-list();
    while(i<M){
        i<-i+1;
        symbol<-v.tickers[i];
        fn1<-paste("W://ChartWork/ForeDistValuesEmpirical_",symbol,".csv",sep="");
        fn2<-paste("W://ChartWork/ForeDistValuesEstimated_",symbol,".csv",sep="");
        fn4<-paste("W://ChartWork/FValuesEstimated_",symbol,".csv",sep="");
        fn3<-paste("W://ChartWork/FValuesEmpirical_",symbol,".csv",sep="");
        fn5<-paste("W://ChartWork/FStatsEstimated_",symbol,".csv",sep="");
        
        df.FDVEmp<-read.csv(fn1,stringsAsFactors = FALSE);
        df.FDVEst<-read.csv(fn2,stringsAsFactors = FALSE);
        df.FVEmp<-read.csv(fn3,stringsAsFactors = FALSE);
        df.FVEst<-read.csv(fn4,stringsAsFactors = FALSE);
        df.FStatsEst<-read.csv(fn5,stringsAsFactors = FALSE);
        l.res<-list(df.FDVEmp=df.FDVEmp,
                    df.FDVEst=df.FDVEst,
                    df.FVEmp=df.FVEmp,
                    df.FVEst=df.FVEst,
                    df.FStatsEst=df.FStatsEst);
        l.WB[[symbol]]<-l.res
    }
    return(l.WB);
}
f.getSt<-function(df.xts){
    #dsum<-sum(df.xts$dRwb)
    dmu       <- mean(df.xts$dRwb,na.rm=TRUE);
    dsigma    <- sd(df.xts$dRwb,na.rm=TRUE);
    v.stats<-c(dmu,dsigma);
    names(v.stats)<-c("dmu","dsigma");
    return(v.stats);
}