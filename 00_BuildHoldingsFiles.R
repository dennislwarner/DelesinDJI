# Get a Price dataset for a partcular ETF for which we have holdings data----
targetETF      <-  "XHS";
#df.ETFdict <-df.EODTickers%>%dplyr::filter(Ticker %in% v.)
#get holdings
fnin        <-paste(dirHoldings,"/",targetETF,"_H.csv",sep="");
file.exists(fnin)
df.x        <-read.csv(fnin)
names(df.x)
v.etfTickers      <-c(targetETF,as.character(df.x$Identifier));
v.goodSymbol      <-v.etfTickers;
df.x              <-f.getETFpackage(v.etfTickers )
fnout             <-paste(dirPackages,"/",targetETF,"_P.csv",sep="");
write.csv(df.x,file=fnout);
#----
v.holdings    <- list.files(dirHoldings,include.dirs=FALSE);
#----Build Price datasets for each ETF fow which we have holdings data----
tic();
ihold<-0;
while(ihold<length(v.holdings)){
    ihold<-ihold+1;
    hold<-v.holdings[ihold];
    
    if(hold=="Original"){next}
    #load the holdings list for this target----
    hold<-str_sub(hold,start=1,end=-5);
    ietf<-match(hold,df.TargetDirectoryETFs$ETF);#<-str_sub(hold,start=1,end=-4);
    etf<-str_sub(hold,1,-3)
    fnin<-paste(dirHoldings,"/",hold,".csv",sep="");
    df.x<-read.csv(fnin);
    #----
    #----Determine which components have historical data and load them and save in "packages"
    v.etfTickers      <- c(etf,as.character(df.x$Identifier));
    etfCO             <- df.EODTickers%>%dplyr::filter(Ticker==etf);
    df.CO             <- df.EODTickers%>%dplyr::filter(Ticker %in% v.etfTickers);
    s                 <- setdiff(v.etfTickers,df.CO$Ticker);
    df.good           <- df.CO%>%dplyr::filter(Ticker %in% v.etfTickers) ;
    v.goodSymbol      <- df.good$Quandl_Code#[v.etfTickers %in% df.CO$Ticker]
    df.x.xts          <-f.getETFpackage(v.goodSymbol)
    
    names(df.x.xts)<-str_sub(names(df.x.xts),5,);#strip the first 4 characters of the names (EOD.xxxx)
    df.x<-f.xts2df(df.x.xts)
    #ensure coloumns are in required order, where the target etf is the last column
    #  Should move to the the 00_BuildHoldingsFiles.R above----
    v.target<-df.x[,etf]
    firsttarget<-min(which(is.finite(v.target)))
    firsttargetdate<-index(df.x.xts)[firsttarget]
    targetdesc<-df.TargetDirectoryETFs$Description[ietf];
    #drop the etf variable
    v.xnames<-names(df.x);
    etfpos<-match(etf,v.xnames);
    df.y<-df.x[,-etfpos]
    df.x<-cbind(df.y,df.x[,etfpos]);
    names(df.x)[ncol(df.x)]<-etf;
  
   # df.xx<-data.frame(df.x,v.target);
   # names(df.xx)[ncol(df.xx)]<-etf
    df.x.xts<-df.x%>%f.df2xts()
    
    
    fnout<-paste(dirPackages,"/",etf,"_P.xts",sep="");
    save(df.x.xts,file=fnout)
    cat(ihold,etf,"---",etfCO$Name,"#Companies = ",nrow(df.CO)-1,"\n");
}
toc();