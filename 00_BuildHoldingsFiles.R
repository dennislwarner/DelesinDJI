# Get a Price dataset for a partcular ETF for which we have holdings data----
targetETF<-"XHS";
v.etf<-df.SectorETFs%>%dplyr::filter(Ticker=="targetETF")
#df.ETFdict<-df.EODTickers%>%dplyr::filter(Ticker %in% v.)
#get holdings
fnin<-paste(dirHoldings,"/",targetETF,"_H.csv",sep="");
file.exists(fnin)
df.x<-read.csv(fnin)
names(df.x)
v.etfTickers<-c(targetETF,as.character(df.x$Identifier));
v.goodSymbol<-v.etfTickers;
df.x<-f.getETFpackage(v.etfTickers )

fnout<-paste(dirPackages,"/",targetETF,"_P.csv",sep="");
write.csv(df.x,file=fnout);
#----
v.holdings<-list.files(dirHoldings,include.dirs=FALSE)
#----Build Price datasets for each ETF fow which we have holdings data----
tic();
ihold<-0;
while(ihold<length(v.holdings)){
    ihold<-ihold+1;
    hold<-v.holdings[ihold]
    if(hold=="Original"){next}
    hold<-str_sub(hold,start=1,end=-5);
    etf<-str_sub(hold,1,-3)
    fnin<-paste(dirHoldings,"/",hold,".csv",sep="");
    df.x<-read.csv(fnin);
    v.etfTickers<-c(etf,as.character(df.x$Identifier));
    etfCO<-df.EODTickers%>%dplyr::filter(Ticker==etf)
    df.CO<-df.EODTickers%>%dplyr::filter(Ticker %in% v.etfTickers)
    s<-setdiff(v.etfTickers,df.CO$Ticker)
    df.good<-df.CO%>%dplyr::filter(Ticker %in% v.etfTickers) ;
    v.goodSymbol<-df.good$Quandl_Code#[v.etfTickers %in% df.CO$Ticker]
    df.x.xts<-f.getETFpackage(v.goodSymbol)
    fnout<-paste(dirPackages,"/",etf,"_P.xts",sep="");
    save(df.x.xts,file=fnout)
    cat(ihold,etf,"---",etfCO$Name,"#Companies = ",nrow(df.CO)-1,"\n");
}