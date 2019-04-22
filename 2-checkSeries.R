dirOneDrive         <-  "C:/Users/Denni/OneDrive";
dirProject          <-  paste(dirOneDrive,"/DelesinDJI/DDJI",sep="");
dirDDrive<-"D:/WarkleighD"       #Piasa Version
dirData<-"D:/WarkleighD/WarkleighData";
dirHoldings<-paste(dirDDrive,"/ETFHoldings",sep="");
dirPackages<-paste(dirDDrive,"/ETFPackages",sep="");
dirsupport<-paste(dirDDrive,"/ETFSupport",sep="");

source(paste(dirProject, "/0_AllFunctions.R", sep = ""));
source(paste(dirProject, "/0_EtfFunctions.R", sep = ""));
source(paste(dirProject, "/0_BuildDJI.R", sep = ""));
source(paste(dirProject, "/0_LoadLibs.R", sep = ""));
dirDocs             <-   paste(dirProject, "/Docs", sep = "");
source(paste(dirProject,"/0_Prep.R",sep=""));

#retrieve the State Street SPDR ES and their components
v.SectorETFS<-c("XLC","XLY","XLP","XLE","XLF","XLV","XLI","XLB","XLRE","XLSR","XLK","XLU")
v.BigETFs<-c("SPY","MDY","SLY","DIA")
v.IndustryETFs<-c("XITK","XNTK","XAR","KBE","XBI",
                  "KCE","XHE","XHS","XHB","KIE",
                  "XWEB","XME","XES","XOP","XPH",
                  "KRE","XRT","XSD","XSW","XTH",
                  "XTL","XTN");
df.shorts<-read.csv("Docs/Shorts.csv",stringsAsFactors = FALSE)
v.shorts<-names(df.shorts)
df.EODTickers<-read.csv("Docs/EODTicker.csv",stringsAsFactors = FALSE)

df.SectorETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.SectorETFS)
df.BigETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.BigETFs)
df.IndustryETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.IndustryETFs)
df.ShortsETFs<-df.EODTickers%>%dplyr::filter(Ticker %in% v.shorts)

#Begin analysis
targetETF<-"XHS";
v.etf<-df.SectorETFs%>%dplyr::filter(Ticker=="targetETF")
#df.ETFdict<-df.EODTickers%>%dplyr::filter(Ticker %in% v.)
#get holdings
fnin<-paste(dirHoldings,"/",targetETF,"_H.csv",sep="");
file.exists(fnin)
df.x<-read.csv(fnin)
names(df.x)
v.etfTickers<-c(targetETF,as.character(df.x$Identifier));
df.x<-f.getETFpackage(v.etfTickers )

fnout<-paste(dirPackages,"/",targetETF,"_P.csv",sep="");
write.csv(df.x,file=fnout);
v.holdings<-list.files(dirHoldings,include.dirs=FALSE)
tic();
ihold<-0;
while(ihold<length(v.holdings)){
    ihold<-ihold+1;
    hold<-v.holdings[ihold]
    if(hold=="Orig"){next}
    hold<-str_sub(hold,start=1,end=-5);
    etf<-str_sub(hold,1,-3)
    cat(ihold,hold,etf,"\n");
    #retrieve the data 
    fn <- paste("EOD/",etf, ".11", sep = "")
    
    df.cl.xts <- Quandl(fn, api_key = '1yhZtVwmHpc7qys3iMuJ',
                        type = "xts", start_date = "2003-01-01")
    plot.xts(df.cl.xts,main=etf)
    
}
    
    