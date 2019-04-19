dirOneDrive         <-  "C:/Users/Denni/OneDrive";
dirProject          <-  paste(dirOneDrive,"/DelesinDJI/DDJI",sep="");
dirDDrive<-"D:/WarkleighD"       #Piasa Version
dirData<-"D:/WarkleighD/WarkleighData";

source(paste(dirProject, "/0_AllFunctions.R", sep = ""));
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
targetETF<-"XLU";
v.etf<-df.SectorETFs%>%dplyr::filter(Ticker=="XLU")
#get holdings
fnin<-paste("Holdings/",targetETF,"_H.csv",sep="");
df.x<-read.csv(fnin)
names(df.x)
v.etfTickers<-c(targetETF,as.character(df.x$Identifier));
df.x<-f.getETFpackage(v.etfTickers )



