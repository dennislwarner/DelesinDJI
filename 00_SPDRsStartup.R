#-----------------------------------------------------------------------------|
#                                                                             |
#    Prepare the directory structures, Load the functions and Load startup    |
#    data and parameters for the SPDR / ETF project                           |
#                                                                             |
#    DLW 5/14/2019                                                            |
#                                                                             |
#-----------------------------------------------------------------------------|

dirDDrive       <- "D:/Projects/WarkleighD";
dirData         <- paste(dirDDrive,"/WarkleighData",sep="");

dirHoldings     <- paste(dirDDrive,"/ETFHoldings",sep="");
length(list.files(dirHoldings));

dirPackages<-paste(dirDDrive,"/ETFPackages",sep="");
length(list.files(dirHoldings));

dirSupport<-paste(dirDDrive,"/ETFSupport",sep="");
length(list.files(dirSupport))

source(paste(dirProject, "/0_LoadLibs.R", sep = ""));
length(search());
source(paste(dirProject, "/0_AllFunctions.R", sep = ""));

source(paste(dirProject, "/0b_EtfFunctions.R", sep = ""));
source(paste(dirProject, "/0_MakePortfolios.R", sep = ""));
source(paste(dirProject, "/0_BuildDJI.R", sep = ""));

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
df.shorts<-suppressMessages(read.csv("Docs/Shorts.csv",stringsAsFactors = FALSE));
v.shorts<-names(df.shorts)
df.EODTickers<-suppressMessages(read.csv("Docs/EODTicker.csv",stringsAsFactors = FALSE));

df.SectorETFs      <- df.EODTickers%>%dplyr::filter(Ticker %in% v.SectorETFS)
df.BigETFs         <- df.EODTickers%>%dplyr::filter(Ticker %in% v.BigETFs)
df.IndustryETFs    <- df.EODTickers%>%dplyr::filter(Ticker %in% v.IndustryETFs)
df.ShortsETFs      <- df.EODTickers%>%dplyr::filter(Ticker %in% v.shorts)
fnin               <- paste(dirDocs,"/TargetDirectory.xlsx",sep="");
df.TargetDirectoryETFs<-read.xlsx(fnin,sheetIndex = 1,stringsAsFactors = FALSE)
