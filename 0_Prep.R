
#------------Get ETF Dictionary
dt.ETFDict <- read_csv(paste(dirDocs, "/ETFDictionary.csv", sep = ""));
v.etfTickers <- dt.ETFDict$Symbol;
df.DJDict<-read_csv(paste(dirDocs,"/DJIA_Dict.csv",sep=""))
v.tickers<-df.DJDict$Symbol[1:30];

fnin<-paste(dirDocs, "/dt.Common.csv", sep = "");
dt.Common<-read_csv(fnin)
dt.CommonIndSec <-  dt.Common %>% dplyr::select(.,ticker, sector, industry);
df.DJIAComposition<-read.xlsx(paste(dirDocs,"/DJIAComposition.xlsx",sep=""),sheetName="DJIAComposition");
