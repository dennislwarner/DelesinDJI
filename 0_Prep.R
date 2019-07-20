
#------------Get ETF Dictionary
dt.ETFDict      <- suppressMessages(read_csv(paste(dirDocs, "/ETFDictionary.csv", sep = "")));
v.etfTickers    <- dt.ETFDict$Symbol;
df.DJDict       <- suppressMessages(read_csv(paste(dirDocs,"/DJIA_Dict.csv",sep="")))
v.tickers       <- df.DJDict$Symbol[1:30];
v.descriptions<-df.DJDict$Description[1:30];
fnin            <- paste(dirDocs, "/dt.Common.csv", sep = "");
dt.Common       <- suppressMessages(read_csv(fnin));
dt.CommonIndSec <- dt.Common %>% dplyr::select(.,ticker, sector, industry);
