suppressMessages(library(Quandl));
suppressMessages(library(TTR,quietly=TRUE));
suppressMessages(library(bit,quietly=TRUE));
suppressMessages(library(chunked));
suppressMessages(library(compiler));
suppressMessages(library(data.table));
suppressMessages(library(dplyr));
suppressMessages(library(dtplyr));
suppressMessages(library(DT));
suppressMessages(library(ff,quietly=TRUE,warn.conflicts=FALSE));
suppressMessages(library(flexdashboard));
suppressMessages(library(lubridate));
suppressMessages(library(date));
suppressMessages(library(quantmod));
suppressMessages(library(readr));
suppressMessages(library(stringr));
suppressMessages(library(tictoc));
suppressMessages(library(tidyquant));
suppressMessages(library(tidyselect));
suppressMessages(library(tibbletime));
suppressMessages(library(xts));
suppressMessages(library(rbokeh));
suppressMessages(library(dygraphs));
#suppressMessages(library(rlist));
#suppressMessages(library(pipeR));
suppressMessages(library(caTools));
suppressMessages(library(qdapTools));
suppressMessages(library(pryr));
suppressMessages(library(abind));
#suppressMessages(library(formattable));
suppressMessages(library(combinat));
suppressMessages(library(htmltools));
enableJIT(3)
#suppressMessages(library(pryr));

suppressMessages(library(arrayhelpers));
suppressMessages(library(PerformanceAnalytics));
suppressMessages(library(modelr));
suppressMessages(library(randomForest));
suppressMessages(library(randomForestExplainer));
suppressMessages(library(randomForestSRC));
suppressMessages(library(party));
suppressMessages(library(ggplot2));
suppressMessages(library(gridExtra));
suppressMessages(library(arm));
options(java.parameters = "-Xmx6000m")
suppressMessages(library(BART));
suppressMessages(library(rJava));
suppressMessages(library(bartMachine));
suppressMessages(library(robustHD));
suppressMessages(library(xlsx));
suppressMessages(library(PortfolioAnalytics));
suppressMessages(library(foreach));
suppressMessages(library(iterators));
suppressMessages(library(ROI));
suppressMessages(library(ROI.plugin.quadprog));
suppressMessages(library(ROI.plugin.glpk));
suppressMessages(library(memisc));
suppressMessages(library(NCmisc));
#suppressMessages(library(installr));
#check.for.updates.R()
#install.R() 
list.functions.in.file("1_TradingSimulator.R", alphabetic = TRUE)
rfile <- file.choose();
