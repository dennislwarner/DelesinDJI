f.buildMAGIC <- function(df) {
    df.MAGIC <- df %>%
        arrange(calendardate) %>%
        select(quarter, datekey, price, pe, de) %>%
        mutate(isLowPE = between(pe, 0.001, 10),
               isLowDE = between(de, 0, 0.5),
               isMagic = ((isLowPE * isLowDE) == 1))

}
f.isMagic <- function(df) {
    df <- df %>% mutate(isLowPE = between(pe, 0.001, 10),
               isLowDE = between(de, 0, 0.5),
               isMagic = ((isLowPE * isLowDE) == 1));
    return(df$isMagic);
}
f.cleanPrices <- function(v.p) {
    #eliminate zeros and NA from the price series
    v.p[v.p == 0] <- NA
    v.p <- na.locf(v.p, na.rm = FALSE);
    v.p <- na.locf(v.p, na.rm = FALSE, fromlast = TRUE)
}
f.generateDailyMagic <- function(df.t) {
    v.pgrat <- df.t$pgrat;
    v.pe <- df.t$pe;
    v.de <- na.locf(df.t$de, na.rm = FALSE);
    v.isMagic <- na.locf(df.t$isMagic, na.rm = FALSE);
    v.isMagic[is.na(v.isMagic)] <- FALSE;
    t <- 1;
    N <- nrow(df.t);
    while (t < N) {
        t <- t + 1;
        if (is.na(v.pe[t])) {
            if (!is.na(v.pe[t - 1])) {
                v.pe[t] <- v.pgrat[t] * v.pe[t - 1];
                if (v.pe[t] > 0) {
                    if (v.de[t] >= 0) {
                        v.isMagic[t] <- ((v.pe[t] < 10) && (v.de[t] < 0.5));
                    }
                }
                
            }
        }
    }
    v.pe[is.na(v.pe)] <- 0;
    v.de[is.na(v.de)] <- 0;
    lMagic <- list(v.de = v.de, v.pe = v.pe, v.isMagic = v.isMagic)
    return(lMagic)
}
f.dailySim <- function(df.m) {
    #get the prices
    fnin <- paste(dirData, "/AdjOpens/", df.m$ticker[1], ".RDATA", sep = "");
    ss <- load(fnin)
    if (ss == "dt.p") {
        dfmm <- df.m %>% select(date = datekey, pe, de, isMagic);
        dfpp <- data.frame(dt.p) %>%
                          select(date, open, close) %>%
                          mutate(open = f.cleanPrices(open),
                                 close=f.cleanPrices(close));
        df.t <- merge(dfpp, dfmm, by = "date", all.x = TRUE) %>%
           mutate(nopen = na.locf(lead(open), na.rm = FALSE),
                  pgrat = f.safeDiv(nopen, lag(nopen)),
                  curpos = 0, posage = 0, closedpos = 0, ror = 0)
        #df.t[is.na(df.t)] <- 0;
        df.t$index <- seq_len(nrow(df.t));
        initpos <- 10000
        N <- nrow(df.t);
        v.posage <- v.closedpos <- v.curpos <- numeric(N);
        v.close <- df.t$close
        v.shares <- numeric(N);
        v.trades <- numeric(N);
        v.holding <- numeric(N);
        v.pon <- numeric(N);
        v.nopen <- df.t$nopen;
        filter(df.t, isMagic == TRUE);
        startpos <- 0;
        #build the signaling mechanism outside the loop
        lMagic <- f.generateDailyMagic(df.t)
        df.t$pe <- lMagic$v.pe
        df.t$de <- lMagic$v.de;
        df.t$isMagic <- lMagic$v.isMagic;
        v.isMagic <- df.t$isMagic;
        t <- 0;
        while (t < N) {
            t <- t + 1;
            if (t > 1) {
                v.shares[t] <- v.shares[t - 1];
                v.pon[t] <- v.pon[t - 1]
                v.curpos[t] <- v.shares[t] * (v.close[t]);
                v.closedpos[t] <- v.closedpos[t - 1];
            }
            if (v.shares[t] != 0) {
                v.holding[t] <- 1;
                v.posage[t] <- v.posage[t - 1] + 1;

                if ((v.posage[t] >= 504) || (t == N) ||
                                       (v.curpos[t] >= 1.5 * initpos)) {
                    v.closedpos[t] = v.closedpos[t] + v.curpos[t] - startpos;
                    v.posage[t] <- 0;
                    v.curpos[t] <- 0;
                    startpos <- 0;
                    v.pon[t] <- v.nopen[t]
                    v.shares[t] <- 0;
                }
            }
            if (v.curpos[t] == 0) {
                if (t != N) {
                    if (v.isMagic[t]) {
                        v.shares[t] <- floor(initpos / v.nopen[t]);
                        v.trades[t] <- 1;
                        v.pon[t] <- v.nopen[t];
                        
                        v.curpos[t] <- v.shares[t] * v.nopen[t];
                        startpos <- v.curpos[t];
                        #cat("---------EVENT--------",
                        #      t,df.t$date[t],v.shares[t],"\n");
                    }
                }
            }
            #cat(t,v.date[t],"Shares=",v.shares[t],v.close[t],
            #        v.curpos[t],v.closedpos[t],"\n");    
        }
        profit <- tail(v.closedpos, n = 1);
        investment <- initpos * sum(v.holding) / 252.0;
        ror <- profit / investment;
        v.ror <- f.roc(v.curpos);
        df.t$shares <- v.shares;
        df.t$curpos <- v.curpos;
        df.t$closedpos <- v.closedpos;
        df.t$trades <- v.trades;
        df.t$holding <- v.holding;
        df.t$posage <- v.posage;
        df.t$ror <- f.roc(v.curpos);
        return(df.t)
    }
    #end dtp
}
f.CompanyTrades <- function(df.m) {
    head(df)
    df.m <- df.m %>% select(-isLowPE, - isLowDE) %>%
            mutate(openpos = 0, posage = 0,closedpos = 0, trades = 0)

    initpos <- 10000
    t <- 0

    N <- nrow(df.m);
    v.curpos <- numeric(N);
    v.posage <- numeric(N);
    v.closedpos <- numeric(N);
    v.isMagic <- df.m$isMagic;
    v.pgrat <- df.m$pgrat;
    v.price <- df.m$price;
    v.pe    <- df.m$pe
    v.trades <- numeric(N);
    v.holding <- numeric(N);
    t <- 0;
    while (t < N) {
        t <- t + 1;
        if (t > 1) {
            v.curpos[t] <- v.curpos[t - 1] * v.pgrat[t];
            v.closedpos[t] <- v.closedpos[t - 1];
        }
        if (v.curpos[t] != 0) {
            v.holding[t] <- 1;
            v.posage[t] <- v.posage[t - 1] + 1;
            if (v.curpos[t] >= 1.50 * initpos) {
                v.closedpos[t] = v.closedpos[t] + v.curpos[t] - initpos
                v.posage[t] <- 0
                v.curpos[t] <- 0
            }
            if (v.posage[t] >= 8) {
                v.closedpos[t] = v.closedpos[t] + v.curpos[t] - initpos
                v.posage[t] <- 0
                v.curpos[t] <- 0

            }
            if (t == N) {
                if (v.curpos[t] != 0) {
                    v.closedpos[t] <- v.closedpos[t] + v.curpos[t] - initpos
                }
            }
        }
        if (v.curpos[t] == 0) {
            if (t != N) {
                if (v.isMagic[t]) {
                    v.curpos[t] <- initpos;
                    v.trades[t] <- 1;
                }
            }
        }
        # cat(t,as_date(df.m$datekey[t]),v.isMagic[t],v.price[t],
        #     "cpos=",v.curpos[t],"age=",v.posage[t],"closedProfs=",
        #      v.closedpos[t],"\n");
    }
    df.m$openpos <- v.curpos;
    df.m$posage <- v.posage;

    df.m$closedpos <- v.closedpos;
    df.m$trades <- v.trades;
    df.m$holding <- v.holding;
    return(df.m);
}
f.isMagic <- function(pe, de) {
    ismagical = FALSE;
    if (pe > 0.001) {
        if (pe < 10) {
            if (de >= 0.0) {
                if (de < 0.5) {
                    ismagical = TRUE;
                }
            }
        }

    }
    return(ismagical)
}
f.GenerateAvailability <- function(df.SPY) {
    dirMagic <- paste(dirResults, "/Magic/Companies", sep = "");
    v.magiccofn <- list.files(path = dirMagic);
    v.magictickers <- str_sub(v.magiccofn, start = 1L, end = -7L);
    dt.ComMagic <- dt.Common %>% arrange(Sector, Industry, CapGroup,
     Ticker) %>% filter(Ticker %in% v.magictickers);
    v.indMagic <- unique(dt.ComMagic$Industry);
    #Build the performance by Industry
    nind <- 0;
    df.All <- df.SPY;
    for (ind in v.industries) {
        nind <- nind + 1;
        ind <- v.industries[nind];
        theseCommon <- dt.ComMagic %>% filter(Industry == ind)
        v.tickers <- theseCommon$Ticker;
        df.ind <- df.SPY;
        ncompany <- 0
        for (company in v.tickers) {
            ncompany <- ncompany + 1;
            company <- v.tickers[ncompany];
            fnin <- paste(dirResults, "/Magic/Companies/",
                      company, ".RDATA", sep = "");
            ss_df.DailyTrades <- load(fnin);
            df.co <- df.DailyTrades %>%
                   as_tbl_time(index = date) %>%
                   select(date, isMagic) %>%
                   mutate(active = ifelse(isMagic, 1, 0)) %>%
                   select(date, active);

            # cat(ncompany,company,"\n");
            df.ind <- merge(df.ind, df.co, by.x = "Date",
                        by.y = "date", all.x = TRUE)
            # names(df.ind$active) <- company;
            #merge(x, y, by = NULL, by.x = NULL, by.y = NULL,
            #all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x", ".y"),
            #allow.cartesian = getOption("datatable.allow.cartesian"), # default FALSE
            #df <- merge(df.SPY, df.co, by.x = "Date", by.y = "date", all.x = TRUE)
            #r <- f.roc(df.SPY$Close);
            #rcum <- cumsum(r);
            #plot.ts(rcum)
        }
        df.ind[is.na(df.ind)] <- 0;
        names(df.ind) <- c("Date", "Close", v.tickers);
        fnout <- paste(dirResults, "/Magic/Available/", ind, ".csv", sep = "");
        write_csv(df.ind, path = fnout);
        df.ind <- select(df.ind, - Close);
        cat(nind, ind, ncol(df.ind), "\n");
        df.All <- merge(df.All, df.ind, by = "Date");
    }
    return(df.All);
}