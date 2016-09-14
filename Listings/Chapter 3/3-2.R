temp <- c(DATA[["Close"]][index(DATA[["Close"]]) %in% c("2015-11-23",
                                                        "2015-11-24",
                                                        "2015-11-25"), "KORS"],
          zoo(NA, order.by = strptime("2015-11-26", "%Y-%m-%d")) ,
          DATA[["Close"]][index(DATA[["Close"]]) %in% c("2015-11-27"), "KORS"],
          zoo(NA, order.by = strptime(c("2015-11-28", "2015-11-29"), "%Y-%m-%d")),
          DATA[["Close"]][index(DATA[["Close"]]) %in% c("2015-11-30",
                                                        "2015-12-01",
                                                        "2015-12-02"), "KORS"]) 
