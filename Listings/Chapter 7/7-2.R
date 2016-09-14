SUBDATA <- lapply(DATA, function(v) v[-(1:3500),])
SUBRETURN <- RETURN[-(1:3500),]

n1 <- 5
n2 <- 34
nSharpe <- 20
shThresh <- 0.80

INDIC <- mcTimeSeries(SUBDATA[["Close"]],
                      function(v) mean(v[(n2 - n1 + 1):n2]) - mean(v),
                      TRUE, n2, workers)


entryfunc <- function(v){
  cols <- ncol(v) / 2
  as.numeric(v[1,1:cols] <= 0 &
               v[2,1:cols] > 0 &
               v[2,(cols+1):(2*cols)] >
               quantile(v[2,(cols+1):(2*cols)], shThresh, na.rm  = TRUE)
             )
}

FAVOR <- mcTimeSeries(SUBRETURN,
                      function(v) mean(v, na.rm = TRUE)/sd(v, na.rm = TRUE),
                      TRUE, nSharpe, workers)

ENTRY <- mcTimeSeries(cbind(INDIC, FAVOR),
                      entryfunc,
                      FALSE, 2, workers)

EXIT <- zoo(matrix(0, ncol=ncol(SUBDATA[["Close"]]), nrow=nrow(SUBDATA[["Close"]])),
            order.by = index(SUBDATA[["Close"]]))
names(EXIT) <- names(SUBDATA[["Close"]])

K <- 10

maxLookback <- max(n1, n2, nSharpe) + 1

RESULTS <- simulate(SUBDATA[["Open"]], SUBDATA[["Close"]],
                    ENTRY, EXIT, FAVOR,
                    maxLookback, K, 100000,
                    0.0005, 0.01, 3.5, 0,
                    TRUE, 0)

