# Computing the return matrix
tsfunc <- function(v) (v[2,] / v[1,]) - 1
RETURN <- mcTimeSeries( DATA[["Close"]], tsfunc, FALSE, 2, workers )


# Computing a simple moving average
SMA <- mcTimeSeries( DATA[["Close"]], mean, TRUE, 20, workers )


# Computing an MACD, n1 = 5, n2 = 34
tsfunc <- function(v) mean(v[(length(v) - 4):length(v)]) - mean(v)
MACD <- mcTimeSeries( DATA[["Close"]], tsfunc, TRUE, 34, workers )


# Computing Bollinger Bands, n = 20, scale = 2
SDSeries <- mcTimeSeries(DATA[["Close"]], function(v) sd(v), TRUE, 20, workers)
upperBand <- SMA + 2 * SDSeries
lowerBand <- SMA - 2 * SDSeries


# Computing custom indicator as in Listing 4.3
tsfunc <- function(v) cor(v, length(v):1)^2 * ((v[length(v)] - v[1])/length(v))
customIndicator <- mcTimeSeries( DATA[["Close"]], tsfunc, TRUE, 10, workers )


# Computing Chaikin Money Flow, n = 20, (Using CMFfunc() function from Listing 4.5)
cols <- ncol(DATA[["Close"]])
CMFseries <- mcTimeSeries( cbind(DATA[["Close"]],
                                 DATA[["High"]],
                                 DATA[["Low"]],
                                 DATA[["Volume"]]),
                            function(v) CMFfunc(v[,(1:cols)],
                                                v[,(cols+1):(2*cols)],
                                                v[,(2*cols + 1):(3*cols)],
                                                v[,(3*cols + 1):(4*cols)]),
                            FALSE, 20, workers)

