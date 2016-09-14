# Using rollapply() element-by-element
RETURN <- rollapply(DATA[["Close"]],
            width = 2,
            FUN = function(v) (v[2]/v[1]) - 1,
            align = "right",
            by.column = TRUE,
            fill = NA)
# 105.77 seconds



# Using rollapply() row-by-row
RETURN <- rollapply(DATA[["Close"]],
            width = 2,
            FUN = function(v) (v[2,]/v[1,]) - 1,
            align = "right",
            by.column = FALSE,
            fill = NA)
# 65.37 seconds
