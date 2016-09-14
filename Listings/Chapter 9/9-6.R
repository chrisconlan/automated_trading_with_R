# Descend the tree to this point
root <- "/query/results/quote"

# Descend to each of these leaves for every node in root
leaves <- c("./Date", "./Open", "./High", "./Low",
                 "./Close", "./Volume", "./Adj_Close")

# Get data in list
df <- getNodeSet(doc, root, fun = function(v) xpathSApply(v, leaves, xmlValue))

# Get symbols as attributes
sym <- getNodeSet(doc, root, fun = function(v) xpathSApply(v, ".", xmlAttrs))

# This is equivalent to the above line in this case
# sym <- as.character(getNodeSet(doc, "/query/results/quote/@Symbol"))

# Organize as data frame
df <- data.frame(t(data.frame(df)), stringsAsFactors = FALSE)

# Append stock symbols
df <- cbind(unlist(sym), df)
df[,3:8] <- lapply(df[3:8], as.numeric)
df[,1] <- as.character(df[,1])

# Fix names
rownames(df) <- NULL
colnames(df) <- c("Symbol", substring(leaves, 3))

