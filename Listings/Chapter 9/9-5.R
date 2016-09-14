# Descend the tree to each individual stock quote
getNodeSet(doc, "/query/results/quote")

# Get the second quote
getNodeSet(doc, "/query/results/quote[2]")

# Descend to the third level of the tree, get second element
getNodeSet(doc, "/*/*/*[2]")

# Get all nodes named "quote" regardless of level
getNodeSet(doc, "//quote")

# Get all node with Symbol = AAPL attribute
getNodeSet(doc, "/query/results/quote[@Symbol = 'AAPL']")

# Get the last quote
getNodeSet(doc, "/query/results/quote[last()]")

# Get the first 3 quotes
getNodeSet(doc, "/query/results/quote[position() <= 3]")

# Get all quotes with closing price less than 40
getNodeSet(doc, "/query/results/quote[./Close < 40]")

# Get all closing prices less than 40
getNodeSet(doc, "/query/results/quote[./Close < 40]/Close")

