library(XML)

# Generate the XML message in Listing 9.7
out <- newXMLNode("FIXML",
                  namespaceDefinitions =
                    "http://www.fixprotocol.org/FIXML-5-0-SP2")

newXMLNode("Order",
           attrs = c(TmInForce = 0, Typ = 1, Side = 1, Acct=999999),
           parent = out)

newXMLNode("Instrmt",
           attrs = c(SecTyp = "CS", Sym = "AAPL"),
           parent = out["Order"])

newXMLNode("OrdQty",
           attrs = c(Qty = 100),
           parent = out["Order"])

print(out)


# Extra example for how to insert content in non-self-closing nodes
newXMLNode("extraInfo", "invalid content.", parent = out["Order"])
print(out)
