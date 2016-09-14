oKey <- "oauthKey"
oSecret <- "oauthSecret"
cKey <- "consumerKey"
cSecret <- "consumerSecret"

credentials <- OAuthFactory$new(consumerKey = cKey,
                                consumerSecret = cSecret,
                                oauthKey = oKey, 
                                oauthSecret = oSecret,
                                needsVerifier=FALSE)

# Manually declare authentication as complete
credentials$handshakeComplete <- TRUE

# Send a FIXML message through OAuth to testURL with POST request
aFIXMLmessage <- c("<FIXML xmlns=...>content</FIXML>")
testURL <- "https://testurl.com/"
credentials$OAuthRequest(testURL, "POST", aFIXMLmessage)
