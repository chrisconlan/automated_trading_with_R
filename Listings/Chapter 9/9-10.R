# Example is not executable.
# For example purposes only.
library(ROAuth)


# Requesting with key-secret and access-request pair
reqURL <- "requestUrl"
accessURL <- "accessUrl"
authURL <- "authenticationUrl"
cKey <- "consumerKey"
cSecret <- "consumerSecret"


credentials <- OAuthFactory$new(consumerKey=cKey,
                                consumerSecret=cSecret,
                                requestURL=reqURL,
                                accessURL=accessURL,
                                authURL=authURL,
                                needsVerifier=FALSE)
credentials$handshake()

# Send GET Request to URL
testURL <- "http://someurl.com/some parameters"
credentials$OAuthRequest(testURL, "GET")


# Send GET Request to URL
testURL <- "http://someurl.com/some un-encoded parameters"
credentials$OAuthRequest(testURL, "GET")
