###############

entries = getMapEntries(con)

br = as(entries[["Berkeley"]], "GoogleMapsDoc")

mm = as(entries[[1]], "GoogleMapsDoc")



##################################
# posting
topLinks = getNodeSet(doc, "/a:feed/a:link", c(a = AtomNS))
rel = sapply(topLinks, xmlGetAttr, "rel")
postLink = xmlGetAttr(topLinks[grep("#post", rel)][[1]], "href")


# Posting a KML file. This currently fails with a 400 status error.
# Perhaps this is related to http://www.google.com/support/forum/p/maps/thread?hl=en&tid=39ac7182b269a3e7
# where this stopped working.
map = paste(readLines("map.kml"), collapse = "\n")
# 400 Bad Request: Invalid request URI or header, or unsupported nonstandard parameter.
q = httpPOST(postLink, postfields = map, httppost = 1L, curl = con,
              verbose = TRUE,
              httpheader = c('GData-Version' = '2.0',
                             'Content-Type' = 'application/vnd.google-earth.kml+xml',
                             "Authorization" = sprintf('GoogleLogin auth="%s"', con@auth)))


q = curlPerform(url = postLink,
                postfields = map, post = 1L, httppost = 1L,  curl = con,
                verbose = TRUE,
                httpheader = c('GData-Version' = '2.0',
                              'Content-Type' = 'application/vnd.google-earth.kml+xml',
                              "Authorization" = sprintf('GoogleLogin auth="%s"', con@auth)))



 # This works.
val = '<entry xmlns="http://www.w3.org/2005/Atom"><title>Bike Ride, 10 Years Old</title><summary></summary></entry>'
q = curlPerform(url = postLink,
                postfields = val, post = 1L, httppost = 1L,  curl = con,
                verbose = TRUE,
                httpheader = c('Content-Type' = 'application/atom+xml',
                               "Authorization" = sprintf('GoogleLogin auth="%s"', con@auth)))

q = httpPOST(postLink,
                postfields = val, post = 1L, httppost = 1L,  curl = con,
                verbose = TRUE,
                httpheader = c('Content-Type' = 'application/atom+xml',
                               "Authorization" = sprintf('GoogleLogin auth="%s"', con@auth)))


csv = 'name,latitude,longitude,description
Hello,-77.066395,-11.968312,Greetings from Lima!
There,145.34502,-38.51512,Out There Down Under
How,-88.421001,44.970465,How is Wisconsin?
Are,13.084501,63.399164,Sorry about that
You,140.637898,42.842568,I love you Hokkaido'

# works.
q = curlPerform(url = postLink,
                postfields = csv, post = 1L, httppost = 1L,  curl = con,
                verbose = TRUE,
                httpheader = c('Content-Type' = 'text/csv',
                               "Authorization" = sprintf('GoogleLogin auth="%s"', con@auth)))

# okay
q = httpPOST(postLink, 
             postfields = csv, post = 1L, httppost = 1L,  curl = con,
             verbose = TRUE,
             httpheader = c('Content-Type' = 'text/csv',
                            'GData-Version' = '2.0',
                            "Authorization" = sprintf('GoogleLogin auth="%s"', con@auth)))
           
