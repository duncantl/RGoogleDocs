# See http://code.google.com/apis/maps/documentation/mapsdata/developers_guide_protocol.html

library(RGoogleDocs)
library(RCurl)
library(XML)

AtomNS = "http://www.w3.org/2005/Atom"

setOldClass("GoogleMapsEntry")
setOldClass(c("GoogleMapsDoc", "XMLInternalDocument", "XMLAbstractDocument"))

makeEntry =
function(node)
{
  ans = structure(xmlToList(node), class = "GoogleMapsEntry")
  isLink = names(ans) == "link"
  names(ans)[isLink] = sprintf("link.%s", sapply(ans[isLink], function(x) gsub(".*#", "#", x["rel"])))
  ans
}

getMapInfo = getMapEntries =
function(con = getGoogleDocsConnection(service = "local"),
         txt = getURLContent("http://maps.google.com/maps/feeds/maps/default/full",  curl = con))
{
  doc = xmlParse(I(txt))
  getNodeSet(doc, "//a:entry", c(a = AtomNS))

  entries = xpathApply(doc, "//a:entry",
                       makeEntry,
                      namespaces = c(a = AtomNS))
  names(entries) = sapply(entries, "[[", "title")
  entries
}
  

if(FALSE) {
  auth = getGoogleAuth(service = "local")
  con = getGoogleDocsConnection(auth = auth)
} else 
  con = getGoogleDocsConnection(service = "local")




setAs("GoogleMapsEntry",  "GoogleMapsDoc",
       function(from) {
         tt = getURLContent(from$link.self["href"], curl = con)
         doc = xmlParse(tt, asText = TRUE)
         class(doc) = c("GoogleMapsDoc", class(doc))  # last so print methods work.
         doc
       })

setGeneric("title<-",
            function(x, ..., value)
              standardGeneric("title<-"))

setMethod("title<-", "GoogleMapsEntry",
            function(x, ..., value) {
                doc = as(x, "GoogleMapsDoc")
                ti = getNodeSet(doc, "//a:title", c(a = AtomNS))
                if(length(ti) == 0) {

                }
                xmlValue(ti) = value

                
                q = httpPUT(getPostLink(x), curl = con,
                             postfields = saveXML(doc),
                             httpheader = c('Content-Type' = 'application/atom+xml',
                                            "Authorization" = sprintf('GoogleLogin auth="%s"', con@auth)))                
            })

setMethod("title<-", "GoogleMapsDoc",
            function(x, ..., value) {
                   
            })


getPostLink =
  #
  # What is doc ? the document for a particular map.
  # This is for the top-level "folder".
  # We may also want it for a particular document.
  #
function(doc)
{
  topLinks = getNodeSet(doc, "/a:feed/a:link", c(a = AtomNS))
  rel = sapply(topLinks, xmlGetAttr, "rel")
  postLink = xmlGetAttr(topLinks[grep("#post", rel)][[1]], "href")
}


