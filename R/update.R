updateDoc =
  #
  #
  #
function(doc, con,  docType, asText = FALSE)
{

}

addFolder =
  #
  # Create a new folder.
  #
  # How do we add it to an existing folder.
  #
function(name, con, url = GoogleURLs["documents"])
{
    # Vectorize this.
  if(length(name) > 1) {
    return(lapply(name, addFolder, con, url))
  }
  
  e = newXMLNode("entry", namespaceDefinitions = nsDefines[1])
  newXMLNode("title", name, parent = e)
  newXMLNode("category", attrs = c(scheme="http://schemas.google.com/g/2005#kind",
                                   term="http://schemas.google.com/docs/2007#folder",
                                   label="folder"),
               parent = e)
  
  txt = saveXML(e)
  curl = dupCurlHandle(con)
  b = basicTextGatherer()
  h = basicTextGatherer()
  curlPerform(.opts = list(postfields = txt,
                           headerfunction = h$update,
                           writefunction = b$update,                
                           url = url,
                           httpheader = c('Content-Type' = "application/atom+xml",
                                          Authorization = paste("GoogleLogin auth=", con@auth, sep = ""))),
               curl = curl)

  status = parseHTTPHeader(h$value())  
  if(floor(as.numeric(status["status"])/100) == 2)  {
    doc = xmlParse(b$value())
    getXMLDocEntryInfo(xmlRoot(doc), con)
  } else
    stop("Failed to add folder: ", status["statusMessage"])
}

makeMetaNode =
   #XXX be more careful here about how we create these, i.e. lists and multi-element vectors.
   # partially done.
function(id, val, parent) {

  if(is.character( val )) {
    if(length(val) > 1) {
      if(length(names(val))) {
         ans = newXMLNode(id, parent = parent)
         lapply(names(val),
                 function(x)  newXMLNode(x, val[x], parent = ans))
         ans
      } else
         warning("Haven't handled this case yet")
    } else
      newXMLNode(id, val, parent = parent)
  } else if(is.list(val)) {
      # e.g. list(author = c(name = "Duncan Temple Lang", email = "duncan@wald.ucdavis.edu"))
     ans = newXMLNode(id, parent = parent)
     mapply(makeMetaNode, names(val), val, ans)
     ans
  }
}  

setGeneric("rename", function(x, value, ...) standardGeneric("rename"))
setMethod("rename", "GoogleDocumentDescription",
            function(x, value, con = getGoogleDocsConnection(), ...) {
              setMetaData(x, con, title = value, ...)
            })

setMetaData =
function(doc, con, ..., .params = list(...))
{
 nsDefines = "http://www.w3.org/2005/Atom" # temporary.
 e = newXMLNode("entry", namespaceDefinitions = nsDefines[1])
 newXMLNode("category", attrs = c(scheme = "http://schemas.google.com/g/2005#kind",
                                  term = "http://schemas.google.com/docs/2007#document"),
              parent = e)

  mapply(makeMetaNode, names(.params), .params, MoreArgs = list(parent = e))

  url = doc@edit["href"]
  h = basicTextGatherer()
  b = basicTextGatherer()
  d = debugGatherer()   
  curlPerform(url = url,
              httpheader = c('Content-Type' = "application/atom+xml",
                             Authorization = paste("GoogleLogin auth=", con@auth, sep = "")),
              postfields = saveXML(e),
              customrequest = "PUT",
              headerfunction = h$update,
              debugfunction = d$update,
              writefunction = b$update)

  status = parseHTTPHeader(h$value())

  if( as.numeric(status["status"])/100 == 2 ) {
      # Want to update the meta-data here.
    doc
  } else {
      #XXX curl error here.
    stop("Failed to update meta-data for document ", doc@title)
  }
}

setMethod("[<-", "GoogleDocumentDescription",
          function(x, i, j, ..., con,  value) {
               #XXX
#            setMetaData(x, con = x@connection, .params = list(...))
            p = structure(value, names = c(i, if(!missing(j)) j, unlist(list(...))))
            setMetaData(x, con = x@connection, .params = p)
            x
          })

setMethod("$<-", "GoogleDocumentDescription",
          function(x, name, value) {
              #XXX con.
            setMetaData(x, x@connection, .params = structure(list(value), names = name))
            x
          })


moveToFolder  =
  #
  # z = moveToFolder(docs$bob, docs$MyDirectory, con)
  #
function(doc, to, con = if(is(doc, "GoogleDocumentDescription")) doc@connection
                        else getGoogleDocsConnection())
{
  if(is.character(doc) || is.character(to)) {
    docs = getDocs(con)
    if(is.character(doc))
      doc = docs[[doc]]
    if(is.character(to))
      to = docs[[to]]    
  }
  
  if(!missing(con))
     con = new("GoogleDocsConnection", ref = dupCurlHandle(con)@ref, auth = con@auth)

  e = newXMLNode("entry", namespaceDefinitions = nsDefines[1])
  newXMLNode("id", doc@id, parent = e)
  newXMLNode("category", attrs = doc@category[c("scheme", "term")],
                parent = e)


  h = basicTextGatherer()
  b = basicTextGatherer()
  d = debugGatherer()   
  curlPerform(url = to@content['src'],
              httpheader = c('Content-Type' = "application/atom+xml",
                             Authorization = paste("GoogleLogin auth=", con@auth, sep = "")),
              postfields = saveXML(e),
              customrequest = "POST",
              headerfunction = h$update,
              debugfunction = d$update,
              writefunction = b$update,
              verbose = TRUE)

  status = parseHTTPHeader(h$value())
  if(floor(as.numeric(status["status"])/100) == 2) {
    d = xmlParse(b$value())
    getXMLDocEntryInfo(xmlRoot(d), con)
  } else
    stop("Failed to move ", doc@title, " to ", to@title)
}
