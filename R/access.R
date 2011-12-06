# See  http://code.google.com/apis/documents/docs/2.0/schema/acl_atom.rnc

getAccess =
function(doc, con = doc@connection)
{
   txt = getURL(doc@access['href'], curl = getCurlCon(con),
                        # make certain to reset after other requests may have changed this.
                 .opts = curlOpts(con, customrequest = "GET",
                                       followlocation = TRUE,
                                       nobody = TRUE, httpget = TRUE))
   doc = xmlParse(txt)
   entries = getNodeSet(doc, "//x:entry", "x")

   structure(sapply(entries, function(x) xmlGetAttr( x[["scope"]], "value", addNamespace = FALSE)),
             names = sapply(entries, function(x) xmlGetAttr( x[["role"]], "value", addNamespace = FALSE)))
}


setAccess =
  # Why do these two 
  #setAccess(docs$foo, writer = 'duncan@wald.ucdavis.edu', reader = 'everyone')
  #                  owner                    writer                    reader 
  #"dtemplelang@gmail.com" "duncan@wald.ucdavis.edu"                "everyone" 
  # getAccess(docs$TwoSheets, getGoogleDocsConnection())
  #                  owner                  reader 
  # "dtemplelang@gmail.com"              "everyone" 
function(doc, ..., .perms = list(...), con = doc@connection)
{
  if(is.character(doc) )
    doc = getDocs(con)[[doc]]
  
  type = names(.perms)
  who = .perms
  
  ans =
   lapply(seq(along = .perms),
          function(i) {
            e = makeAccessEntry(who[[i]], type[i])
            postAtomEntry(e, doc@access['href'], con)
         })
  getAccess(doc, con)
}

makeAccessEntry =
function(who, type)
{
  e = newXMLNode("entry",
                 namespaceDefinitions = c("http://www.w3.org/2005/Atom",
                                          gAcl='http://schemas.google.com/acl/2007'))
  
  newXMLNode("category", attrs = c(scheme = 'http://schemas.google.com/g/2005#kind',
                                   term = 'http://schemas.google.com/acl/2007#accessRule'),
               parent = e)
  
  newXMLNode("gAcl:role", attrs = c(value = type), parent = e)
  newXMLNode("gAcl:scope", attrs = c(type = 'user', value = who), parent = e)

  e
}


postAtomEntry =
function(node, url, con)
{
  b = basicTextGatherer()
  h = basicTextGatherer()


  header = c('Content-Type' = "application/atom+xml",
             'User-Agent' = paste('RGoogleDocs', options()$HTTPUserAgent))
  if(is(con, "AuthenticatedGoogleDocumentsConnection"))
     header['Authorization'] = paste("GoogleLogin auth=", con@auth, sep = "")
  
  curlPerform(url = url,
              customrequest = "POST",
              postfields = saveXML(node),
              httpheader = header,
              headerfunction = h$update,
              writefunction = b$update,
              curl = con)
  
  list(status = parseHTTPHeader(h$value()),
       body = b$value())
}

addWriter =
function(doc, who, con)
{
  if(is.character(doc))
    doc = getDocs(con)[doc]
  
  e = makeAccessEntry(who, 'writer')

  ans = postAtomEntry(e, doc@access['href'], con)

  if(as.integer(as.numeric(ans$status['status'])/100) != 2)
     stop('failed to grant authorization to ', who, ' for document ', doc@title)

  xmlParse(ans$body$value())
}

removeAccess =
function(doc, who, con = doc@connection)
{
  h = basicTextGatherer()
  b = basicTextGatherer()
  
  u = paste(doc@access['href'], "/", paste("user%3A", gsub('@', '%40', who), sep = ""), sep = "")
  curlPerform(url = u, customrequest = "DELETE", headerfunction = h$update,
               writefunction = b$update, curl = con)

  parseHTTPHeader(h$value())
}
