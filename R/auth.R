# Add:
#   Error handling
#   get Authorization field from httpheader of an existing curl handle
#   check uploading binary files with a raw vector.

################################

GoogleServiceClasses =
 c(writely = "GoogleDocsConnection",
   wise = "GoogleSpreadsheetsConnection")

setClass("AbstractGoogleDocumentsConnection", contains = "CURLHandle")
setClass("UnauthenticatedGoogleDocumentsConnection", contains = "AbstractGoogleDocumentsConnection")

setClass("GoogleDocsAuthentication", contains = "character")
setClass("AuthenticatedGoogleDocumentsConnection", representation(auth = "character", service = "character"), contains = "AbstractGoogleDocumentsConnection")


 # Either NULL or a GoogleDocumentsConnection, either authenticated or unauthenticated.
if(TRUE)  
  setClassUnion("GoogleDocumentsConnection", c("AbstractGoogleDocumentsConnection", "NULL"))
#  setClass("GoogleDocumentsConnection", contains = "UnauthenticatedGoogleDocumentsConnection")



setClass("GoogleDocsConnection", contains = "GoogleDocumentsConnection")
setClass("GoogleSpreadsheetsConnection", contains = "GoogleDocumentsConnection")



#setOldClass("GoogleDocument")
setClass("GoogleDocumentDescription",
          representation("id" = "character",
                         "published" = "POSIXt",
                         "updated" = "POSIXt",
                         "category" = "character",
                         "title" = "character",
                         "content" = "character",
                         "alternate" = "character",
                         "self" = "character",
                         "edit" = "character",
                         "edit-media" = "character",
                         "author"  = "character",
                         "feedLink" = "character",
                         access = "character",
                         connection = "GoogleDocumentsConnection"))

setClass("GoogleDocument", contains = "GoogleDocumentDescription")
setClass("GooglePresentation", contains = "GoogleDocumentDescription")
setClass("GoogleSpreadsheet",
          representation(worksheetsfeed = "character"), contains = "GoogleDocumentDescription")

setClass("GoogleFolder", contains = "GoogleDocumentDescription")


######################################################

getCurlCon =
function(con)
{
  if(is.null(con) || is(con, "UnauthenticatedGoogleDocumentsConnection"))
    getCurlHandle()
  else
    con
}


getGoogleAuth =
function(login = getOption("GoogleDocsPassword"), password, service = "writely", appID = "R-GoogleDocs-0.1", error = TRUE)
{
  if(!missing(login) && missing(password) && length(names(login)) > 0 ) {
    password = login
    login = names(password)
  }
    
  if(missing(login) && missing(password)) {

    e = getOption("GoogleDocsPassword")
    if(length(e)) {
      if(length(e) == 1) {
        login = names(e)
        password = e[1]
      } else {
        login = e[1]
        password = e[2]
      }
    } else if( (e <- Sys.getenv("GOOGLE_DOCS_PASSWORD")) != "") {
        e = strsplit(e, "=")[[1]]
        if(length(e) != 2)
          stop("Google Docs password and login must be specified")
        login = e[1]
        password = e[2]
    } else
       if(error)
         stop("You need to specify the Google Documents login and password")
       else
         return(NULL)
  }
  
  ans =  getForm("https://www.google.com/accounts/ClientLogin",
        	 accountType = "HOSTED_OR_GOOGLE",
	         Email = login,
	         Passwd = password,
	         service = service,
	         source = appID,
	        .opts = list(ssl.verifypeer = FALSE))
   ans = parseAuth(ans)["Auth"]

   if(is.na(ans))
     if(error)
       Stop(paste("can't login to Google account with login", login), "GoogleLoginError")
     else
       return(NULL)

   ans = new("GoogleDocsAuthentication", ans)
   attr(ans, "service") = service
   ans
}

Stop   =
function(msg, class = character(), ...)
{
  e = simpleError(msg)

  if(length(list(...)))
    e = c(e, ...)

  class(e) = c(class, class(e))  
  e
}


getConnection = getGoogleDocsConnection = 
function(..., auth = NA, error = TRUE)
{
  if(!missing(auth) && (is.na(auth) || length(auth) == 0 || auth == ""))
    #return(getCurlHandle())
    return(new("GoogleDocumentsConnection", new("UnauthenticatedGoogleDocumentsConnection", getCurlHandle())))
  
  if(missing(auth))  {
    args = list(...)
    if(length(args) > 0 && inherits(args[[1]], "GoogleDocsAuthentication"))
      auth = args[[1]]
    else {
      auth = getGoogleAuth(..., error = error)
      if(is.null(auth))
        if(error)
          stop("Couldn't get authentication")
        else
          return(NULL)
    }
  }

  
  h = getCurlHandle(.opts = curlOpts(auth))


  k = if(!is.null(attr(auth, "service")))
         GoogleServiceClasses[attr(auth, "service")]
      else
         "GoogleDocumentsConnection"

  k =  "AuthenticatedGoogleDocumentsConnection"
  tmp = new(k, ref = h@ref, auth = auth)
  if(!is.null(attr(auth, "service")))
     tmp@service = auth@service

  tmp
}


curlOpts =
function(auth, ...)
{
  h = c('User-Agent' = paste('RGoogleDocs', options()$HTTPUserAgent)) # ,   'GData-Version' = '3.0')

  if(is(auth, "AuthenticatedGoogleDocumentsConnection")) {
    auth = auth@auth
  }
  if(is(auth, "character") && length(auth) > 0)
    h['Authorization'] = paste("GoogleLogin auth=", auth, sep = "")      

  list(httpheader = h, ...)
}

GoogleURLs =
  c(spreadsheets = "https://spreadsheets.google.com/feeds/spreadsheets/private/full",
    wise = "https://spreadsheets.google.com/feeds/spreadsheets/private/full",
    documents = "https://docs.google.com/feeds/documents/private/full",
    writely = "https://docs.google.com/feeds/documents/private/full")

getDocs = 
function(curl = getConnection(auth), folders = TRUE, as.data.frame = FALSE, auth = getGoogleAuth(service = service),
         what = GoogleURLs[service], ..., service = "writely")
{
   if(missing(what)) {
     if(is(curl, "AuthenticatedGoogleDocumentsConnection") && !is.na(curl@service))
       what = curl@service
     else if(!missing(auth) && is(auth, "AuthenticatedGoogleDocumentsConnection") && !is.na(auth@service))
       what = auth@service
   }
   
   if(what %in% names(GoogleURLs)) 
      what = GoogleURLs[what]
   else if(is(curl, "GoogleSpreadsheetsConnection"))
      what = GoogleURLs["spreadsheets"] 
  
   curlSetOpt(customrequest = "GET", curl = curl)
   h = basicTextGatherer()
   if(folders)
      what = paste(what, 'showfolders=true', sep = "?")
   
   x = getURL(what, curl = curl, headerfunction = h$update, followlocation = TRUE, ...)
   status = parseHTTPHeader(h$value())
   if(floor(as.numeric(status[["status"]])/100) != 2)
     stop("problems connecting to get the list of documents: ", status["statusMessage"], " (", status["status"], ")")
   
   doc = xmlParse(x, asText = TRUE)       

   if(toupper(xmlName(xmlRoot(doc))) == "HTML")
      stop("Can't get document list. Is the connection still valid? Perhaps initialize a new connection.")
   
   convertDocList(doc, curl, as.data.frame, what)
}

convertDocList =
function(doc, curl, as.data.frame = FALSE, type = "documents")
{
   x = structure(xpathApply(doc, "//x:entry",  getXMLDocEntryInfo, curl, namespaces = "x"), 
                 class = c("GoogleDocList"))
   names(x) = sapply(x, slot, "title")   

   if(as.data.frame)
      as(x, "data.frame")
   else
      x
}


setOldClass("GoogleDocList")
  # 
setAs("GoogleDocList", "data.frame",
          #  Make this adapt  to whether dealing with spreadsheets or documents.
          # Add the access settings.
      function(from) {

      ans =
        data.frame(
          title = sapply(from, slot, "title"),
          author = sapply(from, slot, "author"),
          updated = structure(sapply(from, slot, "updated"), class = c("POSIXt", "POSIXct")),
          id = sapply(from, slot, "id"), 
          type = factor(sapply(from, function(x) if("label" %in% names(x@category))
                                                    x@category["label"]
                                                 else
                                                    gsub(".*#", "", x@category["term"]))),
          row.names = shorten(sapply(from, slot, "id")),
          stringsAsFactors = FALSE
        )


       ans$edit = sapply(from, function(x) if("edit" %in% names(x)) x@edit["href"] else NA)

        #XXX Need to convert these via strptime?
       tmp = structure(sapply(from, function(x) if(length(x@published)) x@published else NA), class = c("POSIXt", "POSIXct"))
       ans$published = tmp
            
       ans

      })

setMethod("[", "GoogleDocList",
           function(x, i, j, ...) {
              structure(callNextMethod("["), class = class(x))
           })

`[.GoogleDocList` <-
function(x, i, j, ...) {
     structure(NextMethod("["), class = class(x))
   }
  
  


shorten =
function(x)
{
  tmp = basename(x)
  if(length(unique(tmp)) != length(tmp)) {
    #XXX
  }
  tmp
}



listFolder =
function(doc, con = doc@connection, as.data.frame = FALSE, asXML = FALSE)
{
  if(is.character(doc)) 
    doc = getDocs(con, folders = TRUE)[[doc]]
  
  url = doc@content['src']
  ans = getURL(url, curl = getCurlCon(con), followlocation = TRUE)
  xdoc = xmlParse(ans)

  if(asXML)
    return(xdoc)
  convertDocList(xdoc, con, as.data.frame)
}

setMethod("names", "GoogleFolder",
          function(x) {
            doc = listFolder(x, x@connection, asXML = TRUE)
            xpathSApply(doc, "//x:entry/x:title/text()", xmlValue,
                         namespaces = structure(nsDefines[1], names = "x"))
          })


setMethod("[[<-", "GoogleFolder",
           function(x, i, j, ..., value) {

             uploadDoc(value, x@connection, i, ...,  folder = x)
             
             x
           })
          


DocContentPrefix = "http://docs.google.com/feeds/download/documents/RawDocContents"


getDocContent = 
#
# This gets the contents of the document, i.e. the HTML, etc.
# and not just the text. The text is in there.
#
function(doc,  curl = getConnection(auth), auth, ...)
{
    # if doc gives the URI, go. Otherwise use getDocEntryByName
  if(is.character(doc) && !inherits(doc, "AsIs") &&
       substring(doc, 1, nchar(DocContentPrefix)) != DocContentPrefix) 
     doc = getDocEntryByName(doc, curl)


  if(length(doc) == 0)
    stop("no document")
  
  if(is(doc, "GoogleDocumentDescription")) 
      doc = doc@content["src"]


     #??? what about adding followlocation as an explicit argument for getDocContent
     #  just in case the user specifies it and we get a conflict/duplicated argument?
  getURLContent(doc, curl = curl, followlocation = TRUE, ...)
}

DocPrefix = "https://docs.google.com/feeds/documents/private/full/document%3A"

setGeneric("deleteDoc",
#
#  Need the link with the version information.
#  This comes from getDocs()$edit["href"], so easiest to pass in
#  the doc from
function(doc, con = getConnection(auth), auth = getGoogleAuth(), ...)
            standardGeneric("deleteDoc"))

setMethod("deleteDoc", "GoogleDocList",
          function(doc, con = getConnection(auth), auth = getGoogleAuth(), ...) {
             ans = lapply(doc, deleteDoc, con)
             structure(as.logical(unlist(ans) == 0), names = names(ans))
          })

setMethod("deleteDoc", "GoogleDocumentDescription",
          function(doc, con = getConnection(auth), auth = getGoogleAuth(), ...)
          {
             deleteDoc(doc@edit["href"], con, ...) 
           })

setMethod("deleteDoc", "character",
          function(doc, con = getConnection(auth), auth = getGoogleAuth(), ...) {

            if(!inherits(doc, "AsIs") && 
                  !grepl("http://docs.google.com", doc))  # substring(doc, 1, nchar(DocPrefix)) != DocPrefix) 
                 doc = getDocEntryByName(doc, con)
             

          if(is(doc, "GoogleDocumentDescription")) 
             doc = doc@edit["href"]

          ans = curlPerform(customrequest = "DELETE", url = doc, curl = con, writefunction = function(x) {})
             # Reset the curl customrequest just in case.
          curlSetOpt(customrequest = "POST", curl = con)
          ans
        })

getDocEntryByName = 
function(doc, curl, docs = getDocs(curl))
{
     i = pmatch(doc, names(docs))
     if(is.na(i))
        Stop(paste("Can't identify document ", doc), "NoSuchGoogleDocument")

     docs[[i]]
}



matchType = 
function(type)
{
  if(inherits(type, "AsIs"))
     return(type)
  
  i = pmatch(type, DocTypeExtensions)
  if(is.na(i))
    i = pmatch(tolower(type), tolower(names(DocTypeExtensions)))    

  if(is.na(i))
    stop("No match for type ", type)

  DocTypeExtensions[i]
}

DocTypeExtensions = 
  # The extension -> mime type mapping
  # From http://code.google.com/support/bin/answer.py?answer=76306&topic=10713
c(
   CSV = "text/csv",
   TSV = "text/tab-separated-values",
   TAB = "text/tab-separated-values",
   HTML = "text/html",
   HTM = "text/html",
   DOC = "application/msword",
   ODS = "application/x-vnd.oasis.opendocument.spreadsheet",
   ODT = "application/vnd.oasis.opendocument.text",
   RTF = "application/rtf",
   SXW = "application/vnd.sun.xml.writer",
   TXT = "text/plain",
   XLS = "application/vnd.ms-excel",
   PPT = "application/vnd.ms-powerpoint",
   PPS = "application/vnd.ms-powerpoint")



findType =
#
#  figure out the mime type for the document based on the extension.
#
function(name)
{
   ext = gsub(".*\\.([a-zA-Z]+)$", "\\1" , name)
   i = pmatch(ext, tolower(names(DocTypeExtensions)))
   if(is.na(i))
      "text/plain"
   else
       DocTypeExtensions[i]
}


#####################

getXMLDocEntryInfo =
 # This is a very simple form of this function for the moment
 # that does not try to be intelligent about the content by 
 # pretending we know about it. We use xmlToList() but then
 # add just a little contextual info.
 #
 #XXX Should return something based on the class of the document, e.g. spreadsheet or word processing document.
 #
function(node, con)
{
  ans = xmlToList(node)

   # turn these two fields into POSIXct values
  dateEls = c("published", "updated")
  ans[dateEls] = lapply(ans[dateEls],
                        function(el) as.POSIXct(strptime(el, "%Y-%m-%dT%H:%M:%S")))

    # change the generic link name to the value of the rel attribute.
  i = (names(ans) == "link")
  names(ans)[i] =  sapply(ans[i], `[`, "rel")

  ans$author = structure(ans$author[[1]], names = ans$author[[2]])

  if(is(con, "AbstractGoogleDocumentsConnection")) {   # "GoogleDocsConnection"
    klasses = c(presentation = "GooglePresentation", document = "GoogleDocument", spreadsheet = "GoogleSpreadsheet",
                 folder = "GoogleFolder")
    w = which(names(ans) == "category")
    k = ans[[  w[length(w)] ]]
    class = if(k["term"] == "http://schemas.google.com/spreadsheets/2006#spreadsheet")
                klasses["spreadsheet"]
            else if(k["label"] %in% names(klasses))
                klasses[k["label"] ]
            else
                "GoogleDocumentDescription" 
  } else
         #XXXX could be a folder!
     class = "GoogleSpreadsheet"

    obj = new( class)

    sapply(slotNames("GoogleDocumentDescription"),
          function(x) {
              if(!is.null(ans[[x]]))
                 slot(obj, x) <<- ans[[x]]
             })              

  if(is(obj, "GoogleSpreadsheet"))
      obj@worksheetsfeed = ans[["http://schemas.google.com/spreadsheets/2006#worksheetsfeed"]]

  if(is.null(ans$feedLink))
       ans$feedLink = as.character(NA)
  obj@access = ans$feedLink
  obj@connection = con

  obj
}


publicWorksheet =
function(key, curl = getCurlHandle(), obj = new("GoogleSpreadsheet"))
{
  feed = sprintf("http://spreadsheets.google.com/feeds/worksheets/%s/public/values", key)
  obj@worksheetsfeed = c(href = feed)
  obj@connection = new("UnauthenticatedGoogleDocumentsConnection", curl)
  obj
}

###################

parseAuth = 
#
#  parse the response from the Google login request to get the separate
#  elements in the form Auth = ..................\nSID=...........\nLSID=........\n
#  We need the auth.

function(ans)
{
  x = unlist(strsplit(ans, "\\\n"))
  tmp = strsplit(x, "=")
  structure(sapply(tmp, `[`, 2), names = sapply(tmp, `[`, 1)) 
}

#if(!exists("xmlToList", "package:XML"))
   # This comes from an unreleased version of the XML package.
xmlToList =
function(node, addAttributes = TRUE)
{
  if(is.character(node))
    node = xmlTreeParse(node)

  if(inherits(node, "XMLAbstractDocument"))
    node = xmlRoot(node)
  
  if(inherits(node, c("XMLTextNode", "XMLInternalTextNode")))
     xmlValue(node)
  else if(xmlSize(node) == 0)
     xmlAttrs(node)
  else {
     tmp = vals = xmlApply(node, xmlToList)
     tt = xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
     vals[tt] = lapply(vals[tt], function(x) x[[1]])
     if(any(tt) && length(vals) == 1)
       vals[[1]]
     else
       vals
  }
}


removeDoc =
  #
  # uploadDoc("1,2,3\n4,5,6\n", con, "Hazel", "CSV", asText = TRUE)
  # removeDoc("Hazel", con)
  #
  #
function(doc, con)
{
  if(is.character(doc))
    doc = getDocs(con)[[doc]]

  editURL = doc@edit["href"]

  con = dupCurlHandle(con)
  status = curlPerform(customrequest="DELETE", url = editURL, curl = con)
  status
}

