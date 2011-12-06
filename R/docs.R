
comments =
function(doc, con, asNodes = FALSE)
{
  if(!inherits(doc, "XMLInternalDocument"))
     doc = readDoc(doc, con)    

  nodes = getNodeSet(doc, "//span[@class='writely-comment']")
  els = sapply(nodes, xmlValue)

  if(asNodes)
    return(els)

  txt = gsub("(.*) -.*$", "\\1", els)
  other = gsub(".* -(.*)$", "\\1", els)
  date = gsub(".* ([0-9]{1,2}/[0-9]{1,2}/[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2} [AP]M)", "\\1", other)
  who = gsub("(.*) [0-9]{1,2}/[0-9]{1,2}/[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2} [AP]M", "\\1", other)  

  data.frame(text = txt,
             date = as.POSIXct(strptime(date, "%m/%d/%y %H:%M %p")),
             who = who)
}

images =
  #
  # images("Many Parts", con)
  # images(doc)  # where doc is a result of htmlParse()
  #
function(doc, con, full.names = FALSE, asNodes = FALSE)
{
  if(!inherits(doc, "XMLInternalDocument"))
     doc = readDoc(doc, con)    

  nodes = getNodeSet(doc, "//img/@src")
  if(asNodes)
    nodes
  else
    unlist(nodes)
}

footnotes =
  #
  # footnotes("Many Parts", con)
  #
function(doc, con, asNodes = FALSE)
{
  if(!inherits(doc, "XMLInternalDocument"))
     doc = readDoc(doc, con)

  nodes = getNodeSet(doc, "//callout[@callouttype='footnote']")
  if(asNodes == TRUE)
    return(footnotes)
  
  txt = sapply(nodes, xmlValue)
  names(txt) = sapply(nodes, xmlGetAttr, "id", addNamespace = FALSE)

  txt
}


sections =
  #
  # Get the titles of each section and the level of the section, i.e. 1, 2, 3, ..., 6
  #
function(doc, con, asNodes = FALSE)
{
  if(!inherits(doc, "XMLInternalDocument"))
     doc = readDoc(doc, con)

   nodes = getNodeSet(doc, paste("//h", 1:6, sep = "", collapse = "|"))

   if(asNodes)
     return(nodes)
  
   structure(sapply(nodes, xmlValue),
              names = gsub("^h", "", sapply(nodes, xmlName)))
}


readDoc =
  #
  # Convert the name and connection into a parsed XML/HTML document.
  #
function(doc, con)
{
     txt = getDocContent(doc, con)
       # txt should have a class based on the type of document.
     if(is.raw(txt)) {
        txt
     } else if(length(grep("^(<html>|[[:space:]]*<!DOCTYPE HTML)", txt)))
        htmlParse(txt, asText = TRUE, error = function(...){})
     else
        xmlParse(txt, asText = TRUE)
}



# Could use the substitute(deparse(content)) as the default value for name 
setGeneric("uploadDoc",
           function(content, con, name, type = as.character(findType(content)),
                    binary = FALSE, asText = FALSE, folder = NULL, replace = TRUE, ...)
           standardGeneric("uploadDoc"))

setMethod("uploadDoc", 'data.frame',
          function(content, con, name, type = as.character(findType(content)),
                   binary = FALSE, asText = FALSE, folder = NULL, replace = TRUE, ...)
          {
            wcon = textConnection(NULL, "w", local = TRUE)
            on.exit(close(wcon))
            write.csv(content, file = wcon, row.names = FALSE)
            close(wcon); on.exit()
            uploadDoc(paste(textConnectionValue(wcon), collapse = "\n"), con, name, type = "csv", FALSE, TRUE, folder, replace = replace, ...)
          })


setMethod("uploadDoc", 'character',
#
# put a local document onto the Google Documents account.
#
function(content, con, name, type = as.character(findType(content)),
         binary = FALSE, asText = FALSE, folder = NULL, replace = TRUE, ...)
{
    # fix up the type to get the actual MIME type
    # The as.character(), as in the default value for type, discards the name
    # which ends up in the string, e.g. Content-Type.CSV:  text/csv
  if(!missing(type)) 
    type = as.character(matchType(type))
  else  
    type # force the value as the value of content may change!

    # make up the name from the value of content.
  if(missing(name))
     name = deparse(substitute(content))

    # If this is a file, read its contents. This assumes non-binary.
    # We check it is a character first.
  if(binary) 
   content = readBinary(content)
  else if(is.character(content) && !asText && file.exists(content) && !inherits(content, "AsIs")) 
    content = paste(readLines(content), collapse = "\n")

  curl = dupCon(con)
  
  if(replace) {
     curDocs = getDocs(con)
     replace = replace && name %in% names(curDocs)
     if(replace) {
        replaceCurl = dupCon(con)
      }
  }

    # Ensure we are doing a POST, just in case the curl handle was previously used for another
    # operation.
  curlSetOpt(customrequest = "POST", curl = curl)  


  if(!is.null(folder))  {
     if(is(folder, "character") && !inherits(folder, "AsIs"))
        folder = getDocs(con)[[folder]]

     if(is(folder, "GoogleFolder"))
       url = folder@content["src"]
     else if(inherits(folder, "AsIs"))
       url = folder
     else
       stop("Not certain how to deal with this folder")
  } else
     url = "https://docs.google.com/feeds/documents/private/full"  
  
  if(length(name) > 1) {
     # See  http://code.google.com/apis/documents/docs/2.0/developers <- guide <- protocol.html#UploadingToFolder
     url = paste("https://docs.google.com/feeds/folders/private/full", "/folder%3A",  name[1], sep = "")
     name = name[2]
  }


  h = c( 'Content-Type' = type,
         Slug = name)

        # we expect the curl object to already contain the authentication
        # but since we replace the httpheader, we have to set it again.
        # There is no way to get at the existing httpheader settings.
  if(is(con, "AuthenticatedGoogleDocumentsConnection"))    
     h['Authorization'] = paste("GoogleLogin auth=", con@auth, sep = "")  

  if(replace) {
      old = curDocs[[name]]
      tempName = makeTempName(names(curDocs))
      tempDoc = rename(old, tempName)
      on.exit(rename(tempDoc, name, con))
  }
  
    # Post the form with our authorization again.
    # postForm() uses the dynCurlReader() so returns binary or text as approriate.
  ans = postForm(url,
                  .opts = list(httpheader = h,
                               postfields = content,
                                ...),
                   curl = curl)

   if(replace) {
       deleteDoc(tempDoc, replaceCurl)
       on.exit()
   }

   if(!grepl("<", ans, fixed = TRUE)) {
      stop(ans)
   }
  
   doc = xmlParse(ans, asText = TRUE)
   getXMLDocEntryInfo(xmlRoot(doc), con)
})

dupCon =
function(con)
{
   ans = con
   ans@ref = dupCurlHandle(con)@ref
   ans
}

makeTempName =
function(current)
{
  while(TRUE) {
     candidate = basename(tempfile())
     if(!(candidate %in% current))
       return(candidate)
  }
}
  


readBinary = 
function(filename, len = NA)
{
  if(is.na(len)) 
     len = file.info(filename)[1, "size"]

  readBin(filename, "raw", len)
}


searchDocs =
function(text, con = getGoogleDocsConnection(), as.data.frame = FALSE,
          url = GoogleURLs["documents"])
{
  u = paste(url, gsub("[[:space:]]", "+", text), sep = "?q=")
  ans = getURLContent(u, curl = getCurlCon(con), followlocation = TRUE)
  txt = if(is(ans, "raw"))
          rawToChar(ans)
        else
          ans
  
  #XX Encoding from the Content-Type attribute
  doc = xmlParse(txt)
  convertDocList(doc, con, as.data.frame)
}
