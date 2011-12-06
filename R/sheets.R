
# http://code.google.com/apis/spreadsheets/docs/2.0/developers_guide_protocol.html
#
#  To add:
#     adding a worksheet
#     adding a row
#     editing a cell
#

setClass("URI", contains = "character")

setClass("GoogleWorksheetRef",
           representation(title = "character",
                          dims = "integer",
                          url = "URI",
                          listfeed = "URI",
                          cellsfeed = "URI",
                          edit = "URI",
                          connection = "GoogleDocumentsConnection"))

url =
function(x)
  new("URI", as.character(x))


if(FALSE) {
sheet.auth = getGoogleAuth("dtemplelang@gmail.com", "...", service = "wise")
sheets.con = getConnection(sheet.auth)

sheets = getDocs(sheets.con)
z = getURL("https://spreadsheets.google.com/feeds/worksheets/pZC1UNSGH5mvoFyTgmM_HSA/private/full", curl = sheets.con)
}

if(FALSE) {
  sheets.con = getConnection(getGoogleAuth("dtemplelang@gmail.com", "...", service = "wise"))
  a = getDocs(sheets.con)
  ts = getWorksheets(a$TwoSheets, sheets.con)
  names(ts)
}

getFeed =
  #
  # Find the data feed document for the spreadsheet. This is where the
  #
function(doc)
{
#  doc$`http://schemas.google.com/spreadsheets/2006#worksheetsfeed`["href"]
  doc@worksheetsfeed["href"]
}

getWorksheets =
  #
  # Given the basic description of the document, e.g. from getDocs(),
  # get the information about the individual worksheets.
  #
function(doc, con = getGoogleDocsConnection(service = "wise"))
{
  if(is(con, "GoogleDocsConnection")) {
     tmp.con = getGoogleDocsConnection(service = "wise", error = FALSE)
     if(is.null(tmp.con))
       stop("Need a GoogleSpreadsheetsConnection to access spreadsheets and their worksheets")

     con = tmp.con
     warning("established a new GoogleSpreadsheetsConnection")
  }
  
  if(is.character(doc))
    doc = getDocs(con, what = "spreadsheets")[[doc]]
  
  txt = getURL(getFeed(doc), curl = if(is.null(con)) getCurlHandle() else con ,
                followlocation = TRUE)

  processWorksheetFeed(txt, con)
}

processWorksheetFeed =
function(txt, con = NULL)
{
  doc = xmlParse(txt, asText = TRUE)  
  entries = getNodeSet(doc, "//x:entry", "x")
  ans = lapply(entries, readSheetRef, con)
  names(ans) = sapply(entries, function(x) xmlValue(x[["title"]]))

  ans
}

readSheetRef =
  #
  # Read information describing a worksheet and store the information
  # as a list() so that we can go access its contents later.
  #
function(node, con = NULL)
{
  dims = sapply(c("rowCount", "colCount"), function(x) as.integer(xmlValue(node[[x]])))
  url = getNodeSet(node, "./x:link[@rel='self']/@href", "x")[[1]]
  listfeed =  getNodeSet(node, "./x:link[@rel='http://schemas.google.com/spreadsheets/2006#listfeed']/@href", "x")[[1]]
  cellsfeed =  getNodeSet(node, "./x:link[@rel='http://schemas.google.com/spreadsheets/2006#cellsfeed']/@href", "x")[[1]]
  edit =  getNodeSet(node, "./x:link[@rel='edit']/@href", "x")[[1]]  

  new("GoogleWorksheetRef",
        title = xmlValue(node[["title"]]), dims = dims, url = url(url),
        listfeed = url(listfeed), cellsfeed = url(cellsfeed),
        edit = url(edit),
        connection = con)
}


getCells =
  #
  # Retrieve the cells document which gives information about the 
  # cells in the spreadsheet.
  #
function(sheet, includeEmpty = FALSE, range = NA, con = sheet@connection)
{
  if(!includeEmpty && is.na(range))
     getURL(sheet@cellsfeed, curl = getCurlCon(con), followlocation = TRUE)
  else {
    .params = list()
    if(includeEmpty)
        .params[["return%2Dempty"]] = "true"
    
    if(!is.na(range)) {
      .params[["range"]] = if(is.numeric(range))
                              paste(c("R", "C"), range, sep = "", collapse = "")
                           else
                              range
    }
    getForm(sheet@cellsfeed, .params = .params, curl = con,
                  .opts = list(httpheader = c(Authorization = paste("GoogleLogin auth=", con@auth, sep = ""))))
  }
}

sheetAsMatrix =
  #
  #  Add a data.frame target so that we compute the types of each column.
  #
  #
function(sheet, header = FALSE, as.data.frame = TRUE, trim = TRUE, con = sheet@connection, doc = xmlParse(getCells(sheet, con = con)))
{
#  processCells(doc, sheet@dims, header, as.data.frame, trim)
  processCells2(doc, trim, header, as.data.frame) 
}

setAs("GoogleWorksheetRef", "matrix", function(from) sheetAsMatrix(from, as.data.frame = FALSE))
setAs("GoogleWorksheetRef", "data.frame", function(from) sheetAsMatrix(from, as.data.frame = TRUE))


f =
# Seems to be off by 1.
function(x, i, j, ..., drop = TRUE) {

   if(any(i < 0)) {
     if(!all(i < 0))
        stop("can't mix positive and negative indices")
      i = seq_len(nrow(x))[i]
   }

   if(any(j < 0)) {
     if(!all(j < 0))
        stop("can't mix positive and negative indices")
      j = seq_len(nrow(x))[j]
   }   
  
    #XXX Have to do the escaping here. Is this because curlEscape() is applied to the entire
    # URL in getForm() and messes up the x@cellsfeed value.
  
             tt = c('min%2Drow' = min(i), 'max%2Drow' = max(i),
                     'min%2Dcol' = min(j), 'max%2Dcol' = max(j))

             u = paste(x@cellsfeed, "?", paste(names(tt), tt, sep = "=", collapse = "&"), sep = "")
             ans = getURL(u, curl = getCurlCon(x@connection), followlocation = TRUE)
 #            ans = getForm(x@cellsfeed,     curl = x@connection)

             dim = c(diff(range(i)) + 1L,
                     diff(range(j)) + 1L)

             processCells(xmlParse(ans), dim, header = FALSE, as.data.frame = TRUE,
                            offsets = c(min(i) - 1, min(j) - 1))
           }

setMethod("[", c("GoogleWorksheetRef", "numeric", "numeric"), f)

setMethod("[", c("GoogleWorksheetRef", "logical", "ANY"),
          function(x, i, j, ..., drop = TRUE) {
              x[seq_len(nrow(x))[i], j, ..., drop = drop]
          })

setMethod("[", c("GoogleWorksheetRef", "ANY", "logical"),
          function(x, i, j, ..., drop = TRUE) {
              x[i, seq_len(nrow(x))[j], ..., drop = drop]
          })


setMethod("[", c("GoogleWorksheetRef", "missing", "missing"), 
          function(x, i, j, ..., drop = TRUE) {
              x[seq_len(nrow(x)), seq_len(ncol(x)), ..., drop = drop]
          })

setMethod("[", c("GoogleWorksheetRef", "missing", "ANY"), 
          function(x, i, j, ..., drop = TRUE) {
              x[seq_len(nrow(x)), j, ..., drop = drop]
          })



setMethod("[", c("GoogleWorksheetRef", "ANY", "missing"),
          # Could use the listfeed here as we are only dealing with rows.
          function(x, i, j, ..., drop = TRUE) {
            x[i, seq(length = ncol(x)), drop = drop]
          })

setMethod("[", c("GoogleWorksheetRef", "missing", "ANY"),
          function(x, i, j, ..., drop = TRUE) {
             x[seq(length = nrow(x)), j, drop = drop]
          })


nsDefines =
  c("http://www.w3.org/2005/Atom",
    batch="http://schemas.google.com/gdata/batch",
    gs="http://schemas.google.com/spreadsheets/2006")

getExtent =
function(sheet, cellfeed = xmlParse(getCells(sheet)))
{
  cells = getNodeSet(cellfeed, "//gs:cell", nsDefines["gs"])
  rows = as.integer(sapply(cells, xmlGetAttr, "row", addNamespace = FALSE))
  cols = as.integer(sapply(cells, xmlGetAttr, "col", addNamespace = FALSE))
  matrix(c(range(rows), range(cols)), 2, 2,
            dimnames = list(c("min", "max"), c("row", "col")))
}  


setMethod("[<-", c("GoogleWorksheetRef", "numeric", "numeric"),
          function(x, i, j, ..., value) {
             x[cbind(rep(i, each = length(j)), j)] <- value
             x
          })

asExtent =
function(x, extent, cells, row = TRUE)
{
  ans = matrix(0, 2, 2)        
  if(is.logical(extent)) {
    if(extent)
      return(getExtent(x, cells))
    else {
      if(row)
        ans[,1] = c(1, x@dims[1])
      else
        ans[,2] = c(1, x@dims[2])
    }
  } else {
   ans[,if(row) 1 else 2] =
     if(length(extent) == 1)
       c(1, extent)
     else
       sort(extent)
  }
  ans
} 

setMethod("[<-", c("GoogleWorksheetRef", "numeric", "missing"),
          function(x, i, j, ..., extent = TRUE, value) {
             cells = xmlParse(getCells(x))
             ext = asExtent(x, extent, cells, TRUE)
             j = seq(from = ext[1,2], to = ext[2,2])
             value = rep(value, length = length(j))
             x[i, j] <- value
             x
          })

setMethod("[<-", c("GoogleWorksheetRef", "missing", "numeric"),
          function(x, i, j, ..., extent = TRUE, value) {
             cells = xmlParse(getCells(x))
             ext = asExtent(x, extent, cells, TRUE)             
             i = seq(from = ext[1, 1], to = ext[2, 1])
             value = rep(value, length = length(i))
             
             x[i, j] <- value
             x
          })


setMethod("[<-", c("GoogleWorksheetRef", "missing", "missing"),
          function(x, i, j, ..., extent = TRUE, value) {
             cells = xmlParse(getCells(x))
             ext = asExtent(x, extent, cells, TRUE)                          
             i = seq(from = ext[1, 1], to = ext[2, 1])
             j = seq(from = ext[1,2], to = ext[2,2])             
             value = rep(value, length = length(i)*length(j))
             x[i, j] <- value
             x
          })


setMethod("[<-", c("GoogleWorksheetRef", "matrix", "missing"),
          function(x, i, j, ..., cells = NULL, value) {

            #XXX deal with logicals, negative values (?)
            
               # Create the body of the batch request.
            feed = makeUpdateFeed(x, i, value, cells = cells)
            txt = saveXML(feed)
            txt = paste('<?xml version="1.0"?>', txt, sep = "\r\n")

              # 
            url = paste(x@cellsfeed, "batch", sep = "/")
            con = dupCurlHandle(x@connection)
            reset(con)
            r = basicTextGatherer()
            header = basicTextGatherer()
            dbg = debugGatherer()

            curlPerform(url = url,
                        # The documentation claim tht we should use PUT, but that doesn't work
                        # and consumed a lot of time determining that. Eventually, I used
                        # the python client library and sniffed the HTTP packages via tcpdump &
                        # wireshark
                        #  customrequest = "PUT",
                         postfields = txt,
                         debugfunction = dbg$update,
                         writefunction = r$update,
                         headerfunction = header$update,
                         httpheader = c(Authorization = paste("GoogleLogin auth=", x@connection@auth, sep = ""),
                                        'Content-Type' = "application/atom+xml",
                                        'User-Agent' = paste('RGoogleDocs', options()$HTTPUserAgent)),
                         curl = con, verbose = TRUE
                        )

            status = parseHTTPHeader(header$value())
            if(floor(as.numeric(status["status"])/100) != 2)
                #XXX throw an RCurl error.
               stop("error when sending batch request to update worksheet ", x@title, " in spreadsheet")
            
            x
          })


makeUpdateFeed =
  #
  # Create the feed for updating cells in batch.
  #
function(x, els,  values, cells = NULL)
{
  dimnames(els) = NULL
  value = rep(values, length = nrow(els))
  feed = newXMLNode("feed", namespaceDefinitions = nsDefines)
  newXMLNode("id", x@cellsfeed, # paste(x@cellsfeed, "batch", sep = "/"),
              parent = feed)

  ids = paste("A", seq(length = nrow(els)), sep = "")

  cellId = paste(x@cellsfeed,
                 apply(els, 1, function(x)
                                  paste(c("R", "C"), x, sep = "", collapse = "")),
                 sep = "/")

   # Get the range in the form RminCmin:RmaxCmax
  range = paste( apply(apply(els, 2, range), 1,
                       function(x)
                          paste(c("R", "C"), x, sep = "", collapse = "")),
                 collapse = ":")
  if(is.null(cells))
     cells = xmlParse(getCells(x, TRUE, range = range))

  nsDefs = structure(nsDefines, names = c("x", names(nsDefines)[-1]))
  sapply(seq(length = nrow(els)),
         function(i) {
            entry = newXMLNode("entry", parent = feed)
            newXMLNode("id", namespace = "batch", parent = entry, ids[i])

            cell = getNodeSet(cells, sprintf("//x:entry/gs:cell[@row = '%d' and @col='%d']", els[i,1], els[i,2]),
                                nsDefs)
            op = if(length(cell)) "update" else "insert"
            
            newXMLNode("operation", namespace = "batch", attrs = c(type = op), parent = entry)
            newXMLNode("title", attrs = c(type = "text"), parent = entry, ids[i])
            newXMLNode("id", cellId[i], parent = entry)
            newXMLNode("content", values[i], parent = entry)
            newXMLNode("category", parent = entry,
                        attrs = c(term="http://schemas.google.com/spreadsheets/2006#cell",
                                  scheme="http://schemas.google.com/spreadsheets/2006"))
            newXMLNode("category", parent = entry,
                       attrs = c(term="http://schemas.google.com/spreadsheets/2006#cells",
                                 scheme="http://schemas.google.com/g/2005#kind"))
            newXMLNode("link", parent = entry,
                        attrs = c(rel = "self", type = "application/atom+xml",
                                  href = cellId[i]))
            newXMLNode("link", parent = entry,
                        attrs = c(rel = "edit", type = "application/atom+xml",
                                  href = if(length(cell))
                                           xmlGetAttr(getSibling(cell[[1]], FALSE), "href", addNamespace = FALSE)
#                                             getNodeSet(cell[[1]], "./x:link[@rel='edit']/@href", nsDefs)[[1]]
                                         else
                                             cellId[i]))

            newXMLNode("cell", namespace = "gs", parent = entry,
                         attrs = c(row = els[i,1], col = els[i, 2], inputValue = values[i]))

        })

  feed
}



setCell =
  #
  # Set the value of an individual cell
  #  pos is a vector of length two giving the row and column.
  #
  #  If we use the wrong URL, we get a 400 Bad Request error.
  #
  #
function(sheet, pos, value, con = sheet@connection)
{
    # We need to find the edit URL for this cell, and only this cell so we need to query the cellsfeed
    # for this one cell. So we give the range in the query in R<i>C<j> format, and also get empty cell(s).
  cells = xmlParse(getCells(sheet, TRUE, range = paste(c("R", "C"), pos, sep="", collapse = ""), con = con))
  e = getNodeSet(cells, "//x:entry/x:link[@rel='edit']", c(x = nsDefines[1]))

     # This is the URL we PUT to.
  url = xmlGetAttr(e[[1]], "href", addNamespace = FALSE)

      # Create the body of the PUT.
  entry = newXMLNode("entry", namespaceDefinitions = nsDefines)
  # Don't need the id and the <link rel="edit".../>
     #  addChildren(entry, xmlParent(e[[1]])[["id"]])  
     #  addChildren(entry, e[[1]])
  newXMLNode("gs:cell", parent = entry, # as.character(value),
              attrs = c(row = pos[1], col = pos[2], inputValue = as.character(value)))
    
    txt = saveXML(entry)

  # Merge with the code above.
  
            ncon = dupCurlHandle(sheet@connection)
            reset(ncon)
            r = basicTextGatherer()
            header = basicTextGatherer()

            curlPerform(url = url,
                         customrequest = "PUT",
                         postfields = txt,
                         writefunction = r$update,
                         headerfunction = header$update,
                         httpheader = c(Authorization = paste("GoogleLogin auth=", con@auth, sep = ""),
                                        'Content-Type' = "application/atom+xml",
                                        'User-Agent' = paste('RGoogleDocs', options()$HTTPUserAgent)),
                         curl = ncon
                        )
            parseHTTPHeader(header$value())
}



setMethod("dim", "GoogleWorksheetRef", function(x) x@dims)

if(FALSE)
  setMethod("[", c("GoogleWorksheetRef", "numeric", "missing"),
           function(x, i, j, ..., drop = TRUE) {
              # if we just want rows, we can use the listfeed.
              # Note that the first row is considered the header!
             ans = getURL(sheet@listfeed, curl = getCurlCon(sheet@connection), followlocation = TRUE)

             processCells(xmlParse(ans), header = FALSE, as.data.frame = TRUE)
           })


##############################################################



processCells =
  #
  # See tmp.R::processCells2 for an alternative approach.
  #
function(doc, dim = integer(), header = FALSE, as.data.frame = TRUE, trim = TRUE, offsets = c(0, 0))
{
  if(length(dim) == 0) 
    dim = as.integer( sapply(c("rowCount", "colCount"), function(id) xmlValue(doc[[id]])))

  ans = matrix(as.character(NA), dim[1], dim[2])         
  types = ans

  entries = getNodeSet(doc, "//x:entry", "x")

  if(length(entries) == 0)
    return(if(as.data.frame) data.frame() else matrix(0, 0, 0))
  
  sapply(entries,
          function(e) {
             c = e[["cell"]]
             attrs = xmlAttrs(c)
             i = as.integer(attrs["row"]) - offsets[1]
             j = as.integer(attrs["col"]) - offsets[2]
             
             if("numericValue" %in% names(attrs)) {
               val = as.numeric(attrs["numericValue"])
               types[i, j] <<- "numeric"
             } else
               val = attrs["inputValue"]
             ans[i, j] <<- val
          })

  if(trim) {
      # find out which rows and which columns are mentioned.
      # But if the cells include the empty cells too, they will be mentioned.
#   idx = sapply(entries, function(x) xmlAttrs(x)[c("row", "col")])
#   i = unique(idx[1,])
#   j = unique(idx[2,])    
#   ans = ans[i,][,j]
#   types = types[i,][,j]
    
    i = apply(ans, 1, function(x) all(is.na(x)))
    if(any(i)) {
      ans = ans[!i, , drop = FALSE]
      types = types[!i, , drop = FALSE]
    }
    i = apply(ans, 2, function(x) all(is.na(x)))
    if(any(i)) {
      ans = ans[,!i, drop = FALSE]
      types = types[, !i, drop = FALSE]      
    }
  }

  if(is.character(header)) 
    colnames(ans) = header
  else if(is.logical(header) && header) {
    colnames(ans) = ans[1]
    types = types[-1, drop = FALSE]
    ans = ans[-1, drop = FALSE]
  } else
    colnames(ans) = paste("V", seq(length = ncol(ans)), sep = "")  

  if(as.data.frame) {
    structure(as.data.frame(
      lapply(seq(length = ncol(ans)),
              function(i) {
                tp = types[,i][!is.na(types[,i])]
                tp = unique(tp)
                if(length(tp) == 1)
                   as(ans[,i], tp)
                else
                   ans[,i]
              })), names = colnames(ans))
  } else {
    tp = unique(types[!is.na(types)])
    if(length(tp) == 1)
      matrix(as(ans, tp), nrow(ans), ncol(ans), dimnames = dimnames(ans))
    else
      ans
  }
}


addSpreadsheet =
function(con, dim = c(20, 10) , name = "Sheet")
{
  txt = paste(rep(paste(rep(",", dim[2] - 1 - 1), collapse = ""), dim[1] - 1), collapse = "\n")
  uploadDoc(txt, con, name, "CSV", asText = TRUE)
}

addWorksheet =
  #
  #  This doesn't 
  #
function(doc, con, dim = c(30, 20), title = "Sheet", asSheetRef = TRUE, ...)
{
  auth = con@auth

  if(is.character(doc)) 
    doc = getDocs(con)[[doc]]

      # if  doc is a GoogleDocument object, then we have the post URL information already.
      # So we don't need to call getFeed(doc), ...
  
  if(is(doc, "GoogleDocumentDescription")) 
    postURL = doc@worksheetsfeed["href"]
  else {
    tmp = xmlParse(getURL(getFeed(doc), curl = getCurlCon(con), followlocation = TRUE))
    postURL = as.character(getNodeSet(tmp, "//x:link[@rel='http://schemas.google.com/g/2005#post']/@href", "x")[[1]])
  }
  
  ns = c(gs = "http://schemas.google.com/spreadsheets/2006")
  node =
     newXMLNode("entry", namespaceDefinitions = c("http://www.w3.org/2005/Atom"),
                newXMLNode("title", title),
                newXMLNode("gs:rowCount", dim[1], namespaceDefinitions = ns),
                newXMLNode("gs:colCount", dim[2], namespaceDefinitions = ns))

  txt = saveXML(node)

  h = basicTextGatherer()

     # Using the current values in con causes the POST not to be effective (but not fail).
  orig.con = con
  con = dupCurlHandle(con)
  reset(con)
  ans = curlPerform(url = postURL,
                    writefunction = h$update,
                    headerfunction = h$update,
                    httpheader = c('Content-Type' = 'application/atom+xml',
                                   'User-Agent' = paste('RGoogleDocs', options()$HTTPUserAgent),
                                   Authorization = paste("GoogleLogin auth=", auth, sep = "")),
                    ...,
                    postfields = txt,
                    curl = con)

  if(asSheetRef) {
    sheets = getWorksheets(doc, orig.con)
    sheets[[length(sheets)]]
  } else
    ans
}


