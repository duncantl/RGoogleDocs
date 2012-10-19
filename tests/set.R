library(RGoogleDocs)

if(!is.null(getOption("GoogleDocsPassword"))) 
  assign("GoogleDocsPassword", getOption("GoogleDocsPassword"))

if(exists("GoogleDocsPassword") || Sys.getenv("GOOGLE_DOCS_PASSWORD") != "") {
  
con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword)
book = addSpreadsheet(con, c(21, 12), "dummy")

sheets.con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword, "wise")
docs = getDocs(sheets.con)

sh = addWorksheet(docs$dummy1, sheets.con, title = "testing")

sh[1, 3] = "testing"

sh[, 10, extent = 5] = letters[1:3]
sh[, 4, extent = c(2, 5)] = letters[1:3]
sh[, 8, extent = TRUE] = letters[1:3]
sh[, 9, extent = FALSE] = letters[1:3]


}
