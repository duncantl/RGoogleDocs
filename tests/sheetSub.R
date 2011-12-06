 library(RGoogleDocs)

if(!is.null(getOption("GoogleDocsPassword"))) 
  assign("GoogleDocsPassword", getOption("GoogleDocsPassword"))

if(exists("GoogleDocsPassword") || Sys.getenv("GOOGLE_DOCS_PASSWORD") != "" || !is.null(getOption("GoogleDocsPassword"))) {
 sheets.con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword, "wise")
 sheets = getWorksheets("TwoSheets", sheets.con)
 sheets[[1]][3:4,1]
}
