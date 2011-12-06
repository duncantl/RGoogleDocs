library(RGoogleDocs)

if(!is.null(getOption("GoogleDocsPassword"))) 
  assign("GoogleDocsPassword", getOption("GoogleDocsPassword"))

# Need the GOOGLE_DOCS_PASSWORD to be set as an environment
# variable or as an option(GoogleDocsPassword = c(login = "password") .
if(exists("GoogleDocsPassword") || Sys.getenv("GOOGLE_DOCS_PASSWORD") != "") {
  con = getGoogleDocsConnection()
  docs = getDocs(con)
  listFolder(docs$MyDir, con)
}

