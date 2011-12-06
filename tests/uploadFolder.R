library(RGoogleDocs)

if(!is.null(getOption("GoogleDocsPassword"))) 
  assign("GoogleDocsPassword", getOption("GoogleDocsPassword"))

# Need the GOOGLE_DOCS_PASSWORD to be set as an environment
# variable or as an option(GoogleDocsPassword = c(login = "password") .
if(exists("GoogleDocsPassword") || Sys.getenv("GOOGLE_DOCS_PASSWORD") != "") {

  con = getGoogleDocsConnection(getGoogleAuth())
  docs = getDocs(con)
  x = "1, 2, 3\n4, 5, 6\n"
  uploadDoc(x, con, name = "boo", type = "csv", folder = docs$MyFolder)

  uploadDoc(x, con, name = "boo2", type = "csv", folder = "MyFolder")

  uploadDoc(x, con, name = "boo3", type = "csv", folder = I(docs$MyFolder@content["src"]))
}
