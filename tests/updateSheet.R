if(!is.null(getOption("GoogleDocsPassword"))) 
  assign("GoogleDocsPassword", getOption("GoogleDocsPassword"))

if(exists("GoogleDocsPassword") || Sys.getenv("GOOGLE_DOCS_PASSWORD") != "") {
  library(RGoogleDocs)
  auth = getGoogleAuth(names(GoogleDocsPassword), GoogleDocsPassword, "wise")
  sheets.con = getGoogleDocsConnection(auth = auth)
  sheets = getWorksheets("Copy of mine3", sheets.con)

  sheets$Bob[ matrix(c(1,1, 2, 3, 4, 1), , 2, byrow = TRUE)] = c(10, 20, 30)
  sheets$Bob[ matrix(c(1,1), , 2, byrow = TRUE)] = 1000
  sheets$Bob[ 1, 2 ] = 145
  sheets$Bob[ 1,  ] = 190


 RGoogleDocs:::setCell(sheets$Bob, c(1, 1), "Duncan")
}
