 library(RGoogleDocs)

if(!is.null(getOption("GoogleDocsPassword"))) 
  assign("GoogleDocsPassword", getOption("GoogleDocsPassword"))

if(exists("GoogleDocsPassword")) { 
 auth = getGoogleAuth(names(GoogleDocsPassword), GoogleDocsPassword, "wise")
 sheets.con = getGoogleDocsConnection(auth = auth)

 addWorksheet("mine4", auth, c(20, 10), "florence")

 sh = getWorksheets("mine4", sheets.con)
 names(sh)
}
