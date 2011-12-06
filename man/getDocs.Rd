\name{getDocs}
\alias{getDocs}
%\alias{getDocContent}
\title{Get a list of available documents} 
\description{
  This function retrieves the list of available documents
  from the Google Docs account.
  Depending on the service specified when creating
  the connection and authenticating the login, the documents
  may be all document types or just the spreadsheets.

  One can specify \url{http://code.google.com/apis/gdata/docs/2.0/reference.html#Queries}
  in the search  via the \code{what} parameter.
  At present, you have to specify the full URL yourself rather than using 
  more convenient R functionality and syntax.
}
\usage{
getDocs(curl = getConnection(auth), folders = TRUE,
         as.data.frame = FALSE, auth = getGoogleAuth(service = service), 
          what = GoogleURLs[service],
           ..., service = "writely")
}
\arguments{
  \item{curl}{the authenticated connection to the Google Docs account}
  \item{folders}{logical value indicating whether to include folders in the 
     list of documents.}
  \item{as.data.frame}{a logical value indicating whether to return the information about each document 
    as a row in a data frame or as individual \code{GoogleDocument} objects.}
  \item{auth}{authentication token obtained from calling \code{\link{getGoogleAuth}}
    which can be used to initialize a Curl handle to use as the connection.}
  \item{what}{the type of documents to fetch, e.g. documents or spreadsheets.
      One can specify a value in GoogleURLs or use a name from this vector.
    }
  \item{\dots}{additional arguments passed to
    \code{\link[RCurl]{getURL}}.}
  \item{service}{the name of the Google Docs service, e.g. "wise" or "writely"}
}
%\details{}
\value{
  A list of \code{GoogleDocument} objects or a data frame.
}
\references{}
\author{Duncan Temple Lang}
\seealso{
\code{\link{getGoogleDocsConnection}}
\code{\link{searchDocs}}
}
\examples{
if(exists("GoogleDocsPassword")) {
         # getGoogleDocsConnection("my login", "my password")
cat("Getting documents\n")
  con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword)

  getDocs(con)  # list of GoogleDocuments
  getDocs(con, TRUE) # a data frame

\dontrun{
  con = getGoogleDocsConnection("me", "password", service = "wise")
  getDocs(con)  # just the spreadsheets
}
    # Get just the word processing documents, i.e. not the spreadsheets, 
  d = getDocs(con, what = "https://docs.google.com/feeds/documents/private/full/-/document")
    # The above should be slightly faster than doing the subsetting in R.
  d = getDocs(con)
  d[sapply(d, is, "GoogleDocument")]

  spreadsheetsDocs = getDocs(con, what = "https://docs.google.com/feeds/documents/private/full/-/spreadsheet")

     # Get the starred spreadsheets
  starredSheets = getDocs(con, what = "https://docs.google.com/feeds/documents/private/full/-/spreadsheet/starred")


      # documents within the directory named MyDir
   o = getDocs(con, what = "https://docs.google.com/feeds/documents/private/full/-/MyDir")

}
}
\keyword{IO}

