\name{searchDocs}
\alias{searchDocs}
\title{Search for text in Google Documents}
\description{
  This function allows one to searchidentify
  all, or some subsets, of the Google Documents
  in an account to look for a particular matching text.
  One specifies the text to be matched and a connection
  to the Google Docs account and then search is done on the server.
  The return value is either a list or a data frame
  describing the  matching documents.

  This is essentially a convenience function that 
  can be done with \code{\link{getDocs}}
}
\usage{
searchDocs(text, con = getGoogleDocsConnection(), as.data.frame = FALSE, url = GoogleURLs["documents"])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{the text to be searched for in the Google documents}
  \item{con}{the authenticated connection to the Google Docs account}
  \item{as.data.frame}{a logical value indicating whether to return the information about each document 
    as a row in a data frame or as individual \code{GoogleDocument} objects.}
  \item{url}{the type of documents to fetch, e.g. documents or spreadsheets.
      One can specify a value in GoogleURLs or use a name from this vector.}
}
\value{
 A list of GoogleDocumentDescription objects or a 
 data frame.
}
\references{\url{http://code.google.com/apis/documents/docs/2.0/developers\_guide\_protocol.html#Querying}}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getDocs}}
}
\examples{

\dontrun{
 searchDocs("a",con, url = "https://docs.google.com/feeds/documents/private/full/-/document")
   # Restrict the search to only word processing documents
 searchDocs("a",con, url = "https://docs.google.com/feeds/documents/private/full/-/document")
}

}
\keyword{IO}

