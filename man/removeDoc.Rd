\name{removeDoc}
\alias{removeDoc}
\title{Remove a document from a Google Docs account}
\description{
 This function removes a document within a Google 
 Docs repository/account.
}
\usage{
removeDoc(doc, con)
}
\arguments{
  \item{doc}{the name of the document to delete or  a 
    \code{\link{GoogleDocument-class}} returned by \code{\link{getDocs}}.}
  \item{con}{ an object of class \code{GoogleDocumentsConnection}
     or sub-class.}
}
\value{
  A named integer vector returned by \code{\link[RCurl]{curlPerform}}.
}
\references{}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getGoogleDocsConnection}}
}
\examples{
if(exists("GoogleDocsPassword")) {
          # getGoogleDocsConnection("my login", "my password")

  con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword)

    # Create a new document
  uploadDoc("1,2,3\n4,5,6\n", con, "Hazel", "CSV", asText = TRUE)
    # Then remove it.
  removeDoc("Hazel", con)
}
}
\keyword{IO}
