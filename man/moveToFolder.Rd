\name{moveToFolder}
\alias{moveToFolder}
\title{Move a document to a folder}
\description{
  This function allows us to move 
  a document, be it a spreadsheet, word processing document, presentation or
  folder, into a Google Docs folder on the repository.
}
\usage{
moveToFolder(doc, to, con = if (is(doc,"GoogleDocumentDescription")) 
                              doc@connection 
                            else
                              getGoogleDocsConnection())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the document (spreadsheet, word processing document, folder, presentation) to be moved.
    This should be a GoogleDocumentDescription, but can be a name.}
  \item{to}{the folder object of class GoogleFolder, or a name.}
  \item{con}{an authenticated connection to the Google API. See \code{\link{getGoogleDocsConnection}}.}
}
\value{
  A \code{GoogleFolder}
}
\references{\url{http://code.google.com/apis/documents/docs/2.0/developers\_guide\_protocol.html}}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{addFolder}}
}
\examples{
if(exists("GoogleDocsPassword")) {
  con = getGoogleDocsConnection()
  a = addFolder("A", con)
  b = addFolder("B", con)
  c = addFolder("C", con)

  moveToFolder(b, a, con)
  moveToFolder(c, a, con)

  f = addFolder(c("One", "Two", "Three"), con)
}
}
\keyword{IO}

