\name{readDoc}
\alias{readDoc}
\alias{getDocContent}
\title{Get the contents of a Google Document}
\description{
  \code{getDocContent} fetches the contents of a Google Document.
  \code{readDoc} fetches and parses the document, assuming it is HTML or XML.
  For Presentation documents, i.e. slides/overheads, the result is the PDF content of
  the document returned as a raw vector with no intepretation. (See grImport if you really
  want to do something with it in R.)
}
\usage{
readDoc(doc, con)
}
\arguments{
  \item{doc}{the name of the document to be retrieved, or a GoogleDocument object obtained via a call to
    \code{\link{getDocs}}.}
  \item{con}{the authenticated connection to Google Docs.}
}
\value{
 \code{getDocContent} returns a character string (vector of length 1).
 \code{readDoc} returns the parsed HTML or XML document as an 
  \code{XMLInternalDocument}.
 If the document is a presentation, the result is a PDF.
}
\references{}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getDocs}}
 \code{\link{getGoogleDocsConnection}}
}
\examples{

if(exists("GoogleDocsPassword")) {
          # getGoogleDocsConnection("my login", "my password")

  con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword)
  # getDoc("Many Parts", con)
  getDocContent("Many Parts", con)

  o = readDoc("Introduction to RGoogleDocs", con)

  docs = getDocs(con)
  readDoc(docs[[1]], con)
}
}
\keyword{IO}
