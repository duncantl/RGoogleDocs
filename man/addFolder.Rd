\name{addFolder}
\alias{addFolder}
\alias{listFolder}
\title{Create and list the contents of a folder}
\description{
  These functions allow us to create a new folder in
  the Google Docs repository and also to 
  list the documents within a folder.
}
\usage{
addFolder(name, con, url = GoogleURLs["documents"])
listFolder(doc, con = doc@connection, as.data.frame = FALSE, asXML = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{a character string giving the name of the folder to be created.}
  \item{con}{an authenticated connection to the Google Docs repository, for the writely service,
        not the wise service for spreadsheets.}
  \item{url}{the URL where the request is to be posted. This is the default
     top-level of the repository.}
  \item{doc}{the folder whose contents are to be listed. This can be a \code{GoogleDocumentDescription}
    returned by \code{addFolder} or \code{\link{getDocs}} or can be a simple character string
    identifying the folder by name.}
  \item{as.data.frame}{a logical value indicating whether the information about the collection of
    documents should be  returned as a data frame or left as a list of \code{GoogleDocumentDescription} objects.}
  \item{asXML}{a logical value indicating whether the XML returned from the request
      to list the contents of the folder     should be  returned directly as a parsed XML document.}
}
\value{
  \code{addFolder} returns an object of class \code{\link{GoogleFolder-class}}.

  \code{listFolder} returns a list of \code{\link{GoogleDocumentDescription-class}}
  objects or a data frame. See \code{\link{getDocs}}.
}
\references{}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getGoogleDocsConnection}}
 \code{\link{getDocs}}
 \code{\link{moveToFolder}}
}
\examples{
}
\keyword{IO}
