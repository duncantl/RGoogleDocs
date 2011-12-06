\name{setAccess}
\alias{setAccess}
\alias{getAccess}
\alias{removeAccess}

\title{Query and modify the Access Control List}
\description{
  These functions allow one to programmatically 
  query and modify the access control list for a
  a Google document.
  These allows us to find out or specify who owns, can write,
  or read the document.
  We can also revoke permission.
}
\usage{
setAccess(doc, ..., .perms = list(...), con = doc@connection)
getAccess(doc, con = doc@connection)
removeAccess(doc, who, con = doc@connection)
}
\arguments{
  \item{doc}{the GoogleDocumentDescription object or the name of a document.
     If this is just the name, \code{con} must be specified.}
  \item{\dots}{name = value pairs where name gives the type of access being 
    granted and value is the identity of the person to whom it is being granted.
    value is typically an email address or \code{'everyone'}.}
  \item{.perms}{this provides an alternative way to specify the type-who pairs
      rather than \dots whch is more convenient for calling this function programmatically rather than
      interactively.}
  \item{con}{the authenticated Google connection. See \code{\link{getGoogleDocsConnection}}.}
  \item{who}{the identity of the user for who the permission is to be removed.
     This is an email address or \code{'everyone'}.}
}
\value{
  \code{getAccess} returns a character vector giving the
  type-who pairs.

  \code{setAccess} returns the updated access list.

  \code{removeAccess} returns the result of the HTTP request to remove the permissions.
  See \code{\link[RCurl]{parseHTTPHeader}}.
}
\references{\url{http://code.google.com/apis/documents/docs/2.0/developers\_guide\_protocol.html#AccessControlLists}}
\author{Duncan Temple Lang}
\seealso{ 
 \code{\link{getDocs}}
 \code{\link{getGoogleDocsConnection}}
}
\note{
For reasons I haven't got to the bottom of yet, 
setAccess() can return a value that is different
from a subsequent call to getAccess(),  yet
that is precisely the call at the end of setAccess() with 
the same connection object!
\code{
con = getGoogleDocsConnection()
docs = getDocs(con)
setAccess(docs$foo, writer = 'duncan@wald.ucdavis.edu', reader = 'everyone')
                    owner                    writer                    reader 
  "dtemplelang@gmail.com" "duncan@wald.ucdavis.edu"                "everyone" 
getAccess(docs$TwoSheets, getGoogleDocsConnection())
                  owner                  reader 
"dtemplelang@gmail.com"              "everyone" 
}
}
\examples{
}
\keyword{IO}

