\name{uploadDoc}
\alias{uploadDoc}
\alias{uploadDoc,data.frame-method}
\alias{uploadDoc,character-method}
\alias{[[<-,GoogleFolder-method}
\alias{deleteDoc}
\alias{deleteDoc,GoogleDocList-method}
\alias{deleteDoc,GoogleDocumentDescription-method}
\alias{deleteDoc,character-method}
\title{Add or remove a document}
\description{
 These functions allow the caller to manipulate the collection of
 documents in the Google Docs account.
 One can upload a document from the local machine.
 And one can remove or delete a document from the remote repository.
}
\usage{
uploadDoc(content, con, name, type = as.character(findType(content)), 
           binary = FALSE, asText = FALSE, folder = NULL, replace = TRUE, ...)
deleteDoc(doc, con = getConnection(auth), auth = getGoogleAuth(), ...)
}
\arguments{
  \item{content}{the name of a local file or the actual content of a document to be uploaded.}
  \item{doc}{the name of the Google document to be removed.}
  \item{con}{the CURL connection to the Google Docs API which has been 
        authenticated. See \code{\link{getGoogleDocsConnection}}.}
  \item{name}{the name of the new document to be created (or the document to be replaced).}
  \item{type}{the type of document.
      This can be the MIME type  (e.g. "text/csv") 
      or the short hand, e.g. "csv", "odt", ..., capitalized or uncapitalized.
      The type is matched in the vector DocTypeExtensions.
      The caller can pass the value as an "AsIs" object, i.e.
        \code{type = I("text/kml")} and the type will not be matched
	against DocTypeExtensions. The server wil still validate the
	type. So this is a way of using a new type recognized by Google
	Docs that is not reflected in the code in this package.}
  \item{binary}{a logical value indicating whether the content being uploaded is binary or not.}
  \item{asText}{a logical indicating whether the value of \code{content}
     is to be treated as the content to be uploaded (\code{TRUE}) or
      the name of a file (\code{FALSE}).}
  \item{\dots}{ additional arguments to be passed to the \code{curlPerform} call.}
  \item{auth}{the auth token returned from a call to \code{\link{getGoogleAuth}} and used to initialize a connection.}
  \item{folder}{the identifier for a Google Folder. This should be a
     \code{GoogleFolder} object, e.g., returned via
     \code{\link{getDocs}}.
     Alternatively, it can be the name of a folder and this function
     will call \code{\link{getDocs}} itself to obtain the folder
     information.
     Finally, it can be the URL for the folder's content, i.e. the
     value of the "src" element of the \code{content} slot
     in a \code{GoogleFolder} object. This is the /feeds/folders/...
     URL.  To specify the folder in this way, one must
     use the AsIs function \code{\link{I}} to identify the
     content as being the URL.
   }
   \item{replace}{a logical value. If this is \code{TRUE},  we check if
     there is an existing document named \code{name}. If there is,
     we rename  it to a temporary name and attempt the upload of the new
     document in \code{content}. If this succeeds, the earlier document
     is removed. If the upload fails, the earlier document is moved back
     to the original name and so nothing is lost.
     If \code{replace} is \code{FALSE}, none of this is done
     and Google Docs allows documents with the same name in the same folder.
   }
}

\value{
  \code{uploadDoc} returns an object of class \code{GoogleDocument}
   describing the newly uploaded document.

  \code{deleteDoc} returns 0 if the operation succeeded or a CURL error
  if it failed.
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

  x = "1, 2, 3\n4, 5, 6\n"
  uploadDoc(x, con, name = "direct csv", type = "csv")

  deleteDoc("Untitled Presentation", con)

  # uploading to a folder

     # Specify the folder object
  docs = getDocs(con)
  uploadDoc(x, con, name = "boo", type = "csv", folder = docs$MyFolder)

  f = docs$MyFolder
  f[["boo1", type = 'csv']] = x

     # We'll have to lookup the folder id in the call.
  uploadDoc(x, con, name = "boo2", type = "csv", folder = "MyFolder")

    # Give the explicit URL directly
  uploadDoc(x, con, name = "boo3", type = "csv", folder = I(docs$MyFolder@content["src"]))
}
}
\keyword{IO}
\concept{Web Services}
