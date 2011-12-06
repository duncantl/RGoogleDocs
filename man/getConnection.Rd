\name{getGoogleDocsConnection}
\alias{getGoogleDocsConnection}
\alias{getConnection}
\alias{getGoogleAuth}
\title{Connect to Google Documents server}
\description{
  These functions provide the mechanism with which to 
  login to Google Docs and create a connection which
  caches the authentication so that it can be used in
  subsequent interactions with the Google Docs API
  within this R session.

  If the login and password are not specified in a call to either
  function, we look for it in the option named
  \code{GoogleDocsPassword}. This should be a vector of length 2
  giving the login and password, or alternatively a 
  named character vector of length 1 where the value is the password
  and the  name is the login.
  If the option is not set, we look in 
  the environment variable 
  \code{GOOGLE_DOCS_PASSWORD}.  If this is set, 
   its value is used and should be in the form 
  \code{login=value}.
}
\usage{
getGoogleDocsConnection(..., auth, error = TRUE)
getGoogleAuth(login = getOption("GoogleDocsPassword"), password,
                 service = "writely", appID = "R-GoogleDocs-0.1", error = TRUE)
}
\arguments{
  \item{...}{arguments that are passed on to \code{\link{getGoogleAuth}}
     For "convenience", one can also pass the authentication token
     returned from explicitly calling \code{\link{getGoogleAuth}
   }.
 }
  \item{auth}{the value returned from a call to \code{\link{getGoogleAuth}}.}
  \item{login}{the user name/login for the Google account}
  \item{password}{the password for the account corresponding to \code{login}}
  \item{service}{the name of the Google service, i.e. writely, wise}
  \item{appID}{unused}
  \item{error}{a logical value indicating whether, if \code{TRUE}, to raise an error (i.e. call to 
   \code{\link[base]{stop}} or just return \code{NULL} if \code{error} is \code{FALSE}.)
  }
}
\details{

}
\value{
 An object of class either \code{"GoogleSpreadsheetsConnection"}
 or \code{"GoogleDocsConnection"} depending on which service was 
 requested - wise or writely, respectively.
 This object is actually a Curl handle and can be used in subsequent HTTP
 requests.
}
\references{\url{http://code.google.com/apis/accounts/docs/AuthForInstalledApps.html}}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getGoogleAuth}}
}
\examples{
if(exists("GoogleDocsPassword")) {
   con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword)
   # for all documents
  con = getGoogleDocsConnection("dtemplelang@gmail.com", "my password")
   # for spreadsheets
  con = getGoogleDocsConnection("dtemplelang@gmail.com", "my password", "wise")

   # Getting auth separately.
  auth = getGoogleAuth("dtemplelang@gmail.com", "my password", "wise")
  con = getGoogleDocsConnection(auth)
  con = getGoogleDocsConnection(auth = auth)
}
}
\keyword{IO}

