\name{addWorksheet}
\alias{addWorksheet}
\alias{addSpreadsheet}
\title{Add an empty worksheet to a spreadsheet or a new spreadsheet}
\description{
  The \code{addWorksheet} function can be used to add one, empty worksheet to 
  an existing spreadsheet. The new worksheet can be 
  given dimensions and a title.

  \code{addSpreadsheet} adds a new spreadsheet to the list of 
  Google documents by uploading an empty spreadsheet with the specified 
  dimensions. 
}
\usage{
addWorksheet(doc, con, dim, title = "", asSheetRef = TRUE, ...)
addSpreadsheet(con, dim = c(20, 10) , name = "Sheet")
}
\arguments{
  \item{doc}{either the name of or a \dQuote{reference} }
  \item{con}{
the authenticated connection to the Google Spreadsheets API.
    This must be for the "wise" service rather than the general "writely" service.
    So this should be an object of class \code{"GoogleSpreadsheetsConnection"}
    created with a call of the form \code{getGoogleDocsConnection(login, password, "wise")}.}
  \item{dim}{the desired dimensions (rows and columns) of the new worksheet.}
  \item{title}{a character string giving the title for the new worksheet}
  \item{asSheetRef}{a logical value indicating whether the result should be a \code{\link{GoogleWorksheetRef-class}}
      object or the status of the HTTP request to add the worksheet.}
  \item{\dots}{additional arguments passed on to \code{\link[RCurl]{curlPerform}} 
        which submits the request.}
  \item{name}{a character string giving the name of the spreadsheet document}
}

\value{
  If \code{asSheetRef} is \code{TRUE}, an object of 
   \code{\link{GoogleWorksheetRef-class}}.

  If \code{asSheetRef} is \code{FALSE}, the status of the HTTP request
  from \code{\link[RCurl]{curlPerform}}.
}
\references{}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getDocs}}
 \code{\link{getGoogleDocsConnection}}
 \code{\link{getGoogleAuth}}
}
\examples{

}
\keyword{IO}
