\name{getWorksheets}
\alias{getWorksheets}
\alias{sheetAsMatrix}
%\alias{dim,GoogleWorksheetRef-method}
\title{Access the contents of a spreadsheet}
\description{
 These functions provide access to the contents of a spreadsheet.
 They allow one to  access the different worksheets within a spreadsheet
 and then access the values of cells within those worksheets.
 One can convert a worksheet, or a subset of it, into a data frame or matrix in R.

 Having obtained a GoogleWorsheetRef, one can query its  number of rows and columns
 directly (i.e. without an additional query to the Google server) 
 and one can coerce the contents to  data frame or matrix  with
  \code{as(sheetRef, "data.frame")}.
}
\usage{
getWorksheets(doc, con)
sheetAsMatrix(sheet, header = FALSE, as.data.frame = TRUE, 
               trim = TRUE, con = sheet@connection,
                doc = xmlParse(getCells(sheet, con = con))) 
}
\arguments{
  \item{doc}{the name of the spreadsheet document or a  \code{GoogleDocument} object 
    obtained via a call to \code{\link{getDocs}}.
    In \code{sheetAsMatrix} this is the parsed XML document containing
    the cell values. This is typically computed via the
    default value of the parameter.  But one can call \code{getCells}
    separately with non-default values for the parameters.
  }
  \item{con}{the authenticated connection to the Google Spreadsheets API.
    This must be for the "wise" service rather than the general "writely" service.
    So this should be an object of class \code{"GoogleSpreadsheetsConnection"}
    created with a call of the form \code{getGoogleDocsConnection(login, password, "wise")}.}
  \item{sheet}{the sheet object of class \code{GoogleWorksheetRef}.}
  \item{header}{information about the column headers for the worksheet.
      This is a) a character vector giving the names of the resulting columns (after the empty columns have been
      discarded), b) a logical value of \code{TRUE} indicating whether the first row of the worksheet is to be treated
      as the header, and c) a logical value of \code{FALSE} indicating that there are no column headers available
      and so use V1, V2, ...
   }
 \item{as.data.frame}{a logical value indicating whether a matrix (\code{FALSE}) or a data frame (\code{TRUE}) 
         is to be created}
 \item{trim}{a logical value indicating whether to drop the rows and columns of the worksheet that have only
       missing values, i.e. those that may be used for pseudo-formatting/layout.
      This is optional as there may be rows or columns that contain legitmate missing values.
       The algorithm for converting the cells to a table could be more intelligent about this 
       and explicitly determine the rows and columns which have no
       values, not which contain \code{NA}s.}
}
\value{
 A list of the \code{GoogleWorksheetRef} objects.
 Each of these is an object of class \code{\link{GoogleWorksheetRef-class}}.

 The names of the sheets are used as the names of the returned R list object.
}
\references{\url{http://code.google.com/apis/spreadsheets/docs/2.0/developers\_guide\_protocol.html}}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getGoogleDocsConnection}}
 \code{\link{getDocs}}
}
\examples{
if(exists("GoogleDocsPassword")) {
      # getGoogleDocsConnection("my login", "my password", "wise")
  con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword, "wise")

  sheets = getWorksheets("TwoSheets", sheets.con)

  docs = getDocs(sheets.con)
  sheets = getWorksheets(docs[["TwoSheets"]], sheets.con)

  names(sheets)
}
}
\keyword{IO}
