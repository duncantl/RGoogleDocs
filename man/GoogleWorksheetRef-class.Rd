\name{GoogleWorksheetRef-class}
\docType{class}
\alias{GoogleWorksheetRef-class}
\alias{[,GoogleDocList,ANY,ANY-method} % doesn't belong here.
\alias{[,GoogleWorksheetRef,numeric,numeric-method}
\alias{[,GoogleWorksheetRef,numeric,missing-method}
\alias{[,GoogleWorksheetRef,missing,numeric-method}
\alias{[,GoogleWorksheetRef,ANY,logical-method}
\alias{[,GoogleWorksheetRef,ANY,missing-method}
\alias{[,GoogleWorksheetRef,logical,ANY-method}
\alias{[,GoogleWorksheetRef,missing,ANY-method}
\alias{[,GoogleWorksheetRef,missing,missing-method}
\alias{[<-,GoogleWorksheetRef,numeric,numeric-method}
\alias{[<-,GoogleWorksheetRef,numeric,missing-method}
\alias{[<-,GoogleWorksheetRef,missing,numeric-method}
\alias{[<-,GoogleWorksheetRef,missing,missing-method}
\alias{[<-,GoogleWorksheetRef,matrix,missing-method}
\alias{getExtent}

\alias{dim,GoogleWorksheetRef-method}

\title{Description of a remote Google Worksheet}
\description{
This class describes a remote Google worksheet object which is a
single sheet within a Google spreadsheet. 
The object has information about the location of the contents 
of the worksheet and also its dimensions at the time at which the
information was computed by R. (It is possible that the worksheet
has changed after this time.) We can use this object to get the contents
of the worksheet and its dimension, etc.

We can also modify the contents of a worksheet using 
the common subset assignments in R, e.g. 
\code{x[1, 1] <- 10}, \code{x[1:3, 4:9] <- 10}, \code{x[1:3,] <- 10},
\code{x[1:3,] <- 10}, and \code{x[,] <- 10}.

 The \code{dim} and \code{getExtent} functions can be applied to a worksheet.
\code{dim} returns the number of rows and columns for the entire  worksheet,
be they empty or not.
\code{getExtent} returns a 2 x 2 matrix with indicating the
 effective number of rows and columns.  The two columns
 give the minimum and maximum indices for the rows and columns
 respectively.
}
\section{Objects from the Class}{
 Objects are created via calls to 
 \code{\link{getWorksheets}} and \code{\link{addWorksheet}}
 and are very rarely 
}
\section{Slots}{
	 \describe{
 \item{title}{the name/title of the worksheet}
 \item{dims}{the number of rows and columns}
 \item{url}{the URL of the document}
 \item{listfeed}{content feed URL for working with rows.}
 \item{cellsfeed}{content feed URL for working with the individual cells}
 \item{connection}{the connection given as \code{con} in the call which is used to be able to return
      to the list/cells feed to get the contents without the need for a
      connection.}
 \item{edit}{the content feed URL for editing the document.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "GoogleWorksheetRef", i = "numeric", j = "numeric")}: 
         syntactic sugar for retrieving subrows and columns of this matrix}
    \item{dim}{\code{signature(x = "GoogleWorksheetRef")}: get the dimensions (number of rows and columns)
                 of this worksheet.}
	 }
}
\author{ Duncan Temple Lang}
\seealso{
  \code{\link{getWorksheets}}
  \code{\link{addWorksheet}}
}
\examples{
showClass("GoogleWorksheetRef")

\dontrun{
docs = getDocs(con)

book = addSpreadsheet(con, c(21, 12), "dummy")
sh = addWorksheet(docs$mine4, con, title = "testing")

sh[,10, extent = 5] = letters[1:3]
sh[,4, extent = c(2, 5)] = letters[1:3]
sh[,8, extent = TRUE] = letters[1:3]
sh[,9, extent = FALSE] = letters[1:3]
}
}
\keyword{classes}
