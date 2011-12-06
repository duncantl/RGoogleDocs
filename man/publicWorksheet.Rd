\name{publicWorksheet}
\Rdversion{1.1}
\alias{publicWorksheet}
\title{Create reference to a public Google document}  
\description{
  This function gives us an object  which references a public
  Google  document (worksheet). 
}
\usage{
publicWorksheet(key, curl = getCurlHandle(), obj = new("GoogleSpreadsheet"))
}
\arguments{
  \item{key}{the key identifying the public Google document}
  \item{curl}{the curl handle to use when querying this document (in the
  future). This can be \code{NULL}.}
  \item{obj}{an instance of the desired R type. This is filled in and
    then returned. This allows the caller to pass an instance of a
    derived class.
   }
}
\value{
 The updated object \code{obj}.
}
%\references{}
\author{
Duncan Temple Lang
}

\seealso{
 \code{\link{sheetAsMatrix}}
}
\examples{

  wb = publicWorksheet("phAwcNAVuyj2tPLxKvvnNPA")
  ws = getWorksheets(wb, NULL)
  o = sheetAsMatrix(ws[[1]], con = NULL)
}
\keyword{IO}

