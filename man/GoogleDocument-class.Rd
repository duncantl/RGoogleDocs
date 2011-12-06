\name{GoogleDocument-class}
\docType{class}
\alias{GoogleDocumentDescription-class}
\alias{GooglePresentation-class}
\alias{GoogleSpreadsheet-class}
\alias{GoogleDocument-class}
\alias{GoogleFolder-class}

\alias{names,GoogleFolder-method}
\alias{$<-,GoogleDocumentDescription-method}
\alias{[<-,GoogleDocumentDescription,ANY,ANY-method}
\alias{rename}
\alias{rename,GoogleDocumentDescription-method}

\title{Details for a Google Document}
\description{
 This class is used to represent information about a Google Document.
It provides details about 
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
This is an S3 list with dynamic slots at present.
	 \describe{
\item{id}{URI: the unique URL for the document}
\item{published}{POSIXt, POSIXct: date the document was published}
\item{updated}{POSIXt, POSIXct:  the date when the document was last updated/modified}
\item{category}{character: the type of the document, e.g. spreadsheet, presentation, document}
\item{title}{character: the name of the document}
\item{content}{character: gives the type and the URL for the document}
\item{alternate}{character: gives the type and URL for the alternative location
      of this document.}
\item{self}{character vector: rel, type and href giving the 
  URL for the document}
\item{edit}{character vector: rel, type and href giving the 
  URL for editing the document}
\item{edit-media}{character vector: rel, type and href elements.}
\item{author}{named character vector: gives the author's name and email address as 
   the name}
\item{feedLink}{character vector: rel and href givng the general link for the document.}
\item{http://schemas.google.com/spreadsheets/2006\#worksheetsfeed}{URL: only for spreadsheets. This is the URL for the feed for the accessing the worksheets within the spreadsheet}
  }
}
\section{Extends}{
Class \code{"\linkS4class{oldClass}"}, directly.
}
\section{Methods}{
  \code{[<-}  and \code{$<-} allow one to set meta data such as the title of a document
  on the Google server (i.e. not locally)

  \code{rename} is a generic with methods for documents that allows  one to easily
  set the title of a document/folder.

  \code{names} on a GoogleFolder gives the titles of the documents contained in that 
  folder.
}
\references{} 
\author{Duncan Temple Lang}
\seealso{
 \code{\link{getDocs}}
 \code{\link{getWorksheets}}
 \code{\link{getExtent}}
 \code{\link{comments}}
 \code{\link{images}}
 \code{\link{comments}}
 \code{\link{footnotes}}
}
\examples{
if(exists("GoogleDocsPassword")) {
          # getGoogleDocsConnection("my login", "my password")

  con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword)
  getDocs(con)
}
}
\keyword{classes}
