\name{comments}
\alias{comments}
\alias{sections}
\alias{footnotes}
\alias{images}
\title{Query elements of word processing document}
\description{
 These functions allow the caller to retrieve the
 values of certain types of elements within a Google
 word processing document.
 These elements are the referenced images,  the names of the sections,
 the footnotes and the comments.  One can fetch the values
 of these elements or alternatively the HTML/XML nodes
 in which they are contained. This is useful if one wants to
 modify the contents of the document.
}
\usage{
comments(doc, con, asNodes = FALSE)
images(doc, con, full.names = FALSE, asNodes = FALSE)
footnotes(doc, con, asNodes = FALSE)
sections(doc, con, asNodes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{the document to be queried. This can be the name of the document,
   a \code{"GoogleDocument"} returned as an element from a call to \code{\link{getDocs}},
   or a parsed HTML document (using native/internal nodes).}
  \item{con}{the authenticated connection to the Google Docs API. See \code{\link{getGoogleDocsConnection}}.}
  \item{asNodes}{a logical value indicating whether to return the nodes corresponding to the 
    items rather than their contents.}
  \item{full.names}{a logical value indicating whether to give the full names or the 
    relative names of image files. If this is \code{TRUE}, the values are
    (or will be!) expanded relative to the name of the document, i.e.
    if the document is 
  }
}
\value{
 These functions return either a character vector or a data.frame, or if \code{asNodes} is \code{TRUE}
 then a list of the corresponding nodes.

 \code{comments} returns a data frame with a row for each comment
 and three columns: text, date and author's name. The date is of class POSIXct.
}
\references{}
\author{Duncan Temple Lang}

\seealso{
 \code{\link{readDoc}}
 \code{\link{getGoogleDocsConnection}}
}
\examples{

if((tmp <- Sys.getenv("GOOGLE_DOCS_PASSWORD")) != "") {
  tmp = strsplit(tmp, "=")[[1]]
  assign("GoogleDocsPassword", structure(tmp[2], names = tmp[1]), globalenv())
}

if(exists("GoogleDocsPassword")) {
      # getGoogleDocsConnection("my login", "my password")

   con = getGoogleDocsConnection(names(GoogleDocsPassword), GoogleDocsPassword)
   images("Many Parts", con)
   comments("Many Parts", con)

   docs = getDocs(con)
   sections(docs[["Many Parts"]], con)

     # If we want to parse the document just once and then 
     # get the elements without having to go back to Google
     # each time to download the document.
   hdoc = htmlParse(getDocContent("Many Parts", con), asText = TRUE, error = function(...){})
   comments(hdoc)
   images(hdoc)
   sections(hdoc)
   footnotes(hdoc)
}
}
\keyword{IO}

