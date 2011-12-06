library(RGoogleDocs)
wb = publicWorksheet("phAwcNAVuyj2tPLxKvvnNPA")
ws = getWorksheets(wb, NULL)

 # This takes 7 seconds to download.
o = sheetAsMatrix(ws[[1]], con = NULL)

if(FALSE) {
 h = getCurlHandle()
 z = getURL(ws[[1]]@cellsfeed, curl = h, followlocation = TRUE)
 getCurlInfo(h)
}





