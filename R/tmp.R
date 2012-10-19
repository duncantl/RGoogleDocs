processCells2 =
function(doc, trim = TRUE, header = FALSE, as.data.frame = FALSE, stringsAsFactors = default.stringsAsFactors())
{  
  cl = getNodeSet(doc, "//a:entry/gs:cell", c(a = "http://www.w3.org/2005/Atom",
                                            gs="http://schemas.google.com/spreadsheets/2006"))

  rows = as.numeric(sapply(cl, xmlGetAttr, "row", addNamespace = FALSE))
  cols = as.numeric(sapply(cl, xmlGetAttr, "col", addNamespace = FALSE))
  vals = sapply(cl, function(x) { attrs = xmlAttrs(x)
                                  if("numericValue" %in% names(attrs))
                                     c(attrs["numericValue"], "numeric")
                                  else if("inputValue" %in% names(attrs))
                                     c(attrs["inputValue"], "character")
                                  else
                                     c(xmlValue(x), "character")
                                })
  
  dims = c(max(rows), max(cols))
  ans = types = rep(as.character(NA), prod(dims))

  ij = (cols-1)*dims[1] + rows
  ans[ij] = vals[1,]
  types[ij] = vals[2,]  
  attr(ans, "dim") = attr(types, "dim") = dims

  x = if(trim)
         trimRowCol(ans, types)
      else
         list(ans = ans, types = types)
  
  ans = setHeader(x$ans, x$types, header)
  fixColumnTypes(ans$ans, ans$types, as.data.frame, stringsAsFactors = stringsAsFactors)
}

trimRowCol =
function(ans, types = NULL, index = FALSE)
{
  i = !apply(ans, 1, function(x) all(is.na(x)))
  j = !apply(ans, 2, function(x) all(is.na(x)))
  
  if(index)
    return(list(row = i, col = j))

  if(is.null(types))
    return(ans[i,][,j])

  return(list(ans = ans[i,][,j],
              types = types[i,][,j]))
}


setHeader =
function(ans, types, header)
{  
  if(is.character(header))
    colnames(ans) = header
  else if(is.logical(header) && header) {
    colnames(ans) = ans[1,]
    types = types[-1,]
    ans = ans[-1,]
  } else
    colnames(ans) = paste("V", seq(length = ncol(ans)), sep = "")  

  list(ans = ans, types = types)
}

fixColumnTypes =
function(ans, types, as.data.frame = TRUE, stringsAsFactors = default.stringsAsFactors())
{
  if(as.data.frame) {
    structure(as.data.frame(
      lapply(seq(length = ncol(ans)),
              function(i) {
                tp = types[,i][!is.na(types[,i])]
                tp = unique(tp)
                if(length(tp) == 1)
                   as(ans[,i], tp)
                else
                   ans[,i]
              }), stringsAsFactors = stringsAsFactors), names = colnames(ans))
  } else {
    tp = unique(types[!is.na(types)])
    if(length(tp) == 1)
      matrix(as(ans, tp), nrow(ans), ncol(ans), dimnames = dimnames(ans))
    else
      ans
  }
}
