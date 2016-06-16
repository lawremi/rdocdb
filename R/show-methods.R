
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

setMethod("show", "DocCollection", function(object) {
  cat(class(object), " (", ndoc(object), "x", nfield(object), ")\n", sep="")
})


showDoc <- function(x, title) {
    cat(title, "\n")
    lines <- S4Vectors:::labeledLine(names(x), unname(x),
                                     count = lengths(x) > 1L,
                                     vectorized=TRUE)
    cat(lines, "\n", sep="")
}

setMethod("show", "DocList", function(object) {
  callNextMethod()
  if (length(object) > 0L) {
    showDoc(object[[1L]], "First document")
  }
})

setMethod("show", "DocDataFrame", function(object) {
  callNextMethod()
  out <- S4Vectors:::makePrettyMatrixForCompactPrinting(object)
  print(out, quote=FALSE, right=TRUE, max=length(out))
})

setMethod("show", "DocCollectionRef",
          function(object) {
    cat("A reference to a DocCollection\n")
    show(docs(object))
    })
