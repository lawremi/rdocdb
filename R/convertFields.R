### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Iteration
###

setGeneric("convertFields",
           function(x, types, FUN) standardGeneric("convertFields"),
           signature="x")

setMethod("convertFields", "DocDataFrame", function(x, types, FUN) {
  if (length(x) == 0L) {
    return(x)
  }
  initialize(x, as.data.frame(mapply(FUN, x, types, SIMPLIFY=FALSE),
                              optional=TRUE, stringsAsFactors=FALSE,
                              row.names=rownames(x)))
})

setMethod("convertFields", "DocList", function(x, types, FUN) {
  initialize(x, lapply(x, function(xi) {
    if (is.null(xi)) {
      NULL
    } else {
      mapply(FUN, xi, types[names(xi)], SIMPLIFY=FALSE)
    }
  }))
})

## XXX Should this change in place or create a new object? This matters
## here because we have reference mechanics where the above don't.
## for now we do the change in place. ~GB
setMethod("convertFields", "DocCollectionRef", function(x, types, FUN) {
    docs(x) <- convertFields(docs(x), types, FUN)
    x
    })
    
    
