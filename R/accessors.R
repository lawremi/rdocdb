
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Metadata accessors
###

setGeneric("ids", function(x) standardGeneric("ids"))
setMethod("ids", "DocCollection", function(x) ROWNAMES(x))
setMethod("ids", "DocCollectionRef", function(x) ROWNAMES(docs(x)))


setGeneric("ids<-", function(x, value) standardGeneric("ids<-"))
setReplaceMethod("ids", "DocList", function(x, value) {
    names(x) <- value
    x
})
setReplaceMethod("ids", "DocDataFrame", function(x, value) {
  rownames(x) <- value
  x
})

setReplaceMethod("ids", "DocCollectionRef", function(x, value) {
  ids(docs(x)) <- value
  x
})


unid <- function(x) {
  ids(x) <- NULL
  x
}

setReplaceMethod("names", "DocList", function(x, value) {
                     names(S3Part(x, TRUE)) <- value
                     x
                 })
setReplaceMethod("names", "DocCollectionRef", function(x, value) {
                     names(docs(x)) <- value
                     x
                 })



setGeneric("fieldNames", function(x, ...) standardGeneric("fieldNames"))
setMethod("fieldNames", "DocList", function(x) {
  as.character(unique(unlist(lapply(x, names), use.names=FALSE)))
})
setMethod("fieldNames", "DocDataFrame", function(x) names(x))
setMethod("fieldNames", "DocCollectionRef", function(x) fieldNames(docs(x)))



setGeneric("fieldNames<-", function(x, value) standardGeneric("fieldNames<-"))
setReplaceMethod("fieldNames", "DocList", function(x, value) {
  initialize(x, lapply(x, setNames, value))
})
setReplaceMethod("fieldNames", "DocDataFrame", function(x, value) {
  colnames(x) <- value
  x
})
setReplaceMethod("fieldNames", "DocCollectionRef", function(x, value) {
  fieldNames(docs(x)) <- value
  x
})


setGeneric("ndoc", function(x, ...) standardGeneric("ndoc"))
setMethod("ndoc", "DocCollection", function(x) NROW(x))
setMethod("ndoc", "DocCollectionRef", function(x) ndoc(docs(x)))

setGeneric("nfield", function(x, ...) standardGeneric("nfield"))
setMethod("nfield", "ANY", function(x) length(fieldNames(x)))
setMethod("nfield", "DocList", function(x) length(unique(fieldNames(x))))


setGeneric("meta", function(x) standardGeneric("meta"))

setMethod("meta", "ANY", function(x) {
  attr(x, "meta")
})

setMethod("meta", "DocList", function(x) {
  lapply(x, attr, "meta")
})

setMethod("meta", "DocCollectionRef", function(x) meta(docs(x)))


setGeneric("meta<-", function(x, value) standardGeneric("meta<-"))

setReplaceMethod("meta", c("DocList", "NULL"), function(x, value) {
  value <- rep(list(NULL), length(x))
  callGeneric()
})

setReplaceMethod("meta", c("DocList", "list"), function(x, value) {
  initialize(x, mapply(function(xi, valuei) {
    attr(xi, "meta") <- valuei
    xi
  }, x, value, SIMPLIFY=FALSE))
})

setReplaceMethod("meta", c("DocDataFrame", "data.frame"), function(x, value) {
  stopifnot(nrow(value) == nrow(x))
  attr(x, "meta") <- value
  x
})

setReplaceMethod("meta", c("ANY", "NULL"), function(x, value) {
  attr(x, "meta") <- NULL
  x
})

## XXX Is this ambiguous with the above?
setReplaceMethod("meta", c("DocCollectionRef", "ANY"),
                 function(x, value) {
    meta(docs(x)) <- value
    x
    })

unmeta <- function(x) {
  meta(x) <- NULL
  x
}

setGeneric("docs", function(x, ...) standardGeneric("docs"))

setMethod("docs", "DocCollection", function(x) x)
setMethod("docs", "DocCollectionRef", function(x) x$docs)

setGeneric("docs<-", function(x, ..., value) standardGeneric("docs<-"))

setReplaceMethod("docs", "DocCollectionRef", function(x, ..., value) {
    x$docs = value
    x})
