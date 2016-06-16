
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion/Construction
###

setAs("ANY", "DocCollection", function(from) {
  as(from, "DocDataFrame")
})

setAs("list", "DocCollection", function(from) {
  as(from, "DocList")
})

setAs("data.frame", "DocCollection", function(from) {
  as(from, "DocDataFrame")
})

setAs("ANY", "DocList", function(from) {
  new("DocList", lapply(from, as, "list"))
})

setAs("data.frame", "DocList", function(from) {
          l <- lapply(split(from, rownames(from)), as, "list")
          new("DocList", lapply(l, setNames, colnames(from)))
      })

setAs("DocDataFrame", "list", function(from) {
          if (strict) {
              setNames(from@.Data, names(from))
          } else {
              from
          }
      })

setAs("ANY", "DocDataFrame", function(from) {
  new("DocDataFrame", as.data.frame(from))
})

## otherwise, methods package inserts its own method, because
## DocDataFrame ultimately contains 'list'.
setAs("list", "DocDataFrame", function(from) {
  new("DocDataFrame", as.data.frame(from))
})

setAs("DocDataFrame", "data.frame", function(from) {
          if (strict) {
              S3Part(from, TRUE)
          } else {
              from
          }
      })

setAs("DocList", "data.frame", function(from) {
          as(from, "DocDataFrame")
      })

setAs("DocList", "DocDataFrame", function(from) {
          as.data.frame(from, optional=TRUE)
      })

as.data.frame.DocCollection <-
    function (x, row.names = NULL, optional = FALSE, ...) {
        as.data.frame(x, row.names=row.names, optional=optional)
    }

setMethod("as.data.frame", "DocList",
          function (x, row.names = NULL, optional = FALSE) {
              if (!isTRUEorFALSE(optional)) {
                  stop("'optional' must be TRUE or FALSE")
              }
              ans <- as(restfulr:::raggedListToDF(x), "DocDataFrame")
              if (is.null(row.names)) {
                  row.names <- ids(x)
              }
              rownames(ans) <- row.names
              if (!optional) {
                  names(ans) <- make.names(names(ans))
              }
              ans
          })

## 'c' is a primitive, so we could define an S4 method without an S3 method,
## but the S4 generic for 'c' is problematic
c.DocCollection <- function(...) {
  as(NextMethod(), "DocCollection")
}



### -----------------------------------------------------------------------
### Bracket implementations
###

uncommonFields <- function(x, j) {
  fieldtab <- table(unlist(lapply(x, names), use.names=FALSE))
  common <- names(fieldtab)[fieldtab == length(x)]
  setdiff(j, common)
}

setMethod("[", "DocList", function(x, i, j, ..., drop = TRUE) {
  if (!isTRUEorFALSE(drop)) {
    stop("'drop' should be TRUE or FALSE")
  }
  if (!missing(i)) {
### FIXME: have to call S3Part() here due to bug in
### callNextMethod(). It should probably use the C-level
### callNextMethod, but when we specify arguments, it calls
### .nextMethod(), which does not work when .nextMethod is a
### primitive (infinite recursion).
    ans <- callNextMethod(S3Part(x, TRUE), i)
  } else {
    ans <- x
  }
  dropped <- FALSE
  if (!missing(j)) {
    if (!is.character(j)) {
      stop("'j' must be character")
    }
    if (drop && length(j) == 1L) {
      ans <- simplify2array2(lapply(unname(ans), `[[`, j))
      dropped <- TRUE
    } else {
      ans <- lapply(ans, function(d) {
                      d <- d[j]
                      d[!is.na(names(d))]
                    })
    }
  }
  if (dropped) {
    ans
  } else {
    initialize(x, ans)
  }
})

setMethod("[", "DocDataFrame", function(x, i, j, ..., drop = TRUE) {
              ans <- callNextMethod()
              if (is.data.frame(ans)) {
                  as(ans, "DocDataFrame")
              } else {
                  ans
              }
          })

setReplaceMethod("[", "DocList", function(x, i, j, ..., value) {
  if (missing(j)) {
      S3Part(x, TRUE)[i] <- value
      return(x)
  }
  if (missing(i)) {
    i <- seq_along(x)
  }
  if (is.null(value)) {
    value <- rep(list(NULL), length(x[i]))
  } else if (is.atomic(value) && !is.array(value)) {
    if (!is.vector(value)) {
      value <- lapply(value, list) # cannot rely on [<- to do this; drops attrs
    }
  } else {
    value <- as(value, "DocList")
  }
  x[i] <- mapply(function(xi, valuei) {
    xi[j] <- valuei
    xi
  }, x[i], value, SIMPLIFY=FALSE)
  x
})


## XXX In general (when not dropping) should this return a DocCollectionRef or
## a DocCollection??
setMethod("[", "DocCollectionRef", function(x, i, j, ..., drop = TRUE) {
    x <- docs(x)
    callGeneric()
    })

setReplaceMethod("[", "DocCollectionRef", function(x, i, j, ..., value) {
    full <- x
    x <- docs(x)
    res <-  callGeneric()
    full$docs = res
    full
    })
