### =========================================================================
### DocCollection objects
### -------------------------------------------------------------------------

### The virtual DocCollection class, which contains one or more
### documents, via two initial implementations: list (of lists) and
### data.frame. These differ from ordinary lists and data.frames by
### the relationship between the list and data.frame representation. A
### DocList has elements corresponding to the *rows* of a data.frame,
### not the columns.
###
### Thus, we need a separate abstraction, and any
### DocCollection object is contracted to implement this API:
### - Two dimensional [,] extraction
### - ids() returns the document IDs
### - fieldNames() returns the names of the unique document fields
###
### The data.frame object already supports the first, and it would be
### easy to add data.frame methods for the second two, but we want a
### data.frame that extends DocCollection, so that methods based on
### the DocCollection API can restrict their signature to
### DocCollection.
###

setClass("DocCollection")
setClass("DocList", contains = c("list", "DocCollection"),
         validity = function(object) {
           if (any(vapply(lapply(object, names), is.null, logical(1)) &
                   !vapply(object, is.null, logical(1))))
             "all non-NULL elements must have names"
         })
setClass("DocDataFrame", contains = c("data.frame", "DocCollection"))


### ---------------------------------------------------------------------------
### Reference class wrapper 
###


setRefClass("DocCollectionRef",
            fields = list(docs = "DocCollection"))

