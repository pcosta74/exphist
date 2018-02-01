globalVariables(c('N','K'), 'exphist')

#' Create an instance of a exponential histogram
#' @param epson maximum relative error
#' @param winsize size of the sliding window
#' @return an object of class 'exphist'
#' @export
exphist <- function(winsize=100, epson = 0.05) {
  stopifnot(is.numeric(winsize) && winsize > 1)
  stopifnot(is.numeric(epson) && 0 < epson && epson < 1)

  this <- list(
      buckets = NULL,
      sizes   = NULL,
      tstamps = NULL,
      LAST  = 0,
      TOTAL = 0,
      N = winsize,
      K = ceiling(1/epson)
  )
  class(this) <- c(class(this), 'exphist')
  return(this)
}

#' Class mismatch
#' @param x an object
#' @return NULL
.class.mismatch <- function(x, ...) {
  msg <- sprintf(.FMT.CLS.MIS, class(x))
  stop(msg, call.=FALSE)
}

#' Value
#'
#' The generic function \code{value} computes the accumulated value
#' in the buckets of the exponential histogram.
#'
#' For exponential histograms, \code{\link{value.exphist}} will be used.
#' For any other objects, \code{\link{value.default}} will be used
#' to spawn an error message. Use \code{methods(value)}.
#' @rdname value
#' @param x an exponential histogram
#' @return the accumulated value for the histogram
##' @examples
#' ## Create the exp. hist.
#' eh <- exphist()
#' ## Compute the value
#' value(eh)
#' @export
value <- function(x) {
  UseMethod('value', x)
}

#' @rdname value
#' @method value default
value.default <- function(x) {
  .class.mismatch(x)
}

#' @rdname value
#' @method value exphist
value.exphist <- function(x) {
  with(x, sum(buckets), -0.5*buckets[1])
}

#' Insert
#'
#' The generic function \code{insert} stores a value into one of
#' the the buckets of the exponential histogram.
#'
#' For exponential histograms, \code{\link{insert.exphist}} will be used.
#' For any other objects, \code{\link{insert.default}} will be used
#' to spawn an error message. Use \code{methods(insert)}.
#' @rdname insert
#' @param x an exponential histogram
#' @param val a value to be inserted into the histogram
#' @return the histogram holding the newly inserted value
##' @examples
#' ## Create the exp. hist.
#' eh <- exphist()
#' ## Compute the value
#' insert(eh, 1)
#' @export
insert <- function(x, val) {
  UseMethod('insert', x)
}

#' @rdname insert
#' @method insert exphist
insert.default <- function(x, val) {
  .class.mismatch(x)
}

#' @rdname insert
#' @method insert exphist
insert.exphist <- function(x, val) {
  stopifnot(is.numeric(val))

  .env <- environment()
  within(x, {
    on.exit(rm(.env))

    if(!is.null(tstamps) && tstamps[1] >= N) {
      LAST  <- LAST  - sizes[1]
      TOTAL <- TOTAL - sizes[1]
      tstamps <- tstamps[-1]
      sizes   <- sizes[-1]
      buckets <- buckets[-1]
    }

    if(val != 0) {
      tstamps <- c(tstamps + 1, 1)
      sizes   <- c(sizes, 1)
      buckets <- c(buckets, val)
      LAST  <- sizes[1]
      TOTAL <- TOTAL + 1
    }

    .env$L <- 2 + K/2
    while(TRUE) {
      .env$freq <- table(sizes, deparse.level=0)
      if(all(.env$freq < .env$L)) break

      .env$fst <- names(.env$freq)[min(which(.env$freq == .env$L))]
      .env$ndx <- utils::head(which(sizes == .env$fst), 2)

      buckets[.env$ndx[2]] <- sum(buckets[.env$ndx])
      sizes[.env$ndx[2]]   <- sum(sizes[.env$ndx])

      buckets <- buckets[-.env$ndx[1]]
      sizes   <- sizes[-.env$ndx[1]]
      tstamps <- tstamps[-.env$ndx[2]]

      LAST <- sizes[1]
    }
  })
}

#' Size
#'
#' The generic function \code{size} returns the total size of
#' the buckets of the exponential histogram.
#'
#' For exponential histograms, \code{\link{size.exphist}} will be used.
#' For any other objects, \code{\link{size.default}} will be used
#' to spawn an error message. Use \code{methods(size)}.
#' @rdname size
#' @param x an exponential histogram
#' @return the size of the buckets of the exponential histogram
##' @examples
#' ## Create the exp. hist.
#' eh <- exphist()
#' ## Compute the value
#' size(eh)
#' @export
size <- function(x) {
  UseMethod('size', x)
}

#' @rdname size
#' @method size default
size.default <- function(x) {
  .class.mismatch(x)
}

#' @rdname size
#' @method size exphist
size.exphist <- function(x) {
  with(x, TOTAL - 0.5*LAST)
}


#' Print method for class 'exphist'
#' @param x an exponential histogram.
#' @param ... further arguments passed to or from other methods.
#' @return NULL
print.exphist <- function(x, ...) {
  with(x, {
    cat('counters:\n')
    print(c('N'=N, 'K'=K, 'TOTAL'=TOTAL, 'LAST'=LAST), ...)
    cat('buckets:\n')
    print(buckets, ...)
    cat('sizes:\n')
    print(sizes, ...)
    cat('timestamps:\n')
    print(tstamps, ...)
    cat('\n')
  })
}
