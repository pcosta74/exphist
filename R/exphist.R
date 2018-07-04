globalVariables(c('N','K'), 'exphist')

#' Create an instance of a exponential histogram
#' @param epsilon maximum relative error
#' @param winsize size of the sliding window
#' @param duration time duration for timebased buckets
#' @return an object of class 'exphist'
#' @export
exphist <- function(winsize=100, epsilon = 0.05, duration=NULL) {
  stopifnot(is.numeric(winsize) && winsize > 1)
  stopifnot(is.numeric(epsilon) && 0 < epsilon && epsilon < 1)
  stopifnot(is.null(duration) || (is.numeric(duration) && duration > 0))

  this <- list(
      buckets = NULL,
      sizes   = NULL,
      tstamps = NULL,
      LAST  = 0,
      TOTAL = 0,
      N = winsize,
      K = ceiling(1/epsilon)
  )
  attr(this, 'duration') <- duration
  class(this)   <- c(class(this), 'exphist')
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
  f <- as.numeric(.is.last(x))
  with(x, sum(buckets, -0.5*f*buckets[1]))
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
#' @param ts a timestamp
#' @return the histogram holding the newly inserted value
##' @examples
#' ## Create the exp. hist.
#' eh <- exphist()
#' ## Compute the value
#' insert(eh, 1)
#' @export
insert <- function(x, val, ts=NULL) {
  UseMethod('insert', x)
}

#' @rdname insert
#' @method insert exphist
insert.default <- function(x, val, ts=NULL) {
  .class.mismatch(x)
}

#' @rdname insert
#' @method insert exphist
insert.exphist <- function(x, val, ts=NULL) {
  stopifnot(is.numeric(val) && length(val)== 1)
  stopifnot(is.null(ts) || is.numeric(ts))

  expired  <- TRUE
  if(is.numeric(attr(x, 'duration')) && is.numeric(ts)) {
    init <- is.null(attr(x, 'ts'))
    if(init) {
      attr(x, 'ts') <- ts
    }

    ts0 <- attr(x, 'ts')
    if(!init && ts0 >= ts) {
      stop(sprintf(.FMT.INV.TSP, ts0, ts), call.=FALSE)
    }

    expired <- (ts - ts0) >= attr(x,'duration')
    if(expired) {
      attr(x, 'ts') <- ts
    }
  }

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
      if(!is.null(buckets) && !expired) {
        n <- length(buckets)
        buckets[n] <- buckets[n] + val
      } else {
        tstamps <- c(tstamps + 1, 1)
        sizes   <- c(sizes, 1)
        buckets <- c(buckets, val)
        LAST  <- sizes[1]
        TOTAL <- TOTAL + 1
      }
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
  f <- as.numeric(.is.last(x))
  with(x, TOTAL - 0.5*f*LAST)
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

#' Determines if the last bucket is being used
#' @param x exponential histogram
#' @return TRUE is the last bucket is in use, FALSE otherwise
.is.last <- function(x) {
  with(x, length(buckets) > ceiling((K/2)*(log(1+2*N/K)+1)))
}
