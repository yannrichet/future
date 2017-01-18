#' Create a sequential future whose value will be in the current R session
#'
#' A sequential future is a future that is evaluated sequentially in the
#' current R session, which very closely resembles how expressions are
#' evaluated in R itself.  The main difference is that the expression
#' is evaluated in a local environment.
#'
#' @inheritParams future
#' @inheritParams multiprocess
#' @param local If TRUE, the expression is evaluated such that
#' all assignments are done to local temporary environment, otherwise
#' the assignments are done in the calling environment.
#'
#' @return A \link{SequentialFuture}.
#'
#' @example incl/sequential.R
#'
#' @details
#' The preferred way to create a sequential future is not to call these functions
#' directly, but to register them via \code{\link{plan}(sequential)} such that it
#' becomes the default mechanism for all futures.  After this
#' \code{\link{future}()} and \code{\link{\%<-\%}} will evaluate
#' the expressions using \emph{sequential futures}.
#'
#' @section Transparent futures:
#' Transparent futures are sequential futures configured to emulate how R
#' evaluates expressions as far as possible.  For instance, errors and
#' warnings are signaled immediately and assignments are done to the
#' calling environment (without \code{local()} as default for all other
#' types of futures).  This makes transparent futures ideal for
#' troubleshooting, especially when there are errors.
#'
#' @section Deprecated futures:
#' The \code{eager} and the \code{lazy} futures will soon be deprecated and
#' eventually removed.  The \code{eager} future is replaced by the
#' \code{sequential} future (and works identically).
#' The \code{lazy} future needs to be replaced by specifying lazyness when
#' creating futures, e.g. \code{f <- future({ ... }, lazy = TRUE)} and
#' \code{v \%<-\% { ... } \%lazy\% TRUE}.  There is no alternative for
#' \code{plan(lazy)}, because control of lazyness should be in the hands of
#' the developer and not the end user.
#'
#' @export
sequential <- function(expr, envir=parent.frame(), substitute=TRUE, lazy=FALSE, seed=NULL, globals=TRUE, local=TRUE, earlySignal=FALSE, label=NULL, ...) {
  if (substitute) expr <- substitute(expr)
  future <- SequentialFuture(expr=expr, envir=envir, substitute=FALSE, lazy=lazy, seed=seed, globals=globals, local=local, earlySignal=earlySignal, label=label, ...)
  if (!future$lazy) future <- run(future)
  invisible(future)
}
class(sequential) <- c("sequential", "future", "function")


#' @rdname sequential
#' @export
transparent <- function(expr, envir=parent.frame(), substitute=TRUE, lazy=FALSE, seed=NULL, globals=FALSE, local=FALSE, earlySignal=TRUE, label=NULL, ...) {
  if (substitute) expr <- substitute(expr)
  sequential(expr, envir=envir, substitute=FALSE, lazy=lazy, seed=seed, globals=globals, local=local, earlySignal=earlySignal, label=label, ...)
}
class(transparent) <- c("transparent", class(sequential))


#' @rdname sequential
#' @export
eager <- function(expr, envir=parent.frame(), substitute=TRUE, lazy=FALSE, seed=NULL, globals=TRUE, local=TRUE, earlySignal=FALSE, label=NULL, ...) {
  if (substitute) expr <- substitute(expr)
  local <- as.logical(local)

  future <- EagerFuture(expr=expr, envir=envir, substitute=FALSE, lazy=lazy, seed=seed, globals=globals, local=local, earlySignal=earlySignal, label=label, ...)
  if (!future$lazy) future <- run(future)
  invisible(future)
}
class(eager) <- c("eager", class(sequential))


#' @rdname sequential
#' @export
lazy <- function(expr, envir=parent.frame(), substitute=TRUE, lazy=TRUE, seed=NULL, globals=TRUE, local=TRUE, earlySignal=FALSE, label=NULL, ...) {
  if (substitute) expr <- substitute(expr)
  local <- as.logical(local)

  future <- LazyFuture(expr=expr, envir=envir, local=local, lazy=lazy, seed=seed, globals=globals, earlySignal=earlySignal, label=label, ...)
  if (!future$lazy) future <- run(future)
  invisible(future)
}
class(lazy) <- c("lazy", class(sequential))

## WORKAROUND:
## Avoid lazyeval::print.lazy() being called with print(lazy())
## https://github.com/HenrikBengtsson/future/issues/52
class(lazy) <- c("function", class(lazy))
