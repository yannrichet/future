## Used only internally
constant <- function(value, ...) {
  ConstantFuture(value=value, ...)
}
class(constant) <- c("constant", "sequential", "future", "function")
