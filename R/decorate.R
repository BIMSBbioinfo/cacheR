#' Infix decorator operator.
#' Left-hand side: a decorator object (class "decorator")
#' Right-hand side: a function to decorate.
#' @export
`%@%` <- function(decorator, f) {
  if (!is.function(f)) {
    stop("Right-hand side of %@% must be a function.", call. = FALSE)
  }
  UseMethod("%@%", decorator)
}

#' Fallback: object is not a decorator.
#' @export
`%@%.default` <- function(decorator, f) {
  stop(
    sprintf(
      "%s (class: %s) is not a decorator",
      deparse(substitute(decorator)),
      paste(class(decorator), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' Core method: apply a decorator to a function.
#' Supports chaining: d1 %@% d2 %@% function(...) ...
#' @export
`%@%.decorator` <- function(decorator, f) {
  # calls: list(decorator, f) plus possibly more if chained
  mc <- match.call(expand.dots = FALSE)
  decorator_calls <- as.list(mc)[-1L]  # drop the `%@%` symbol
  
  # Patch delayed decorator.
  patched_calls <- attr(decorator, "calls", exact = TRUE)
  if (!is.null(patched_calls)) {
    # first element of decorator_calls is the decorator itself,
    # which is already represented in patched_calls.
    decorator_calls <- c(patched_calls, decorator_calls[-1L])
  }
  
  # If RHS is itself a decorator, create a *combined* decorator lazily.
  if (inherits(f, "decorator")) {
    .delayed_decorate(decorator, f, decorator_calls)
  } else {
    # Actual decoration: apply decorator to f
    wrapped <- decorator(f)
    
    # All but the last element of decorator_calls correspond to decorators,
    # last element is the function itself.
    prettify(
      f = wrapped,
      original = f,
      decorator_calls = decorator_calls[-length(decorator_calls)]
    )
  }
}

#' Turn a function into a decorator.
#' @export
decorator <- function(f) {
  stopifnot(is.function(f))
  structure(
    f,
    class = c("decorator", setdiff(class(f), "decorator"))
  )
}

# Make `decorator` itself a decorator, as in the original code.
decorator <- decorator(decorator)

#' Pretty-print decorated functions.
#' @export
print.decorated <- function(x, useSource = TRUE, ...) {
  bare_fun <- function(f) {
    bare <- unclass(f)
    attr(bare, "decorators") <- NULL
    bare
  }
  
  fun_def <- capture.output(
    print.function(bare_fun(x), useSource = useSource, ...)
  )
  
  decorators <- attr(x, "decorators", exact = TRUE)
  if (!is.null(decorators)) {
    for (d in decorators) {
      cat(deparse(d), "%@%\n")
    }
  }
  
  cat(fun_def, sep = "\n")
  invisible(x)
}

# If you're in a package, do this in .onLoad instead:
# box::register_S3_method('print', 'decorated', print.decorated)

#' Attach pretty metadata to a decorated function.
#' @export
prettify <- function(f, original, decorator_calls) {
  # keep any existing srcref if present, otherwise use the original body
  if (is.null(attr(original, "srcref"))) {
    attr(f, "srcref") <- body(original)
  } else {
    attr(f, "srcref") <- attr(original, "srcref")
  }
  
  attr(f, "decorators") <- decorator_calls
  class(f) <- unique(c("decorated", class(f)))
  f
}

#' Get a "pretty" code representation for a function.
#' @export
pretty_code <- function(f) {
  srcref <- attr(f, "srcref", exact = TRUE)
  if (is.null(srcref)) body(f) else srcref
}

#' Combine two decorators lazily into a new decorator.
#' @export
.delayed_decorate <- function(d1, d2, decorator_calls) {
  stopifnot(inherits(d1, "decorator"), inherits(d2, "decorator"))
  
  structure(
    decorator(function(f) d1(d2(f))),
    calls = decorator_calls
  )
}
