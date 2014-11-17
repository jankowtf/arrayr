#' @title
#' Class: Array
#'
#' @description
#' Class representing arrays.
#'    
#' @field .array \code{\link{list}}.
#'  List that serves as an array container.
#' @example inst/examples/Array.r
#' @template author
#' @template references
#' @import conditionr
#' @import R6
#' @export
Array <- R6Class(
  classname = "Array",
#   inherit = ReactrObservable,
  portable = TRUE,
  public = list(
    .array = "list",
    initialize = function(...) {
      self$.array <- structure(list(), names = character())
      value <- list(...)
      value_length <- length(value)
      if (value_length > 0)  {
        if (value_length == 1 && is.null(names(value))) {
          value <- value[[1]]
        } else if (value_length >= 2 && is.null(names(value))) {
          value <- unlist(value,recursive = FALSE)
        }        
        self$.array <- value
      }
    },
    add = function(..., id = character(), dups = TRUE, strict = 0) {  
      id <- as.character(id)
      value <- list(...)
      if (!length(id)) {
        nms <- names(self$.array)
        out <- unlist(lapply(value, function(ii) {
          value <- ii
          out <- if (!dups && any(names(value) %in% nms)) {
            if (strict == 0) {
              FALSE
            } else if (strict == 1) {
              conditionr::signalCondition(
                condition = "Duplicates",
                msg = c(
                  Reason = "duplicates",
                  IDs = names(value)[which(names(value) %in% names(self$.array))]
                ),
                ns = "Array",
                type = "warning"
              )  
              FALSE
            } else if (strict == 2) {
              conditionr::signalCondition(
                condition = "Duplicates",
                msg = c(
                  Reason = "duplicates",
                  IDs = names(value)[which(names(value) %in% names(self$.array))]
                ),
                ns = "Array",
                type = "error"
              )
            }
          } else {
            self$.array <- c(self$.array, value)  
            structure(rep(TRUE, length(value)), names = names(value)) 
          }
          out
        }))
      } else {
        out <- if (length(value) != length(id)) {
          if (strict == 0) {
            FALSE
          } else if (strict == 1) {
            conditionr::signalCondition(
              condition = "InvalidConstellation",
              msg = c(
                Reason = "lengths differ",
                "Length `value`" = length(value),
                "Length `id`" = length(id)
              ),
              ns = "Array",
              type = "warning"
            )  
            FALSE
          } else if (strict == 2) {
            conditionr::signalCondition(
              condition = "InvalidConstellation",
              msg = c(
                Reason = "lengths differ",
                "Length `value`" = length(value),
                "Length `id`" = length(id)
              ),
              ns = "Array",
              type = "error"
            )
          }
        } else {
          TRUE
        }
        if (out) {
          nms <- names(self$.array)
          out <- sapply(seq(along = value), function(ii) {
            value <- list(value[[ii]])
            names(value) <- id[[ii]]
            
            if (!dups && any(id[[ii]] %in% nms)) {
              out <- if (strict == 0) {
                structure(FALSE, names = id[[ii]])
              } else if (strict == 1) {
                conditionr::signalCondition(
                  condition = "Duplicates",
                  msg = c(
                    Reason = "duplicates",
                    IDs = id[which(id %in% names(self$.array))]
                  ),
                  ns = "Array",
                  type = "warning"
                )  
                structure(FALSE, names = id[[ii]])
              } else if (strict == 2) {
                conditionr::signalCondition(
                  condition = "Duplicates",
                  msg = c(
                    Reason = "duplicates",
                    IDs = id[which(id %in% names(self$.array))]
                  ),
                  ns = "Array",
                  type = "error"
                )
              }
            } else {
              self$.array <- c(self$.array, value)  
              structure(rep(TRUE, length(value)), names = names(value))
            }
          })
        }
        out
      }
      out
    },
    copy = function(from, to, dups = TRUE, strict = 0) {
      from <- as.character(from)
      to <- as.character(to)
      length_diff <- length(from) != length(to) 
      has_dups <- !dups && any(idx_dups <- to %in% names(self$.array))
      out <- if (length_diff || has_dups) {
      if (length_diff) {
          msg = c(
            Reason = "lengths differ",
            "Length `from`" = length(from),
            "Length `to`" = length(to)
          )
        }
        if (has_dups) {
          msg = c(
            Reason = "duplicates",
            Duplicates = paste(to[idx_dups], collapse = ", ")
          )
        }
        if (strict == 0) {
          structure(rep(FALSE, length(from)), names = from)
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidConstellation",
            msg = msg,
            ns = "Array",
            type = "warning"
          )
          structure(rep(FALSE, length(from)), names = from)
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidConstellation",
            msg = msg,
            ns = "Array",
            type = "error"
          )
        }
      } else {
        structure(rep(TRUE, length(from)), names = from)
      }
      if (all(out)) {
        from_to <- list(from = from, to = to)
        out <- sapply(seq(along = from), function(ii) {
          from <- from_to$from[[ii]]
          to <- from_to$to[[ii]]
          out <- if (self$exists(from)) {
            self$add(self$get(from), id = to)
            TRUE
          } else {
            out <- if (strict == 0) {
              FALSE
            } else if (strict == 1) {
              conditionr::signalCondition(
                condition = "InvalidId",
                msg = c(
                  Reason = "Invalid ID",
                  ID = from
                ),
                ns = "Array",
                type = "warning"
              )
              FALSE
            } else if (strict == 2) {
              conditionr::signalCondition(
                condition = "InvalidId",
                msg = c(
                  Reason = "Invalid ID",
                  ID = from
                ),
                ns = "Array",
                type = "error"
              )
            }
          }
        })
        names(out) <- from
      }
      out
    },
    clear = function() {
      self$.array <- structure(list(), names = character())
      TRUE
    },
    exists = function(id) {
      sapply(as.character(id), function(ii) {
        ii %in% names(self$.array)
      })
    },
    get = function(id = character(), default = NULL, inner = TRUE, 
                   simplify = TRUE, strict = 0) {
      id <- as.character(id)
      out <- if (strict > 0 && !all(idx <- id %in% names(self$.array))) {
        if (strict == 0) {
          TRUE
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidId",
            msg = c(
              Reason = "invalid ID(s)",
              IDs = id[!idx]
            ),
            ns = "Array",
            type = "warning"
          )  
          TRUE
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidId",
            msg = c(
              Reason = "invalid ID(s)",
              IDs = id[!idx]
            ),
            ns = "Array",
            type = "error"
          )
        }
      } else {
        TRUE
      }
      out <- if (out) {
        if (length(id)) {
          out <- lapply(id, function(ii) {
            out <- if (inner) self$.array[[ii]] else self$.array[ii]
            if (!is.null(names(out)) && all(is.na(names(out)))) {
              out <- default
            }
            out
          })
          names(out) <- id
          if (length(id) == 1 && length(out) == 1) {
            out <- out[[1]]
          }
          out
        } else {
          self$.array
        }
      } else {
        default
      }
      out
    },
    index = function(id, strict = 0, simplify = FALSE) {
      id <- as.character(id)
      idx <- sort(id) %in% sort(names(self$.array))
      out <- if (strict > 0 && !all(idx)) {
        if (strict == 0) {
          idx
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidId",
            msg = c(
              Reason = "invalid ID(s)",
              IDs = id[!idx]
            ),
            ns = "Array",
            type = "warning"
          )  
          idx
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidId",
            msg = c(
              Reason = "invalid ID(s)",
              IDs = id[!idx]
            ),
            ns = "Array",
            type = "error"
          )
        }
      } else {
        idx
      }
#       if (any(out)) {
        out <- rep(NA, length(id))
        val <- as.numeric(which(sort(names(self$.array)) %in% sort(id)))
        if (length(val)) {
          out[idx] <- val
        }
        names(out) <- sort(id)
        if (any(!idx) && simplify) {
          out <- as.numeric(out[-which(is.na(out))])
        }
#       }  
        out
    },
    rm = function(id, strict = 0) {
      id <- as.character(id)
      nms <- names(self$.array)
      idx <- nms %in% id
      names(idx) <- nms   
      out <- if (!all(idx) || !length(idx)) {
        if (strict == 0) {
          if (length(idx)) TRUE else FALSE
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidId",
            msg = c(
              Reason = "invalid ID",
              IDs = paste(id[!idx], collapse = ", ")
            ),
            ns = "Array",
            type = "warning"
          )
          if (length(idx)) TRUE else FALSE
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidId",
            msg = c(
              Reason = "invalid ID",
              IDs = paste(id[!idx], collapse = ", ")
            ),
            ns = "Array",
            type = "error"
          )
        }
      } else {
        TRUE
      }
      if (out) {
        self$.array <- self$.array[-which(idx)]
      } 
      out <- id %in% nms
      names(out) <- id
      out
    },
    rmFirst = function(n = 1, strict = 0, simplify = FALSE) {
      scope <- length(self$.array)
      out <- if (n > scope) {
        if (strict == 0) {
          out <- structure(rep(FALSE, n), names = 1:n)
          out[1:scope] <- TRUE
          out
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidScope",
            msg = c(
              Reason = "invalid scope",
              Scope = n,
              Length = scope
            ),
            ns = "Array",
            type = "warning"
          )
          out <- structure(rep(FALSE, n), names = 1:n)
          out[1:scope] <- TRUE
          out
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidScope",
            msg = c(
              Reason = "invalid scope",
              Scope = n,
              Length = scope
            ),
            ns = "Array",
            type = "error"
          )
        }
      } else {
        out <- structure(rep(TRUE, n), names = 1:n)
      }
      self$.array <- self$.array[-(1:min(n, scope))]
      if (simplify) {
        out <- all(out)
      } 
      out
    },
    rmLast = function(n = 1, strict = 0, simplify = FALSE) {
      scope <- length(self$.array)
      out <- if (n > scope || scope == 0) {
        if (strict == 0) {
          out <- structure(rep(FALSE, n), names = scope:(scope - (n - 1)))
          if (scope > 0) {
            out[1:scope] <- TRUE
          }
          out
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidScope",
            msg = c(
              Reason = "invalid scope",
              Scope = n,
              Length = scope
            ),
            ns = "Array",
            type = "warning"
          )
          out <- structure(rep(FALSE, n), names = scope:(scope - (n - 1)))
          if (scope > 0) {
            out[1:scope] <- TRUE
          }
          out
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidScope",
            msg = c(
              Reason = "invalid scope",
              Scope = n,
              Length = scope
            ),
            ns = "Array",
            type = "error"
          )
        }
      } else {
        out <- structure(rep(TRUE, n), names = scope:(scope - (n - 1)))
      }
      self$.array <- self$.array[-(max((scope - (n-1)), 1):scope)]
      if (simplify) {
        out <- all(out)
      } 
      out
    },
    set = function(..., id = character(), must_exist = TRUE, strict = 0) {      
      value <- list(...)
      if (!length(id)) {
        nms <- names(self$.array)
        out <- unlist(lapply(value, function(ii) {
          value <- ii        
          out <- if (must_exist && !all(names(value) %in% nms)) {
            if (strict == 0) {
              FALSE
            } else if (strict == 1) {
              conditionr::signalCondition(
                condition = "NonExistingComponent",
                msg = c(
                  Reason = "component does not exist",
                  IDs = names(value)[which(!names(value) %in% nms)]
                ),
                ns = "Array",
                type = "warning"
              )  
              FALSE
            } else if (strict == 2) {
              conditionr::signalCondition(
                condition = "NonExistingComponent",
                msg = c(
                  Reason = "component does not exist",
                  IDs = names(value)[which(!names(value) %in% names(self$.array))]
                ),
                ns = "Array",
                type = "error"
              )
            }
          } else {
            for (ii in names(value)) {
              self$.array[ii] <- value[[ii]]
            }
            structure(rep(TRUE, length(value)), names = names(value)) 
#             TRUE
          }
          out
        }))
      } else {
        out <- if (length(value) != length(id)) {
          if (strict == 0) {
            FALSE
          } else if (strict == 1) {
            conditionr::signalCondition(
              condition = "InvalidConstellation",
              msg = c(
                Reason = "lengths differ",
                "Length `value`" = length(value),
                "Length `id`" = length(id)
              ),
              ns = "Array",
              type = "warning"
            )  
            FALSE
          } else if (strict == 2) {
            conditionr::signalCondition(
              condition = "InvalidConstellation",
              msg = c(
                Reason = "lengths differ",
                "Length `value`" = length(value),
                "Length `id`" = length(id)
              ),
              ns = "Array",
              type = "error"
            )
          }
        } else {
          TRUE
        }
        if (out) {
          nms <- names(self$.array)
          out <- sapply(seq(along = value), function(ii) {
            value <- list(value[[ii]])
            names(value) <- id[[ii]]
            
            if (must_exist && !all(id[[ii]] %in% nms)) {
              out <- if (strict == 0) {
                structure(FALSE, names = id[[ii]])
              } else if (strict == 1) {
                conditionr::signalCondition(
                  condition = "NonExistingComponent",
                  msg = c(
                    Reason = "component does not exist",
                    IDs = id[which(!id %in% names(self$.array))]
                  ),
                  ns = "Array",
                  type = "warning"
                )  
                structure(FALSE, names = id[[ii]])
              } else if (strict == 2) {
                conditionr::signalCondition(
                  condition = "NonExistingComponent",
                  msg = c(
                    Reason = "component does not exist",
                    IDs = id[which(!id %in% names(self$.array))]
                  ),
                  ns = "Array",
                  type = "error"
                )
              }
            } else {
              for (ii in names(value)) {
                self$.array[ii] <- value[[ii]]
              } 
              structure(rep(TRUE, length(value)), names = names(value)) 
            }
          })
        }
        out
      }
      out
    }
  )
)


