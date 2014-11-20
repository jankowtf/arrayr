#' @title
#' Class: ListArray
#'
#' @description
#' Class representing ListArrays.
#'    
#' @field .array \code{\link{list}}.
#'  List that serves as an ListArray container.
#' @example inst/examples/ListArray.r
#' @template author
#' @template references
#' @import conditionr
#' @import R6
#' @export
ListArray <- R6Class(
  classname = "ListArray",
#   inherit = ReactrObservable,
  portable = TRUE,
  public = list(
    .array = "list",
    initialize = function(...) {
      self$.array <- structure(list(), names = character())
      value <<- list(...)
      value_length <- length(value)
      if (value_length > 0)  {
        if (value_length == 1 && is.null(names(value))) {
          value <- value[[1]]
        } else if (value_length >= 2 && is.null(names(value))) {
          value <- unlist(value,recursive = FALSE)
        }        
#         self$.array <- value
        self$.set(value)
      }
    },
    .set = function(value, id = character(), intnum = TRUE) {
      if (inherits(value, "environment")) {
        value <- list(value)
      }
      if (inherits(value, "data.frame")) {
        value <- list(value)
      }
## TODO: issue #3      
      sapply(seq(along = value), function(ii) {
        name <- names(value[ii])
        value <- value[[ii]]
        if (inherits(value, "integer") && intnum) {
          value <- as.numeric(value)
        }
        if (is.null(name) || name == "") {
          has_name <- FALSE
          .names <- names(self$.array)
          .nums <- grep("^\\d*$", .names, value = TRUE)
          name <- if (length(.nums) && !all(.nums == "")) {
            as.character(max(sort(as.numeric(.nums))) + 1)
          }
        } else {
          has_name <- TRUE
        }        
        if (class(value) != "list" || has_name) {
          value <- structure(list(value), names = name)
        }
        if (length(id) && inherits(id, c("integer", "numeric"))) {
          self$.array[id] <- value
        } else {
          self$.array <- c(self$.array, value)
        }
        structure(TRUE, names = name)
      })
    },
    add = function(..., id = character(), dups = TRUE, strict = 0) {  
#       id <- as.character(id)
      value <- list(...)   
      nms <- names(self$.array)
      if (!length(id)) {
        out <- unlist(lapply(seq(along = value), function(ii) {
          name <- names(value[ii])
          value <- value[[ii]]
          if (!is.null(name)) {
            value <- structure(list(value), names = name)
          }
          has_dups <- !dups && any(idx_dups <- names(value) %in% nms)
          out <- if (has_dups) {
            if (strict == 0) {
              structure(FALSE, names = names(value))
            } else if (strict == 1) {
              conditionr::signalCondition(
                condition = "Duplicates",
                msg = c(
                  Reason = "duplicates",
                  IDs = names(value)[which(idx_dups)]
                ),
                ns = "ListArray",
                type = "warning"
              )  
              structure(FALSE, names = names(value))
            } else if (strict == 2) {
              conditionr::signalCondition(
                condition = "Duplicates",
                msg = c(
                  Reason = "duplicates",
                  IDs = names(value)[which(idx_dups)]
                ),
                ns = "ListArray",
                type = "error"
              )
            }
          } else {
            self$.set(value)
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
              ns = "ListArray",
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
              ns = "ListArray",
              type = "error"
            )
          }
        } else {
          TRUE
        }
        if (out) {
          out <- sapply(seq(along = value), function(ii) {
            value <- list(value[[ii]])
            id <- id[[ii]]
            if (!inherits(id, c("integer", "numeric"))) {
              names(value) <- id
            }
            has_dups <- !dups && any(idx_dups <- id %in% nms)
            if (has_dups) {
              out <- if (strict == 0) {
                structure(FALSE, names = id)
              } else if (strict == 1) {
                conditionr::signalCondition(
                  condition = "Duplicates",
                  msg = c(
                    Reason = "duplicates",
                    IDs = id[which(idx_dups)]
                  ),
                  ns = "ListArray",
                  type = "warning"
                )  
                structure(FALSE, names = id)
              } else if (strict == 2) {
                conditionr::signalCondition(
                  condition = "Duplicates",
                  msg = c(
                    Reason = "duplicates",
                    IDs = id[which(idx_dups)]
                  ),
                  ns = "ListArray",
                  type = "error"
                )
              }
            } else {
              self$.set(value, id = id)
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
            ns = "ListArray",
            type = "warning"
          )
          structure(rep(FALSE, length(from)), names = from)
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidConstellation",
            msg = msg,
            ns = "ListArray",
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
                ns = "ListArray",
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
                ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
            ns = "ListArray",
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
                ns = "ListArray",
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
                ns = "ListArray",
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
              ns = "ListArray",
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
              ns = "ListArray",
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
                  ns = "ListArray",
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
                  ns = "ListArray",
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


