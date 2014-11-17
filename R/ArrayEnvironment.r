#' @title
#' Class: ArrayEnvironment
#'
#' @description
#' Class representing arrays.
#'    
#' @field .array \code{\link{environment}}.
#'  Environment that serves as an array container.
#' @example inst/examples/ArrayEnvironment.r
#' @template author
#' @template references
#' @import conditionr
#' @import R6
#' @export
ArrayEnvironment <- R6Class(
  classname = "ArrayEnvironment",
  portable = TRUE,
  public = list(
    .array = "environment",
    initialize = function(...) {
      self$.array <- new.env(parent = emptyenv())
      value <<- list(...)
      value_length <- length(value)
      if (value_length > 0)  {
        if (value_length == 1 && is.null(names(value))) {
          value <- value[[1]]
        } else if (value_length >= 2 && is.null(names(value))) {
          value <- unlist(value,recursive = FALSE)
        }        
        self$.set(value)
      }
    },
    .set = function(value) {
      sapply(seq(along = value), function(ii) {
        name <- names(value[ii])
        value <- value[[ii]]
        if (is.null(name) || name == "") {
          .names <- ls(self$.array, all.names = TRUE)
          .nums <- grep("^\\d*$", .names, value = TRUE)
          name <- if (length(.nums)) {
            as.character(max(sort(as.numeric(.nums))) + 1)
          } else {
            "1"
          }
        }
        assign(name, value, envir = self$.array)  
        TRUE
      })
    },
    add = function(..., id = character(), must_exist = FALSE, dups = TRUE, strict = 0) {      
      id <- as.character(id)
      value <- list(...)
      if (!length(id)) {
        nms <- ls(self$.array, all.names = TRUE)
        out <- unlist(lapply(value, function(ii) {
          value <- ii
          has_non <- must_exist && !all(idx_non <- names(value) %in% nms)
          has_dups <- !dups && any(idx_dups <- names(value) %in% nms)
          out <- if (has_non || has_dups) {      
            if (has_non) {
              msg <- c(
                Reason = "no such element",
                IDs = names(value)[which(!idx_non)]
              )
            }
            if (has_dups) {
              msg <- c(
                Reason = "duplicates",
                IDs = names(value)[which(idx_dups)]
              )
            }
            if (strict == 0) {
              FALSE
            } else if (strict == 1) {
              conditionr::signalCondition(
                condition = "Invalid",
                msg = msg,
                ns = "ArrayEnvironment",
                type = "warning"
              )  
              FALSE
            } else if (strict == 2) {
              conditionr::signalCondition(
                condition = "Invalid",
                msg = msg,
                ns = "ArrayEnvironment",
                type = "error"
              )
            }
          } else {
            self$.set(value)
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
              condition = "Invalid",
              msg = c(
                Reason = "lengths differ",
                "Length `value`" = length(value),
                "Length `id`" = length(id)
              ),
              ns = "ArrayEnvironment",
              type = "warning"
            )  
            FALSE
          } else if (strict == 2) {
            conditionr::signalCondition(
              condition = "Invalid",
              msg = c(
                Reason = "lengths differ",
                "Length `value`" = length(value),
                "Length `id`" = length(id)
              ),
              ns = "ArrayEnvironment",
              type = "error"
            )
          }
        } else {
          TRUE
        }
        if (out) {
          nms <- ls(self$.array, all.names = TRUE)
          out <- sapply(seq(along = value), function(ii) {
            value <- list(value[[ii]])
            names(value) <- id[[ii]]
            
            has_non <- must_exist && !all(idx_non <- id[[ii]] %in% nms)
            has_dups <- !dups && any(idx_dups <- id[[ii]] %in% nms)
            if (has_dups || has_non) {
              if (has_non) {
                msg <- c(
                  Reason = "no such element",
                  IDs = id[[ii]][which(!idx_non)]
                )
              }
              if (has_dups) {
                msg <- c(
                  Reason = "duplicates",
                  IDs = id[[ii]][which(idx_dups)]
                )
              }
              out <- if (strict == 0) {
                structure(FALSE, names = id[[ii]])
              } else if (strict == 1) {
                conditionr::signalCondition(
                  condition = "Invalid",
                  msg = msg,
                  ns = "ArrayEnvironment",
                  type = "warning"
                )  
                structure(FALSE, names = id[[ii]])
              } else if (strict == 2) {
                conditionr::signalCondition(
                  condition = "Invalid",
                  msg = msg,
                  ns = "ArrayEnvironment",
                  type = "error"
                )
              }
            } else {
              self$.set(value)
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
      has_dups <- !dups && any(idx_dups <- to %in% ls(self$.array, all.names = TRUE))
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
            ns = "ArrayEnvironment",
            type = "warning"
          )
          structure(rep(FALSE, length(from)), names = from)
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "InvalidConstellation",
            msg = msg,
            ns = "ArrayEnvironment",
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
                ns = "ArrayEnvironment",
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
                ns = "ArrayEnvironment",
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
      rm(list = ls(self$.array, all.names = TRUE), envir = self$.array)
      TRUE
    },
    exists = function(id) {
      sapply(as.character(id), function(ii) {
        exists(ii, envir = self$.array, inherits = FALSE)
      })
    },
    get = function(id = character(), as_list = FALSE, default = NULL, 
                   inner = TRUE, simplify = TRUE, strict = 0) {
      id <- as.character(id)
      out <- if (strict > 0 && !all(idx <- id %in% ls(self$.array, all.names = TRUE))) {
        if (strict == 0) {
          TRUE
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "InvalidId",
            msg = c(
              Reason = "invalid ID(s)",
              IDs = id[!idx]
            ),
            ns = "ArrayEnvironment",
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
            ns = "ArrayEnvironment",
            type = "error"
          )
        }
      } else {
        TRUE
      }
      out <- if (out) {
        if (length(id)) {
          out <- lapply(id, function(ii) {
#             out <- get(ii, envir = self$.array, inherits = FALSE)
            this <- as.list(self$.array)
            out <- if (inner) this[[ii]] else this[ii]
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
          if (as_list) {
            as.list(self$.array)
          } else {
            self$.array
          }
        }
      } else {
        default
      }
      out
    },
    index = function(id, strict = 0, simplify = FALSE) {
      id <- as.character(id)
      idx <- sort(id) %in% sort(ls(self$.array, all.names = TRUE))
      out <- if (strict > 0 && !all(idx)) {
        if (strict == 0) {
          idx
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "Invalid",
            msg = c(
              Reason = "invalid ID(s)",
              IDs = id[!idx]
            ),
            ns = "ArrayEnvironment",
            type = "warning"
          )  
          idx
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "Invalid",
            msg = c(
              Reason = "invalid ID(s)",
              IDs = id[!idx]
            ),
            ns = "ArrayEnvironment",
            type = "error"
          )
        }
      } else {
        idx
      }
#       if (any(out)) {
        out <- rep(NA, length(id))
        val <- as.numeric(which(sort(ls(self$.array, all.names = TRUE)) %in% sort(id)))
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
      idx <- sapply(id, exists, envir = self$.array, inherits = FALSE)
      out <- if (!any(idx)) {       
        if (strict == 0) {
          FALSE
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "Invalid",
            msg = c(
              Reason = "invalid ID",
              IDs = paste(id[!idx], collapse = ", ")
            ),
            ns = "ArrayEnvironment",
            type = "warning"
          )
          FALSE
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "Invalid",
            msg = c(
              Reason = "invalid ID",
              IDs = paste(id[!idx], collapse = ", ")
            ),
            ns = "ArrayEnvironment",
            type = "error"
          )
        }
      } else {
        TRUE
      }
      if (out) {
        rm(list = id[idx], envir = self$.array, inherits = FALSE)
      } 
      out <- idx
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
            condition = "Invalid",
            msg = c(
              Reason = "invalid scope",
              Scope = n,
              Length = scope
            ),
            ns = "ArrayEnvironment",
            type = "warning"
          )
          out <- structure(rep(FALSE, n), names = 1:n)
          out[1:scope] <- TRUE
          out
        } else if (strict == 2) {
          conditionr::signalCondition(
            condition = "Invalid",
            msg = c(
              Reason = "invalid scope",
              Scope = n,
              Length = scope
            ),
            ns = "ArrayEnvironment",
            type = "error"
          )
        }
      } else {
        out <- structure(rep(TRUE, n), names = 1:n)
      }
      nms <- ls(self$.array, all.names = TRUE)
      idx <- grepl("^\\d*$", nms)
      if (length(idx)) {
        nms <- if (any(idx)) {
          c(sort(as.numeric(nms[idx])), sort(nms[!idx]))
        } else {
          sort(nms)
        }
        rm(list = nms[1:min(n, scope)], envir = self$.array)
      }
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
            ns = "ArrayEnvironment",
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
            ns = "ArrayEnvironment",
            type = "error"
          )
        }
      } else {
        out <- structure(rep(TRUE, n), names = scope:(scope - (n - 1)))
      }
      nms <- ls(self$.array, all.names = TRUE)
      idx <- grepl("^\\d*$", nms)
      if (length(idx)) {
        nms <- if (any(idx)) {
          c(sort(as.numeric(nms[idx])), sort(nms[!idx]))
        } else {
          sort(nms)
        }
        rm(list = nms[(max((scope - (n-1)), 1):scope)], envir = self$.array)
      }
      if (simplify) {
        out <- all(out)
      } 
      out
    },
    set = function(..., id = character(), must_exist = TRUE, dups = TRUE, strict = 0) {      
      self$add(..., id = id, must_exist = must_exist, dups = dups, strict = strict)
    }
  )
)


