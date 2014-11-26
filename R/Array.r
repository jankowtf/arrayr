#' @title
#' Class: Array
#'
#' @description
#' Class representing ListArrays.
#'    
#' @field .array \code{\link{list}}.
#'  List that serves as an Array container.
#' @example inst/examples/Array.r
#' @template author
#' @template references
#' @import conditionr
#' @import R6
#' @export
Array <- R6Class(
  classname = "Array",
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
    .order = function(input = NULL, decreasing = FALSE) {
      if (is.null(input)) {
        input <- names(self$.array)
      }
      idx <- grepl("^\\d*$", input)
      if (length(idx)) {
        input <- if (any(idx)) {
          if (!decreasing) {
            c(sort(as.numeric(input[idx]), decreasing = decreasing), 
              sort(input[!idx], decreasing = decreasing))
          } else {
            c(sort(input[!idx], decreasing = decreasing), 
              sort(as.numeric(input[idx]), decreasing = decreasing))
          }
        } else {
          sort(input, decreasing = decreasing)
        }
      }
      input
    },
    .set = function(value, id = character(), add = FALSE, intnum = TRUE, caller = NULL) {
      if (inherits(value, c("environment", "R6"))) {
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
    
## TODO: issue #6
        if (  (length(id) && inherits(id, c("integer", "numeric"))) ||
                (!is.null(caller) && caller == "add" && !add)
        ) {
          if (!is.null(caller) && caller == "add" && !add) {
            self$.array[name] <- value  
          } else {
            self$.array[id] <- value
          }
        } else {
          self$.array <- c(self$.array, value)
        }
        structure(TRUE, names = name)
      })
    },
    add = function(..., id = character(), dups = TRUE, 
      strict = 0, .must_exist = FALSE) {  
      
      caller <- as.character(match.call()[[1]])
      caller <- if (all(caller %in% c("self", "$", "add"))) {
        "add"
      } else {
        "other"
      }
      ## Ensure that argument is de factor disregarded if method is 
      ## not called by `self$add()`:
      if (caller != "add") {
        .must_exist <- FALSE
      }      
      value <- list(...)   
      nms <- names(self$.array)
      if (!length(id)) {
        out <- unlist(lapply(seq(along = value), function(ii) {
          name <- names(value[ii])
          value <- value[[ii]]
          if (!is.null(name)) {
            value <- structure(list(value), names = name)
          } else {
            name <- names(value)
          }
          has_non <- .must_exist && !all(idx_non <- names(value) %in% nms)
          has_dups <- !dups && any(idx_dups <- names(value) %in% nms)
          invalid_set <- (is.null(name) || name == "") && caller == "add"  
          
          out <- if (has_dups || has_non || invalid_set) {
            if (has_non) {
              msg <- c(
                Reason = "no such element",
                IDs = names(value)[which(!idx_non)]
              )
            }
            if (has_dups) {
              msg <- c(
                Reason = "element already exists",
                IDs = names(value)[which(idx_dups)]
              )
            }
            if (invalid_set) {
              msg <- c(
                Reason = "invalid set operation (no name/ID)"
              )
            }
            if (strict == 0) {
              structure(FALSE, names = names(value))
            } else if (strict == 1) {
              conditionr::signalCondition(
                condition = "Invalid",
                msg = msg,
                ns = "Array",
                type = "warning"
              )  
              structure(FALSE, names = names(value))
            } else if (strict == 2) {
              conditionr::signalCondition(
                condition = "Invalid",
                msg = msg,
                ns = "Array",
                type = "error"
              )
            }
          } else {
            self$.set(value, caller = caller, add = (!.must_exist && dups))
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
          out <- sapply(seq(along = value), function(ii) {
            value <- list(value[[ii]])
            id <- id[[ii]]
            if (!inherits(id, c("integer", "numeric"))) {
              names(value) <- id
            }
            has_non <- .must_exist && !all(idx_non <- id %in% nms)
            has_dups <- !dups && any(idx_dups <- id %in% nms)
            invalid_set <- (is.null(id) || id == "") && caller == "add"  
            if (has_dups || has_non || invalid_set) {
              if (has_non) {
                msg <- c(
                  Reason = "no such element",
                  IDs = names(value)[which(!idx_non)]
                )
              }
              if (has_dups) {
                msg <- c(
                  Reason = "element already exists",
                  IDs = names(value)[which(idx_dups)]
                )
              }
              if (invalid_set) {
                msg <- c(
                  Reason = "invalid set operation (no name/ID)"
                )
              }
              out <- if (strict == 0) {
                structure(FALSE, names = id)
              } else if (strict == 1) {
                conditionr::signalCondition(
                  condition = "Invalid",
                  msg = msg,
                  ns = "Array",
                  type = "warning"
                )  
                structure(FALSE, names = id)
              } else if (strict == 2) {
                conditionr::signalCondition(
                  condition = "Invalid",
                  msg = msg,
                  ns = "Array",
                  type = "error"
                )
              }
            } else {
              self$.set(value, id = id, caller = caller, add = (!.must_exist && dups))
            }
          })
        }
        out
      }
      out
    },
    copy = function(from, to, char = FALSE, dups = TRUE, 
      sorting = c(0, 1, 2), strict = c(0, 1, 2)) {
      
      ## Argument checks //
      sorting <- as.numeric(match.arg(as.character(sorting), 
        as.character(c(0, 1, 2))))    
      strict <- as.numeric(match.arg(as.character(strict), 
        as.character(c(0, 1, 2)))) 
      
      from <- if (inherits(from, c("numeric", "integer")) && !char) {
        from
      } else {
        as.character(from)  
      }
      to <- if (inherits(to, c("numeric", "integer")) && !char) {
        to
      } else {
        as.character(to)  
      }
      
      nms <- if (sorting == 0) {
        names(self$.array)
      } else {
        self$.order(decreasing = switch(sorting, "1" = FALSE, "2" = TRUE))
      }
      
      length_diff <- length(from) != length(to) 
      has_dups <- !dups && any(idx_dups <- to %in% nms)
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
            Reason = "element already exists",
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
            self$add(self$get(from), id = to, dups = dups)
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
#     exists = function(id) {
#       sapply(as.character(id), function(ii) {
#         ii %in% names(self$.array)
#       })
#     },
    exists = function(..., id = character(), char = FALSE) {
      if (!length(id)) {
        id <- unlist(list(...))
      }
      ## Index object type //
      out <- if (inherits(id, c("numeric", "integer")) && !char) {
        scope <- 1:length(self$.array)
        structure(id %in% scope, names = id)
      } else {
        id <- as.character(id)  
        id %in% names(self$.array)
      }
      names(out) <- id
      out
    },
    get = function(..., id = character(), char = FALSE, default = list(NULL, list()), 
      inner = TRUE, simplify = FALSE, sorting = c(0, 1, 2), strict = c(0, 1, 2)) {
      
      ## NOTE //
      ## Before refactoring, make absolutely sure that you run all unit tests
      ## as this method needs to fulfill the requirements of quite diverse 
      ## usage/calling scenarios!
      
      ## Argument checks //
      sorting <- as.numeric(match.arg(as.character(sorting), 
        as.character(c(0, 1, 2))))    
      strict <- as.numeric(match.arg(as.character(strict), 
        as.character(c(0, 1, 2))))    
      
      if (!length(id)) {
        id <- unlist(list(...))
      } 
      id_0 <- id
      nms <- if (sorting == 0) {
        names(self$.array)
      } else {
        self$.order(decreasing = switch(sorting, "1" = FALSE, "2" = TRUE))
      }

      ## Index object type //
      if (length(id)) {
        if (inherits(id, c("numeric", "integer")) && !char) {
          ## Approach 1 //
          id <- nms[id]     
          idx <- structure(id %in% nms, names = id)
          ## Approach 2 //
#           idx <- structure(rep(FALSE, length(id)), names = id)         
#           id <- na.exclude(nms[id])
#           if (length(id)) {
#             idx[which(id_0 %in% 1:length(nms))] <- TRUE
#           } 
          ## Approach 3 //
#           scope <- 1:length(self$.array)
#           idx <- structure(id %in% scope, names = id)
        } else {
#           idx <- id %in% names(self$.array)
#           id <- as.character(id)  
          id <- as.character(id)  
          idx <- id %in% nms
          names(idx) <- id
        }
      } else {
        idx <- FALSE
      }
      out <- if (length(id) && !all(idx)) {
        if (strict == 0) {
          TRUE
        } else if (strict == 1) {
          conditionr::signalCondition(
            condition = "Invalid",
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
            condition = "Invalid",
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

      ## Default //
      default <- if (length(idx) >= 2 && simplify) {
        default[[2]]
      } else {
        default[[1]]
      }

      ## Simplify //
      if (simplify) {
        idx <- idx[which(idx)]
        out <- length(idx) > 0
      }
      id <- as.character(names(idx))

      out <- if (out) {
        if (length(id)) {
          out <- lapply(seq(along = idx), function(ii) {  
            id <- names(idx[ii])
            value <- idx[[ii]]
            out <- if (value) {
              if (inner) self$.array[[id]] else self$.array[id]
            } else {
              default
            }
            out
          })
          names(out) <- id
          if (length(id_0) == 1 && length(out) == 1) {
            out <- out[[1]]
          } else {
            nms <- if (sorting == 0) {
              names(out)
            } else {
              self$.order(input = names(out), 
                decreasing = switch(sorting, "1" = FALSE, "2" = TRUE))
            }
            out <- out[nms]
            idx <- which(is.na(id))
            if (length(idx)) {
              names(out)[idx] <- ""
            }
          }
          out
        } else {
          self$.array[nms]
        }
      } else {
        default
      }
      out
    },
    index = function(..., id = character(), simplify = FALSE, 
      sorting = c(0, 1, 2), strict = 0) {
      
      ## Argument checks //
      sorting <- as.numeric(match.arg(as.character(sorting), 
        as.character(c(0, 1, 2))))    
      strict <- as.numeric(match.arg(as.character(strict), 
        as.character(c(0, 1, 2)))) 
      
      id <- if (!length(id)) {
        as.character(unlist(list(...)))
      } else {
        as.character(id)
      }
      nms <- if (sorting == 0) {
        names(self$.array)
      } else {
        self$.order(decreasing = switch(sorting, "1" = FALSE, "2" = TRUE))
      }
#       idx <- sort(id) %in% sort(names(self$.array))
      idx <- id %in% nms
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
        out <- rep(NA_integer_, length(id))
        val <- as.numeric(
          which(nms %in% id)
        )
        if (length(val)) {
          out[idx] <- val
        }
        names(out) <- id
        if (any(!idx) && simplify) {
#           out <- as.numeric(out[-which(is.na(out))])
          out <- na.omit(out)
          attributes(out)$na.action <- NULL
          attributes(out)$class <- NULL
        }
        out
    },
    rm = function(..., id = character(), fast = FALSE, 
      sorting = c(0, 1, 2), strict = c(0, 1, 2)) {
      
      ## Argument checks //
      sorting <- as.numeric(match.arg(as.character(sorting), 
        as.character(c(0, 1, 2))))    
      strict <- as.numeric(match.arg(as.character(strict), 
        as.character(c(0, 1, 2)))) 
      
      id <- if (!length(id)) {
        unlist(list(...))
      } 
      
      ## Fast vs. checked //
      ## If `fast = FALSE`, then there actually is no need to query names and
      ## check `id` against them!
      if (!fast) {     
        nms <- if (sorting == 0) {
          names(self$.array)
        } else {
          self$.order(decreasing = switch(sorting, "1" = FALSE, "2" = TRUE))
        }
        if (inherits(id, c("numeric", "integer"))) {
            id_0 <- id
            idx <- structure(rep(FALSE, length(id)), names = id)         
            id <- na.exclude(nms[id])
            if (length(id)) {
              idx[which(id_0 %in% 1:length(nms))] <- TRUE
            }      
        } else {
          id <- as.character(id)  
          idx <- id %in% nms
          names(idx) <- id
        }
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
              ns = "Array",
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
              ns = "Array",
              type = "error"
            )
          }
        } else {
          TRUE
        }   
        if (out) {
          self$.array[id] <- NULL
        } 
        out <- idx
      } else {
        ## Most efficient way to remove things if `fast = TRUE` //
        self$.array[id] <- NULL
        out <- structure(rep(NA, length(id)), names = id)
      }
      out
    },
    rmFirst = function(n = 1, strict = 0, simplify = FALSE) {
      scope <- length(self$.array)
      out <- if (n > scope) {
        if (strict == 0) {
          out <- structure(rep(FALSE, n), names = 1:n)
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
#       self$.array <- self$.array[-(1:min(n, scope))]
      self$.array[(1:min(n, scope))] <- NULL
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
      self$.array[(max((scope - (n-1)), 1):scope)] <- NULL
      if (simplify) {
        out <- all(out)
      } 
      out
    },
#     set = function(..., id = character(), must_exist = TRUE, strict = 0) {      
#       value <- list(...)
#       if (!length(id)) {
#         nms <- names(self$.array)
#         out <- unlist(lapply(value, function(ii) {
#           value <- ii        
#           out <- if (must_exist && !all(names(value) %in% nms)) {
#             if (strict == 0) {
#               FALSE
#             } else if (strict == 1) {
#               conditionr::signalCondition(
#                 condition = "NonExistingComponent",
#                 msg = c(
#                   Reason = "component does not exist",
#                   IDs = names(value)[which(!names(value) %in% nms)]
#                 ),
#                 ns = "Array",
#                 type = "warning"
#               )  
#               FALSE
#             } else if (strict == 2) {
#               conditionr::signalCondition(
#                 condition = "NonExistingComponent",
#                 msg = c(
#                   Reason = "component does not exist",
#                   IDs = names(value)[which(!names(value) %in% names(self$.array))]
#                 ),
#                 ns = "Array",
#                 type = "error"
#               )
#             }
#           } else {
#             for (ii in names(value)) {
#               self$.array[ii] <- value[[ii]]
#             }
#             structure(rep(TRUE, length(value)), names = names(value)) 
# #             TRUE
#           }
#           out
#         }))
#       } else {
#         out <- if (length(value) != length(id)) {
#           if (strict == 0) {
#             FALSE
#           } else if (strict == 1) {
#             conditionr::signalCondition(
#               condition = "InvalidConstellation",
#               msg = c(
#                 Reason = "lengths differ",
#                 "Length `value`" = length(value),
#                 "Length `id`" = length(id)
#               ),
#               ns = "Array",
#               type = "warning"
#             )  
#             FALSE
#           } else if (strict == 2) {
#             conditionr::signalCondition(
#               condition = "InvalidConstellation",
#               msg = c(
#                 Reason = "lengths differ",
#                 "Length `value`" = length(value),
#                 "Length `id`" = length(id)
#               ),
#               ns = "Array",
#               type = "error"
#             )
#           }
#         } else {
#           TRUE
#         }
#         if (out) {
#           nms <- names(self$.array)
#           out <- sapply(seq(along = value), function(ii) {
#             value <- list(value[[ii]])
#             names(value) <- id[[ii]]
#             
#             if (must_exist && !all(id[[ii]] %in% nms)) {
#               out <- if (strict == 0) {
#                 structure(FALSE, names = id[[ii]])
#               } else if (strict == 1) {
#                 conditionr::signalCondition(
#                   condition = "NonExistingComponent",
#                   msg = c(
#                     Reason = "component does not exist",
#                     IDs = id[which(!id %in% names(self$.array))]
#                   ),
#                   ns = "Array",
#                   type = "warning"
#                 )  
#                 structure(FALSE, names = id[[ii]])
#               } else if (strict == 2) {
#                 conditionr::signalCondition(
#                   condition = "NonExistingComponent",
#                   msg = c(
#                     Reason = "component does not exist",
#                     IDs = id[which(!id %in% names(self$.array))]
#                   ),
#                   ns = "Array",
#                   type = "error"
#                 )
#               }
#             } else {
#               for (ii in names(value)) {
#                 self$.array[ii] <- value[[ii]]
#               } 
#               structure(rep(TRUE, length(value)), names = names(value)) 
#             }
#           })
#         }
#         out
#       }
#       out
#     },
    set = function(..., id = character(), must_exist = TRUE, strict = 0) {      
      self$add(..., id = id, strict = strict, .must_exist = must_exist)
    }
  )
)


