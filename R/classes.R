if (inherits(try(getRefClass("AnchorsConnectionMutableState"), silent = TRUE), "try-error")) {

#'
#' The AnchorsConnectionMutableState class
#'
#' This class represents the mutable aspects of a connection to an Anchors cluster.
#'
#' @name AnchorsConnectionMutableState
#' @slot session_id A \code{character} string specifying the Anchors session identifier.
#' @slot key_count A \code{integer} value specifying count for the number of keys generated for the \code{session_id}.
#' @aliases AnchorsConnectionMutableState
setRefClass("AnchorsConnectionMutableState",
            fields = list(session_id = "character", key_count = "integer"),
            methods = list(
              initialize =
                function(..., session_id = NA_character_, key_count = 0L) {
                  .self$initFields(session_id = session_id, key_count = key_count)
                  callSuper(...)
                }))
}




#'
#' The AnchorsConnection class.
#'
#' This class represents a connection to an Anchors server
#'
#' A new Anchors connection is established via the anchors.init() function, which takes as parameters
#' the `ip` and `port` of the machine running an instance to connect with. The default behavior
#' is to connect with a local instance of Anchors at port 54321, or to boot a new local instance if one
#' is not found at port 54321.
#' @slot ip A \code{character} string specifying the IP address of the H2O cluster.
#' @slot port A \code{numeric} value specifying the port number of the H2O cluster.
#' @slot name A \code{character} value specifying the name of the H2O cluster.
#' @slot mutable An \code{H2OConnectionMutableState} object to hold the mutable state for the H2O connection.
#' @aliases H2OConnection
#' @export
setClass("AnchorsConnection",
         representation(ip="character", port="numeric", name="character",
                        mutable="AnchorsConnectionMutableState"),
         prototype(ip           = NA_character_,
                   port         = NA_integer_,
                   name         = NA_character_,
                   mutable      = new("AnchorsConnectionMutableState")))

#setClassUnion("H2OConnectionOrNULL", c("H2OConnection", "NULL"))

#' @rdname H2OConnection-class
#' @param object an \code{H2OConnection} object.
#' @export
setMethod("show", "AnchorsConnection", function(object) {
  cat("IP Address:", object@ip,                 "\n")
  cat("Port      :", object@port,               "\n")
  cat("Name      :", object@name,               "\n")
  cat("Session ID:", object@mutable$session_id, "\n")
  cat("Key Count :", object@mutable$key_count,  "\n")
})
