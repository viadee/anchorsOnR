#' Shutdown the anchors java server.
#'
#' Attempts to shutdown a running Anchors instance.

#' @param control Object of class \code{anchors_control}. Must have a slot \code{connection} representing a socketConnection.
#' @return this method will shutdown the socketConnection and return a nullified control object.
#' @export
shutdown <- function(control = NULL){
  if(is.null(control$connection)){
    stopf("AnchorsControl Object does not maintain a connection object")
  }

  con = control$connection

  cat("Shutting down Anchors JVM: ");
  message = rjson::toJSON(list("quit" = 1))
  writeLines(message, con)
  close(con)
  cat("Anchors has been successfully terminated.")
  control <- NULL
  return(control)
}


#' Initialize and Connect to anchors
#'
#' Attempts to start and/or connect to an Anchors instance.
#' @param ip Object of class \code{character} representing the IP address of the server where Anchors is running.
#' @param port Object of class \code{numeric} representing the port number of the Anchors server.
#' @param name (Optional) A \code{character} string representing the Anchors cluster name.
#' @param startAnchors (Optional) A \code{logical} value indicating whether to try to start Anchors from R if no connection with Anchors is detected. This is only possible if \code{ip = "localhost"} or \code{ip = "127.0.0.1"}.  If an existing connection is detected, R does not start Anchors.
#' @return this method will load it and return a socketConnection
#' @export
initAnchors <- function(ip = "localhost", port = 6666, name = NA_character_, startAnchors = TRUE) {

  if(!is.character(ip) || length(ip) != 1L || is.na(ip) || !nzchar(ip))
    stop("`ip` must be a non-empty character string")
  if(!is.numeric(port) || length(port) != 1L || is.na(port) || port < 0 || port > 65536)
    stop("`port` must be an integer ranging from 0 to 65536")
  if(!is.character(name) && !nzchar(name))
    stop("`name` must be a character string or NA_character_")
  if(!is.logical(startAnchors) || length(startAnchors) != 1L || is.na(startAnchors))
    stop("`startAnchors` must be TRUE or FALSE")

  con = NULL

  con = tryCatch({
    con = socketConnection(host = ip, port = port, timeout = 1)
  }, error = function(cond){
    # no need to handle failure
  }, warning = function(cond){
    # no need to handle failure
  })

  if (is.null(con) && startAnchors == TRUE){
    if (ip == "localhost" || ip == "127.0.0.1"){
      cat("\nAnchors is not running yet, starting it now...\n")
      stdout <- .anchors.getTmpFile("stdout")
      .anchors.startJar(ip = ip, port = port, name = name, ice_root = tempdir(), stdout = stdout, bind_to_localhost = FALSE, log_dir = NA, log_level = NA, context_path = NA)

      Sys.sleep(5L)

      cat("Starting Anchors JVM and connecting: ")
      con = tryCatch({
        con = socketConnection(host = ip, port = port, timeout = 5L)
      }, error = function(cond){
        message(cond)
        return(NULL)
      }, warning = function(cond) {
        message(cond)
        return(NULL)
      })
    }
  } else if(is.null(con) && startAnchors == FALSE){
    stopf("No running instance of Anchors found. Set 'startAnchors = TRUE' to start an Anchors instance.")
    return (null)
  }

  if (is.null(con)){
    stop("Anchors failed to start, stopping execution.")
  }
  cat("Connection successful!\n\n")
  .anchors.jar.env$port <- port #Ensure right port is called when quitting R
  cat("\n")

  return(con)
}


.anchors.pkg.path <- NULL
.anchors.pkg.path <- system.file("inst/java", "RModuleExtension.jar", package = "anchors")
.anchors.jar.env <- new.env()    # Dummy variable used to shutdown Anchors when R exits

.onLoad <- function(lib, pkg) {
  .anchors.pkg.path <<- file.path(lib, pkg)

  # installing RCurl requires curl and curl-config, which is typically separately installed
  rcurl_package_is_installed = length(find.package("RCurl", quiet = TRUE)) > 0L
  if(!rcurl_package_is_installed) {
    if(.Platform$OS.type == "unix") {
      # packageStartupMessage("Checking libcurl version...")
      curl_path <- Sys.which("curl-config")
      if(!nzchar(curl_path[[1L]]) || system2(curl_path, args = "--version") != 0L)
        stop("libcurl not found. Please install libcurl\n",
             "(version 7.14.0 or higher) from http://curl.haxx.se.\n",
             "On Linux systems you will often have to explicitly install\n",
             "libcurl-devel to have the header files and the libcurl library.")
    }
  }
}

#
# Returns error string if the check finds a problem with version.
# This implementation is supposed to blacklist known unsupported versions.
#
.anchors.check_java_version <- function(jver = NULL) {
  if(any(grepl("GNU libgcj", jver))) {
    return("Sorry, GNU Java is not supported for Anchors.")
  }
  if (any(grepl("^java version \"1\\.[1-7]\\.", jver))) {
    return(paste0("Your java is not supported: ", jver[1]))
  }
  return(NULL)
}


.anchors.startJar <- function(ip = "localhost", port = NULL, name = NULL, nthreads = -1,
                              max_memory = NULL, min_memory = NULL,
                              enable_assertions = TRUE, forceDL = FALSE, extra_classpath = NULL,
                              ice_root, stdout, log_dir, log_level, context_path, jvm_custom_args = NULL,
                              bind_to_localhost) {

  command <- .anchors.checkJava()

  if (missing(ice_root)) {
    stop("`ice_root` must be specified for .anchors.startJar")
  }

  # Note: Logging to stdout and stderr in Windows only works for R version 3.0.2 or later!
  stderr <- .anchors.getTmpFile("stderr")
  write(Sys.getpid(), .anchors.getTmpFile("pid"), append = FALSE)   # Write PID to file to track if R started anchors

  jar_file <- .anchors.downloadJar(overwrite = forceDL)
  jar_file <- paste0('"', jar_file, '"')

  # Throw an error if GNU Java is being used
  if (.Platform$OS.type == "windows") {
    command <- normalizePath(gsub("\"","",command))
  }

  jver <- tryCatch({system2(command, "-version", stdout = TRUE, stderr = TRUE)},
                   error = function(err) {
                     print(err)
                     stop("You have a 32-bit version of Java. Anchors works best with 64-bit Java.\n",
                          "Please download the latest Java SE JDK 8 from the following URL:\n",
                          "http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html")
                   }
  )
  jver_error <- .anchors.check_java_version(jver);
  if (!is.null(jver_error)) {
    stop(jver_error, "\n",
         "Please download the latest Java SE JDK 8 from the following URL:\n",
         "http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html")
  }
  if(any(grepl("Client VM", jver))) {
    warning("You have a 32-bit version of Java. Anchors works best with 64-bit Java.\n",
            "Please download the latest Java SE JDK 8 from the following URL:\n",
            "http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html")
    # Set default max_memory to be 1g for 32-bit JVM.
    if(is.null(max_memory)) max_memory = "1g"
  }

  if (.Platform$OS.type == "windows") {
    slashes_fixed_ice_root = gsub("\\\\", "/", ice_root)
  }  else {
    slashes_fixed_ice_root = ice_root
  }

  # Compose args
  mem_args <- c()
  if(!is.null(min_memory)) mem_args <- c(mem_args, paste0("-Xms", min_memory))
  if(!is.null(max_memory)) mem_args <- c(mem_args, paste0("-Xmx", max_memory))

  args <- mem_args
  ltrs <- paste0(sample(letters,3, replace = TRUE), collapse="")
  nums <- paste0(sample(0:9, 3,  replace = TRUE),     collapse="")

  if(is.na(name)) name <- paste0("Anchors_started_from_R_", gsub("\\s", "_", Sys.info()["user"]),"_",ltrs,nums)
  .anchors.jar.env$name <- name

#  if(enable_assertions) args <- c(args, "-ea")
#  if(!is.null(jvm_custom_args)) args <- c(args,jvm_custom_args)

  class_path <- paste0(c(jar_file, extra_classpath), collapse=.Platform$path.sep)
 # args <- c(args, "-cp", class_path, "water.AnchorsApp")
 # args <- c(args, "-cp", class_path)
  args <- c(args, "-jar", jar_file)
  #args <- c(args, "-name", name)
  #args <- c(args, "-ip", ip)
  #if (bind_to_localhost) {
  #  args <- c(args, "-web_ip", ip)
  #}
  args <- c(args, "-port", port)
  #args <- c(args, "-ice_root", slashes_fixed_ice_root)

  #if(!is.na(log_dir)) args <- c(args, "-log_dir", log_dir)
  #if(!is.na(log_level)) args <- c(args, "-log_level", log_level)
  #if(!is.na(context_path)) args <- c(args, "-context_path", context_path)

  #if(nthreads > 0L) args <- c(args, "-nthreads", nthreads)
  #if(!is.null(license)) args <- c(args, "-license", license)

   cat("\n")
   cat(        "Note:  In case of errors look at the following log files:\n")
   cat(sprintf("    %s\n", stdout))
   cat(sprintf("    %s\n", stderr))
   cat("\n")

  # Print a java -version to the console
  system2(command, c(mem_args, "-version"))
  cat("\n")
  # Run the real anchors java command
  rc = system2(command,
               args=args,
               stdout=stdout,
               stderr=stderr,
               wait=FALSE)
  if (rc != 0L) {
    stop(sprintf("Failed to exec %s with return code=%s", jar_file, as.character(rc)))
  }
}


.anchors.getTmpFile <- function(type) {
  if(missing(type) || !(type %in% c("stdout", "stderr", "pid")))
    stop("type must be one of 'stdout', 'stderr', or 'pid'")

  if(.Platform$OS.type == "windows") {
    usr <- gsub("[^A-Za-z0-9]", "_", Sys.getenv("USERNAME", unset="UnknownUser"))
  } else {
    usr <- gsub("[^A-Za-z0-9]", "_", Sys.getenv("USER", unset="UnknownUser"))
  }

  if(type == "stdout")
    file.path(tempdir(), paste("anchors", usr, "started_from_r.out", sep="_"))
  else if(type == "stderr")
    file.path(tempdir(), paste("anchors", usr, "started_from_r.err", sep="_"))
  else
    file.path(tempdir(), paste("anchors", usr, "started_from_r.pid", sep="_"))
}


.anchors.checkJava <- function() {
  if(nzchar(Sys.getenv("JAVA_HOME"))) {
    if(.Platform$OS.type == "windows") { file.path(Sys.getenv("JAVA_HOME"), "bin", "java.exe") }
    else                               { file.path(Sys.getenv("JAVA_HOME"), "bin", "java") }
  }
  else if(.Platform$OS.type == "windows") {
    # Note: Should we require the version (32/64-bit) of Java to be the same as the version of R?
    prog_folder <- c("Program Files", "Program Files (x86)")
    for(prog in prog_folder) {
      prog_path <- file.path("C:", prog, "Java")
      java_folder <- list.files(prog_path)

      for(java in java_folder) {
        path <- file.path(prog_path, java, "bin", "java.exe")
        if(file.exists(path)) return(path)
      }
    }
  }
  else if(nzchar(Sys.which("java")))
    Sys.which("java")
  else
    stop("Cannot find Java. Please install the latest JRE from\n",
         "http://www.oracle.com/technetwork/java/javase/downloads/index.html")
}

# This function returns a string to the valid path on the local filesystem of the anchors.jar file,
# or it calls stop() and does not return.
#
# It will download a jar file if it needs to.
.anchors.downloadJar <- function(overwrite = FALSE) {
  if(!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) stop("`overwrite` must be TRUE or FALSE")
  #.anchors.pkg.path <- system.file("inst/java", "RModuleExtension.jar", package = "anchors")
  .anchors.pkg.path <- system.file("java", "RModuleExtension.jar", package = "anchors")
  # PUBDEV-3534 hook to use arbitrary anchors.jar
  own_jar = Sys.getenv("ANCHORS_JAR_PATH")
  is_url = function(x) any(grepl("^(http|ftp)s?://", x), grepl("^(http|ftp)s://", x))
  if (nzchar(own_jar) && !is_url(own_jar)) {
    if (!file.exists(own_jar))
      stop(sprintf("Environment variable ANCHORS_JAR_PATH is set to '%s' but file does not exists, unset environment variable or provide valid path to anchors.jar file.", own_jar))
    return(own_jar)
  }

  if (!is.null(.anchors.pkg.path)){
    return(.anchors.pkg.path)
  }

  if (is.null(.anchors.pkg.path)) {
    pkg_path = dirname(system.file(".", package = "anchors"))
  } else {
    pkg_path = .anchors.pkg.path

    # Find Anchors-jar from testthat tests inside RStudio.
    if (length(grep("Anchors-dev/h2o-r/h2o$", pkg_path)) == 1L) {
      tmp = substr(pkg_path, 1L, nchar(pkg_path) - nchar("h2o-dev/h2o-r/h2o"))
      return(sprintf("%s/h2o-dev/build/h2o.jar", tmp))
    }
  }

  # Check for jar file in 'java' directory.
  if (! overwrite) {
    possible_file <- file.path(pkg_path, "java", "RModuleExtension.jar")
    if (file.exists(possible_file)) {
      return(possible_file)
    }
  }

  # Check for jar file in 'inst/java' directory.
  if (! overwrite) {
    possible_file <- file.path(pkg_path, "inst", "java", "RModuleExtension.jar")
    if (file.exists(possible_file)) {
      return(possible_file)
    }
  }
  return(possible_file)
#
#   branchFile <- file.path(pkg_path, "branch.txt")
#   branch <- readLines(branchFile)
#
#   buildnumFile <- file.path(pkg_path, "buildnum.txt")
#   version <- readLines(buildnumFile)
#
#   # mockup h2o package as CRAN release (no java/h2o.jar) hook h2o.jar url - PUBDEV-3534
#   jarFile <- file.path(pkg_path, "jar.txt")
#   if (file.exists(jarFile) && !nzchar(own_jar))
#     own_jar <- readLines(jarFile)
#
#   dest_folder <- file.path(pkg_path, "java")
#   if (!file.exists(dest_folder)) {
#     dir.create(dest_folder)
#   }
#
#   dest_file <- file.path(dest_folder, "h2o.jar")
#
#   # Download if h2o.jar doesn't already exist or user specifies force overwrite
#   if (nzchar(own_jar) && is_url(own_jar)) {
#     h2o_url = own_jar # md5 must have same file name and .md5 suffix
#     md5_url = paste(own_jar, ".md5", sep="")
#   } else {
#     base_url <- paste("s3.amazonaws.com/h2o-release/h2o", branch, version, "Rjar", sep = "/")
#     h2o_url <- paste("http:/", base_url, "h2o.jar", sep = "/")
#     # Get MD5 checksum
#     md5_url <- paste("http:/", base_url, "h2o.jar.md5", sep = "/")
#   }
#   # ttt <- getURLContent(md5_url, binary = FALSE)
#   # tcon <- textConnection(ttt)
#   # md5_check <- readLines(tcon, n = 1)
#   # close(tcon)
#   md5_file <- tempfile(fileext = ".md5")
#   download.file(md5_url, destfile = md5_file, mode = "w", cacheOK = FALSE, quiet = TRUE)
#   md5_check <- readLines(md5_file, n = 1L)
#   if (nchar(md5_check) != 32) stop("md5 malformed, must be 32 characters (see ", md5_url, ")")
#   unlink(md5_file)
#
#   # Save to temporary file first to protect against incomplete downloads
#   temp_file <- paste(dest_file, "tmp", sep = ".")
#   cat("Performing one-time download of h2o.jar from\n")
#   cat("    ", h2o_url, "\n")
#   cat("(This could take a few minutes, please be patient...)\n")
#   download.file(url = h2o_url, destfile = temp_file, mode = "wb", cacheOK = FALSE, quiet = TRUE)
#
#   # Apply sanity checks
#   if(!file.exists(temp_file))
#     stop("Error: Transfer failed. Please download ", h2o_url, " and place h2o.jar in ", dest_folder)
#
#   md5_temp_file = md5sum(temp_file)
#   md5_temp_file_as_char = as.character(md5_temp_file)
#   if(md5_temp_file_as_char != md5_check) {
#     cat("Error: Expected MD5: ", md5_check, "\n")
#     cat("Error: Actual MD5  : ", md5_temp_file_as_char, "\n")
#     stop("Error: MD5 checksum of ", temp_file, " does not match ", md5_check)
#   }
#
#   # Move good file into final position
#   file.rename(temp_file, dest_file)
#   return(dest_file[file.exists(dest_file)])
}

#' Retrieve an Anchors Connection
#'
#' Attempt to recover an anchors connection.
#'
#' @return Returns an \linkS4class{AnchorsConnection} object.
#' @export
anchors.getConnection <- function() {
  conn <- .attemptConnection()
  if (is.null(conn))
    stop("No active connection to an Anchors cluster. Did you run `anchors.init()` ?")
  conn
}

.attemptConnection <- function() {
  conn <- get("SERVER", .pkg.env)
  if (is.null(conn)) {
    # Try to recover an AnchorsConnection object from a saved session
    for (objname in ls(parent.frame(), all.names = TRUE)) {
      object <- get(objname, globalenv())
      if (is(object, "AnchorsConnection") && anchors.clusterIsUp(object)) {
        conn <- object
        assign("SERVER", conn, .pkg.env)
        break
      }
    }
  }
  conn
}

