## $Id: DBIODBC.R,v 1.7 2001/12/08 04:04:27 dj Exp dj $
## A DBI front-end to RODBC version 0.8-3

require(DBI, quietly = TRUE, warn.conflicts = FALSE)

"RODBC" <- 
function()
{
   pid <- .Call("RS_getpid")
   new("ODBCDriver", Id = newId(pid), driver.name = "RODBC",
       driver.version = "0.8-3", DBI.version = "0.1-2", 
       client.version = "NA", max.connections = 16)
}

## An ODBCObject is a VIRTUAL class that provides a mutable (closure) 
## slot "Id" to all other ODBC* classes.  The idea is that the slot Id 
## will be some kind of remote reference into a C structure that the C 
## code manages (and modifies!), but in the case of RODBC we simulate 
## this behaviour with the closure.  
## (TODO: Take a look at Luke's external references.)

## 
## Helper class "Id" (provides a mutable slot).
##

setClass("Id", representation(closure = "list"))
"newId" <- 
function(.Id = -1)
{
   length(.Id)    ## force eval of this, due to (bug?) in promises
   cls <- list(getId = function() .Id, setId = function(id) .Id <<- id)
   new("Id", closure = cls)
}
setMethod("format", "Id",
   def = function(x, ...)
      paste("(", paste(x@closure$getId(), collapse=","), ")", sep="")
)
setMethod("print", "Id", 
   def = function(x, ...) print(format(x), quote=FALSE, ...)
)
## Simplify subsetting and replacement of Id 
setMethod("[", "Id", 
   def = function(x, i, j, ..., drop = TRUE){
      if(missing(i)) 
         x@closure$getId() 
      else 
         x@closure$getId()[i]
   }
)
setReplaceMethod("[", "Id", 
   def = function(x, i, j, ..., value){
      out <- x@closure$getId()
      out[i] <- value
      x@closure$setId(out)
      x
   }
)

##
## Class: ODBCObject
##
## Base class for all ODBC objects.  It is virtual in order
## to group all ODBC* objects;  notice that it provides a mutable 
## slot Id to all ODBC objects (driver, connections, results).
##
setClass("ODBCObject", representation(Id = "Id", "DBIObject", "VIRTUAL"))
setMethod("format", "ODBCObject", def = function(x, ...) format(x@Id))
setMethod("print", "ODBCObject",
   def = function(x, ...) {
      if(isIdCurrent(x))
         str <- paste("<", class(x), ":", format(x), ">", sep = "")
      else 
         str <- paste("<Expired ", class(x), ":", format(x), ">", sep="")
      cat(str, "\n")
      invisible(NULL)
   }
)
setMethod("show", "ODBCObject", def = function(object) print(object))

"isIdCurrent" <- 
function(obj)
## verify that obj refers to a currently open/loaded database
{ 
   id <- obj@Id[ ]   ## its length can be > 1
   pid <- .Call("RS_getpid")
   pid == id[1] && all(0<=id)
}

##
## Class: ODBCDriver
##
## ODBCDrivers inherit a mutable "Id" slot from class ODBCObject,
## and it defines slots "driver.name", "driver.version", ... "max.connection"

setClass("ODBCDriver", representation( "DBIDriver", "ODBCObject",
   driver.name = "character", driver.version = "character", 
   DBI.version = "character", client.version = "character", 
   max.connections = "numeric")
)

setMethod("dbGetInfo", "ODBCDriver",
   def = function(dbObj, ...){
      ## return a list of slotname/value pairs
      if(!isIdCurrent(dbObj))
         stop(paste("Expired", class(dbObj), "object"))
      out <- list()
      for(sl in slotNames(dbObj)) 
         out[[sl]] <- slot(dbObj, sl)
      out
  }
)
setMethod("dbUnloadDriver", "ODBCDriver", 
   def = function(drv, ...) { 
      for(conn in dbListConnections(drv))
         dbDisconnect(con)
      drv@Id[1] <- -1
      TRUE
   }
)
## major hack, since I could not find how to verify that a channel
## id is current/valid.  Note that we're creating  *copies* of the
## existing connection objects and thus these copies will not reflect
## changes to the original objects.  In practice the Id should point
## into the C struct that manages the ODBC connections.
setMethod("dbListConnections", "ODBCDriver",
   def = function(drv, ...){
      out <- vector("list", 16)
      ind <- NULL
      k <- 0
      for(i in seq(from=0, to = 15)){
         dummy <- sqlTables(channel=i)
         if(is.numeric(dummy)) next
         k <- k + 1
         out[[k]] <- new("ODBCConnection", Id = newId(c(drv@Id[1],i)))
         ind <- c(ind, k)
      }
   out[ind]
   }
)

##
## Class: ODBCConnection
##
## Note that in a production version (i.e., if/when a future version of
## RODBC that doesn't require this DBI front-end) we could define 
## slots "dsn", "uid", "pwd", etc. for this class as we did for the
## ODBCDriver.  For simplicity, we won't do that here.

setClass("ODBCConnection", representation("DBIConnection", "ODBCObject"))

setMethod("dbConnect", "ODBCDriver",
   def = function(drv, ...){
      ch <- odbcConnect(...)
      if(ch<0)
         stop("could not open odbc connection")
      new("ODBCConnection", Id = newId(c(drv@Id[1], ch)))
   },
   valueClass = "ODBCConnection"
)
## allow calls of the form dbConnect("RODBC", ...)
setMethod("dbConnect", "character",
   def = function(drv, ...){
      drv <- dbDriver(drv)
      dbConnect(drv, ...)
   },
   valueClass = "ODBCConnection"
)
setMethod("dbSendQuery", 
   sig = signature(conn = "ODBCConnection", statement = "character"),
   def = function(conn, statement, ...){
      ch <- conn@Id[2]
      st <- odbcQuery(ch, statement, ...)
      if(st<0) stop("error in query")
      new("ODBCResult", Id = newId(c(conn@Id[1:2], st)))
   },
   valueClass = "ODBCResult"
)
setMethod("dbGetQuery", 
   sig = signature(conn = "ODBCConnection", statement = "character"),
   def = function(conn, statement, ...){
      ch <- conn@Id[2]
      sqlQuery(ch, query = statement, ...)
   },
   valueClass = "data.frame"
)
setMethod("dbColumnInfo", "ODBCResult",
   def = function(res, ...) .NotYetImplemented(),
   valueClass = "data.frame"
)
setMethod("dbGetInfo", "ODBCConnection",
   def = function(dbObj, ...){
      if(!isIdCurrent(dbObj))
         stop(paste("Expired", class(dbObj), "object"))
      list(dsn = "", uid = "", pwd = "", host = "", case = "", 
           server.version = "NA", results = NA)
   },
   valueClass = "list"
)
setMethod("dbListFields", 
   sig = signature(conn = "ODBCConnection", name = "character"),
   def = function(conn, name, ...) .NotYetImplemented(),
   valueClass = "character"
)
setMethod("dbGetException", "ODBCConnection",
   def = function(conn, ...) {
      ch <- conn@Id[2]
      list(errNum = NA, errMsg = odbcGetErrMsg(ch))
   },
   valueClass = "list"
)
setMethod("dbDisconnect", "ODBCConnection", 
   def = function(conn, ...){
      ch <- conn@Id[2]
      rc <- try(odbcClose(ch))
      if(inherits(rc, "try-error"))
         return(FALSE)
      conn@Id[2] <- -1
      TRUE
   },
   valueClass = "logical"
)
setMethod("dbListTables", "ODBCConnection",
   def = function(conn, ...){
      ch <- conn@Id[2]
      sqlTables(ch)[,3]
   },
   valueClass = "character"
)
setMethod("dbReadTable", 
   sig = signature(conn = "ODBCConnection", name = "character"),
   def = function(conn, name, ...) {
      ch <- conn@Id[2]
      ## had to replace sqlFetch(ch, as.name(name), ...) with the following...
      do.call("sqlFetch", c(list(ch, as.name(name)), list(...)))
   },
   valueClass = "data.frame"
)
setMethod("dbWriteTable", 
   sig=signature(conn="ODBCConnection",name="character",value="data.frame"),
   def = function(conn, name, value, ...) {
      ch <- conn@Id[2]
      rc <- try(do.call("sqlSave", c(list(ch,value,name), list(...))))
      !inherits(rc, "try-error")
   },
   valueClass = "logical"
)
## TODO: do we need to wory about upper/lower case SQL identifiers?
setMethod("dbExistsTable", 
   sig = signature(conn = "ODBCConnection", name = "character"),
   def = function(conn, name, ...) {
      ch <- conn@Id[2]
      match(tolower(name), tolower(sqlTables(ch)[,3]), nomatch=0)>0
   },
   valueClass = "logical"
)
## FIXME: Again, note that invoking sqlDrop(ch, name) inside another 
## function breaks sqlDrop's idea of sqltable being an R "name" 
## (identifier), thus we have to do.call() as a workaround. (I may be
## misinterpreting sqlDrop's usage?)
setMethod("dbRemoveTable", 
   sig = signature(conn="ODBCConnection", name = "character"),
   def = function(conn, name, ...) {
      ch <- conn@Id[2]
      rc <- try(do.call("sqlDrop", c(list(ch, as.name(name)), list(...))))
      !inherits(rc, "try-error")
   },
   valueClass="logical"
)
setMethod("dbListResults", "ODBCConnection",
   def = function(conn, ...) .NotYetImplemented(),
   valueClass = "list"
)
setMethod("dbCallProc", "ODBCConnection", 
   def = function(conn, ...) .NotYetImplemented(),
   valueClass = "logical"
)
setMethod("dbCommit", "ODBCConnection",
   def = function(conn, ...) .NotYetImplemented(),
   valueClass = "logical"
)
setMethod("dbRollback", "ODBCConnection",
   def = function(conn, ...) .NotYetImplemented(),
   valueClass = "logical"
)

##
## Class: ODBCResult
##
## Note that in a production version (i.e., if/when a future version of
## RODBC that doesn't require this DBI front-end) we could define 
## slots "statement", "row.count" (mutable), "row.affected", etc. for 
## this class.  For simplicity, we won't do that here.

setClass("ODBCResult", representation("DBIResult", "ODBCObject"))

setMethod("dbGetInfo", "ODBCResult",
   def = function(dbObj, ...){
      if(!isIdCurrent(dbObj))
         stop(paste("Expired", class(dbObj), "object"))
      list(statement = "", rows.affected = NA, row.count = NA,
           hasCompleted = NA)
   },
   valueClass = "list"
)
setMethod("dbGetStatement", "ODBCResult",
   def = function(res, ...) .NotYetImplemented(),
   valueClass = "character"
)
setMethod("dbHasCompleted", "ODBCResult",
   def = function(res, ...) .NotYetImplemented(),
   valueClass = "logical"
)
setMethod("dbGetException", "ODBCResult",
   def = function(conn, ...){
      ch <- conn@Id[2]
      list(errNum = NA, errMsg = odbcGetErrMsg(ch))
   },
   valueClass = "list"
)
setMethod("fetch", "ODBCResult",
   def = function(res, n = 0, ...) {
      if(n<0) n <- 0
      ch <- res@Id[2]
      sqlGetResults(ch, max = n, ...)
   },
   valueClass = "data.frame"
)
setMethod("dbClearResult", "ODBCResult", 
   def = function(res, ...){
      ## FIXME: how do we clear the result on the server??
      res@Id[3] <- -1
      TRUE
   },
   valueClass = "logical"
)

## Utilities

## RODBC maps *all* objects to varchar(255)
setMethod("dbDataType", "ODBCObject",
   def = function(dbObj, obj, ...) "varchar(255)",
   valueClass = "character"
)
setMethod("make.db.names", "ODBCObject",
   def = function(dbObj, snames, ...) 
      make.db.names.default(snames, keywords = .SQL92Keywords, ...),
   valueClass = "character"
)
setMethod("isSQLKeyword", "ODBCObject",
   def = function(dbObj, name, ...) 
      isSQLKeyword.default(name, keywords = .SQL92Keywords, ...),
   valueClass = "logical"
)
#".conflicts.OK" <- TRUE

".First.lib" <- 
function(lib, pkg) 
{
   library.dynam("DBI.RODBC", pkg, lib)
   require(RODBC, quietly = TRUE, warn.conflicts=FALSE)
   require(DBI, quietly = TRUE, warn.conflicts=FALSE)
}
