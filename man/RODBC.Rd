\name{RODBC}
\alias{RODBC}
\title{Instantiate an R-ODBC database driver}
\description{
  Instantiates an R-ODBC database driver according
  to the common database interface (DBI).
}
\usage{
RODBC()
}
\details{
  The current implementation simply creates an object
  of class \code{ODBCDriver} and attaches the
  existing (non Version 4) package \code{RODBC}.
}
\value{
  An object of class \code{ODBCDriver} that can be used
  to establish one or more simultaneous channels (connections) to one
  or more remote databases.
}

\note{This current version is simply a front-end in order to implement
  the common DBI on top of the existing (version 0.8-3) RODBC 
  package.  
}

\seealso{
  \link[DBI]{\code{dbDriver}},
  \link[DBI]{\code{dbConnect}},
  \link[DBI]{\code{dbSendQuery}},
  \link[DBI]{\code{dbListTables}},
  \link[RODBC]{\code{odbcConnect}}.
}

\examples{\dontrun{
drv <- dbDriver("RODBC")
con1 <- dbConnect(drv, "dsn", "uid", "pwd")
con2 <- dbConnect(drv, "dsn2", "uid2", "pwd2")
}
}
\keyword{interface}
\keyword{database} 

% vim: syntax=tex
