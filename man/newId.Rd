\name{newId}
\alias{newId}
\alias{isIdCurrent}
\title{Help (``mixin'') Id class and methods}
\description{
  The \code{Id} helper class provides simple mutable objects
  to track remote references Id.  The only use for this class
  and its methods is in the \emph{temporary} DBI front-end to RODBC
  and RPgSQL.
}
\usage{
isIdCurrent(obj)
newId(.Id = -1)
}
\arguments{
  \item{obj}{an object that inherits from class \code{Id}.}
  \item{.Id}{the initial \code{Id} value.}
}
\details{
  The class \code{Id} is used for holding remote (foreign) references
  to data managed outside of R/S, typically by a C library.
  The remote object may be a piece of memory and the Id object
  may hold a pointer into memory, but it can also refer to 
  connection ids.  These references to remote objects are
  transient and should not be used across R/S sessions.
}
\value{
  \code{newId} returns an object of class \code{Id} initialized to 
  \code{.Id}.

  \code{isIdCurrent} returns a logical specifying whether the 
  reference held by object \code{obj} is still valid or whether 
  it has expired.
}

\note{Need to check how this relates to Luke Tierney's external references.}

\seealso{
  \code{\link{RODBC}}
}
\examples{\dontrun{
   isIdCurrent(m)
}
}
\keyword{interface} 
\keyword{database}

% vim: syntax=tex
