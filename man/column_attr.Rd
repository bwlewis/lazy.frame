\name{column_attr}
\alias{column_attr}
\alias{column_attr<-}
\title{
Get or set a lazy frame column attribute.
}
\description{
Get or set a lazy frame column attribute.
}
\usage{
column_attr(x, col, which)
column_attr(x, col, which) <- value
}
\arguments{
  \item{x}{A lazy.frame object.}
  \item{col}{A numeric column index.}
  \item{which}{The attribute name to set or get.}
  \item{value}{The new attribute value.}
}
\details{
Because lazy frames load data from text files on demand,
column attributes set in the usual way are ephemeral--they
only affect data frame subsets returned as a result of
index operations.

Use the column_attr function to set attributes on a
lazy frame column that persist for all extracted subsets.
}
\value{
The attribute value is returned by the get version
of the function.
}
\author{ B. W. Lewis <blewis@illposed.net> }
\note{
This function is typically used to manually set factor levels
for factor-valued columns, for example if
\code{stringsAsFactors=TRUE}. See below for an example.
}

\seealso{
\code{\link{lazy.frame}}
}
\examples{
# Note that the 5th column of the 'iris' data set is factor coded.
data(iris)
f = tempfile()
write.table(iris, file=f, sep=",", row.names=FALSE, col.names=TRUE)

x = lazy.frame(f, header=TRUE, stringsAsFactors=TRUE)
# Lazy frames load data on demand from the file, they can't automatically
# code factor levels. Subsets will only include factor level codings
# defined within the subset (probably not what you want!), for example:
x[1:3,5]

# If we know all the levels, however, we can explicitly set them with
# the column_attr function:
column_attr(x,5,"levels") <- levels(iris[,5])

# Now we're OK:
x[1:3,5]

unlink(f)

}
