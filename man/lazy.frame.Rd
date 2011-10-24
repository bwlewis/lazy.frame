\name{lazy.frame}
\alias{lazy.frame}
\title{A file-backed data frame promise.}
\description{
A \code{lazy.frame} is a data frame promise. It presents a delimited text file
as a kind of simple read-only data frame, without initially loading the file
into memory.  Lazy frames load data from their backing files on demand.  Lazy
frames are useful for quickly and efficiently extracting subsets from large csv
and other text files.  They support normal and gzip-compressed files.
}
\usage{
lazy.frame(file, sep=",", gz=regexpr(".gz$",file)>0, skip=0L, stringsAsFactors=FALSE, header=FALSE, ...)
}
\arguments{
  \item{file}{ A gzip-compressed or uncompressed text file.}
  \item{sep}{The column delimiter character.}
  \item{gz}{TRUE indicates gzip-compressed file, FALSE an uncompressed file.}
  \item{skip}{
       Number of lines to skip at the top of the file (see \code{read.table}).
  }
  \item{stringsAsFactors}{Strings cannot automatically be converted to factor
    variables by lazy.frame since data is loaded on demand. If you specify
    TRUE, you must also explicitly set the column factor levels manually.
  }
  \item{header}{TRUE if the first line of the file should be read as
       column names, FALSE otherwise (see \code{read.table}).
  }
  \item{\dots}{Other arguments are passed directly to \code{read.table}. }
}
\details{
Lazy frames express raw text files as data frames, invoking
\code{read.table} as required. Because the file contents are not loaded
until accessed, lazy.frames are a fast and memory efficient way to
extract subsets from medium to large text files (for example with
tens of millions of rows).

Lazy frames are read only. They support gzip-compressed and uncompressed text
files.

Indexing operations generally follow standard array indexing with a few
exceptions:
\enumerate{
  \item Only positive indices are allowed.
  \item A missing row index requires specification of a single column for
        use in some basic comparison operations discussed below.
  \item Lazy frames don't yet support the dollar sign column selector.
}
Otherwise, specify row and column indices like those for normal data.frames.

Because lazy.frames load data on demand, the default setting of
\code{stringsAsFactors} is \code{FALSE} (see help for \code{data.frame} for
more information). See the \code{\link{column_attr}} function for an example
of working with factors.

Lazy frames provide a few very basic comparison operations that work quickly on
single columns and act like the \code{which} function. Presently supported
operations are ==, !=, <=, >=, <, and >, and may only compare a single column
with a scalar numeric  or integer value, or with a character string. See below
for an example.

}
\value{
A lazy.frame object is returned.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\note{
I often just need to quickly filter row subsets or sample out of large files.
Lazy frames are intended to do that quickly and efficiently.

Lazy frames can in pricinple index data files with more than 2^31 rows
(returned subsets must conform to R's indexing limits of course).
However, the internal indexing scheme needs efficiency improvement to make
handling of such large text files practical. A future version may improve
this. The present version is well-suited to text files with millions to
hundreds of millions of rows.

This package was inspired by the \code{mmap} and \code{bigmemory} packages.
}

\examples{
data(iris)
f = tempfile()
write.table(iris, file=f, sep=",",col.names=TRUE,row.names=FALSE)
x = lazy.frame(f, header=TRUE)

# Subsetting
print(x[c(5,15,25),])

# Quickly apply basic numeric comparisons to a column
print(x[x[,1]<4.5,])

# Basic string and integer comparisons work too. Note that they are faster
# than numeric or integer comparisons.
v = x[x[,5]=="versicolor"]
print(dim(v))

unlink(f)
}
