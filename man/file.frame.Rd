\name{file.frame}
\alias{file.frame}
\title{File Frames: A lazy person's file-backed data frame.}
\description{
File frames are very simple relatives of data frames backed by text files.
Text is lazily parsed and converted a data frame by \code{read.table}
only when required. File frames are useful for quickly subsetting large
csv and other text files without loading the entire file.
}
\usage{
file.frame(file, sep = ",", gz = regexpr(".gz$", file) > 0, skip = 0L, header = FALSE, ...)
}
\arguments{
  \item{file}{ A gzip-compressed or uncompressed text file.}
  \item{sep}{The column delimiter character.}
  \item{gz}{TRUE indicates gzip-compressed file, FALSE an uncompressed file.}
  \item{skip}{
       Number of lines to skip at the top of the file (see \code{read.table}).
  }
  \item{header}{TRUE if the first line of the file should be read as
       column names, FALSE otherwise (see \code{read.table}).
  }
  \item{\dots}{Other arguments are passed directly to \code{read.table}. }
}
\details{
File frames express raw text files as data frames, lazily invoking
\code{read.table} as required. Because the file contents are not loaded
until accessed, file frames are a fast and memory-efficient way to
extract subsets from medium to large text files (for example with
tens of millions of rows).

File frames are read only. They support gzip-compressed and uncompressed text
files, but presently only with Unix-like (newline only) line termination. A
future version may support newline + carriage return termination. Maybe.

Indexing operations generally follow standard array indexing with a few
exceptions:
\enumerate{
  \item Only positive indices are allowed.
  \item A missing row index requires specification of a single column, for
        use in some basic comparison operations discussed below.
  \item File frames don't yet support the dollar sign column selector.
}
Otherwise, specify row and column indices like those for normal data.frames.

Because file frames lazily load data, the default setting of
\code{stringsAsFactors} is \code{FALSE} (see help for \code{data.frame} for
more information).

File frames provide a few very basic numeric comparison operations that
work on single columns and act like the \code{which} function. Presently
supported operations are ==, !=, <=, >=, <, and >, and may only compare
a single column with a scalar numeric value. See below for an example.

}
\value{
A file.frame object is returned.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
\note{
I often just need to quickly filter row subsets or sample out of large files.
File frames are intended to do that quickly and efficiently.

File frames can in pricinple index data files with more than 2^31 rows
(returned subsets must conform to R's indexing limits of course).
However, the indexing scheme used needs efficiency improvement to make
handling of such large text files practical. A future version may improve
this. The present version is well-suited to text files with millions to
hundreds of millions of rows. If your files are smaller, don't bother
using this package--just read them in!

This package was inspired by the \code{mmap} and \code{bigmemory} packages.
}

\examples{
data(iris)
f = tempfile()
write.table(iris, file=f, sep=",",col.names=TRUE,row.names=FALSE)
x = file.frame(f, header=TRUE)

# Subsetting
print(x[c(5,15,25),])

# Quickly apply basic numeric comparisons to a column
print(x[x[,1]<4.5,])

rm(f)
}
