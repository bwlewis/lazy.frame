\name{file.frame-package}
\alias{file.frame-package}
\title{File Frames: A lazy person's file-backed data frame.}
\description{
File frames are very simple relatives of data frames backed by text files.
Text is lazily parsed and converted a data frame by \code{read.table}
only when required. File frames are useful for quickly subsetting large
csv and other text files without loading the entire file.
}
