\name{lazy.frame-package}
\alias{lazy.frame-package}
\title{File Frames: A lazy person's file-backed data frame.}
\description{
Lazy frames are very simple relatives of data frames backed by text files.
Text is parsed and converted to data frame by read.table on demand.  Lazy
frames are useful for quickly and efficiently extracting subsets from large csv
and other text files without loading the entire file. They support normal and
gzip-compressed files.
}
