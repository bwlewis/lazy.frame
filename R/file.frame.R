`.file.frame_finalizer` = function(x)
{
  invisible(.Call("FREE",x))
}

`file.frame` = function(file, sep=",", gz=regexpr(".gz$",file)>0,
                        skip=0L, header=FALSE, ...)
{
   if(missing(file)) stop("'file' must be specified")
   obj = new.env()
   obj$data=.Call("OPEN",
                  as.character(file),
                  as.integer(gz),
                  PKG="file.frame")
   reg.finalizer(obj$data, .file.frame_finalizer, TRUE)
   obj$call = match.call()
   obj$file = file
   obj$sep = sep
   obj$args = list(...)
   obj$which = NULL
   obj$header = header
   obj$skip = skip
   obj$internalskip = as.integer(skip + header)
   nl = .Call("NUMLINES",obj$data)
   n = min(nl,5)
   f = tempfile()
   b = .Call("RANGE",obj$data,1,as.integer(n),f)
   tmp = .getframe(obj,f,header=header,skip=skip)
   if(any(class(tmp)=="error")) {
     rm(obj)
     stop(tmp)
   }
   if(nrow(tmp)<n){
     obj$header = TRUE
     obj$internalskip = as.integer(skip + header)
   }
   obj$dim = c(nl-obj$internalskip,ncol(tmp))
   obj$dimnames = list(NULL, colnames(tmp))
   class(obj) = "file.frame"
   obj
}

`.getframe` = function(x,f,header=FALSE,skip=0L)
{
  tmp = tryCatch(
    do.call("read.table",
      args=c(list(file=f,sep=x$sep,skip=skip,header=header),x$args)),
      error=function(e) e)
# DEBUG # system2("cat",args=f)
  unlink(f)
  if(!(any(class(tmp)=="error"))) {
    if(ncol(tmp)==length(names(x)))
      names(tmp) = names(x)
  }
  tmp
}

`names.file.frame` = function(x)
{
  dimnames(x)[[2]]
}

`summary.file.frame` = function(x)
{
  warning("Not yet supported")
  invisible()
}

`[<-.file.frame` = function(x, j, k, ..., value)
{
  stop("File frames are read-only.")
}

`[.file.frame` = function(x, j, k, ...)
{
  f = match.call()
  drop=ifelse(is.null(f$drop),TRUE,f$drop)
# This abuse of array subsetting is used for the fast
# single-column 'which' function in comparison operations.
  if(missing(j)) {
    if(missing(k)) stop("Really?")
    if(length(k)>1)
      stop("Selection of all rows limited to single column")
    x$which = as.integer(k)
    return(x)
  }
  if(missing(k))
    k = 1:(x$dim[2])
  if(is.character(k)) k = match(k,names(x))
  if(!is.numeric(k)) stop("Invalid column index")
  if(any(k<1) || any(j<1)) stop("Non-positive indices not supported")
  badk = which(k>(x$dim[2]))
  if(length(badk)>0) k = k[-badk]
  d = dim(x)
  x$which = NULL
# A too-expensive check for sequential indices?
  tmp = c()
  if(sum(diff(j)-1)==0) {
    n  = as.numeric(min(j) + x$internalskip)
    m  = as.integer(max(j) + x$internalskip - n + 1)
    w = tempfile()
    b = .Call("RANGE",x$data,n,m,w)
    tmp = .getframe(x,w)
  } else {
    j = j + x$internalskip
    badj = which(j>nrow(x))
    if(length(badj)>0) j = j[-badj]
    w = tempfile()
    b = .Call("LINES",x$data,as.numeric(j),w)
    tmp = .getframe(x,w)
  }
  if(any(class(tmp)=="error")) stop(tmp)
  tmp[,k,drop=drop]
}

Ops.file.frame = function(e1,e2) {
  col = e1$which
  e1$which = NULL
  OP <- switch(.Generic,"=="=1L,
                         "!="=2L,
                         ">="=3L,
                         "<="=4L,
                         ">"= 5L,
                         "<"= 6L)
  if(!inherits(e1,"file.frame")) stop("Left-hand side must be file.frame object")
  if(!is.numeric(e2)) stop("Sorry, right-hand side must be numeric only")
  if(is.null(col)) stop("Can only compare a single column")
  .Call("WHICH",e1$data,
                as.integer(col),
                as.integer(e1$internalskip),
                as.character(e1$sep),
                OP,
                as.numeric(e2))
}

`dim.file.frame` = function(x)
{
  x$dim
}

`dim<-.file.frame` = function(x, value)
{
  x$dim <- value
  x
}

`dimnames.file.frame` = function(x)
{
  x$dimnames
}

`dimnames<-.file.frame` = function(x, value)
{
  x$dimnames = value
  x
}

`names<-.file.frame` = function(x,value)
{
  x$dimnames[[2]] = make.names(value[1:(x$dim[2])])
  x
}

`head.file.frame` = function(x, n=6L, ...)
{
  if(is.null(x$which)) return(x[1:min(nrow(x),n),])
  cat("Note: If you really want this whole column as a vector, supply the row indices too.\n")
  x[1:min(nrow(x),n),x$which,drop=FALSE]
}

`tail.file.frame` = function(x, n=6L, ...)
{
  x[max((nrow(x)-n),1):nrow(x),]
}

`str.file.frame` = function(object, ...)
{
  ls.str(object)
}

`print.file.frame` = function(x, ...)
{
  cat("\nLazy person's file-backed data frame for",x$file,"\n\n")
  print(head(x))
  j = x$dim[1]-6
  if(j>2) cat("and (",j,"more rows not displayed...)\n")
}

`ncol.file.frame` = function(x) x$dim[2]
`nrow.file.frame` = function(x) x$dim[1]
`dim.file.frame` = function(x) x$dim
