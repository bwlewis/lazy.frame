#define _GNU_SOURCE
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <locale.h>
#ifndef WIN32
#include <zlib.h>
#endif

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

#define BUFSZ 16384		/* stack-allocated buffers */
#define IDXSZ 1024		/* newline index buffer base size */

/* The exp10 function in GNU's math lib is apparently not widely available. */
double
port_exp10 (double arg)
{
  return exp (2.30258509299404568402 * arg);
}

typedef struct
{
  FILE *f;			/* FILE pointer */
  size_t *nl;			/* Byte position of newlines */
  size_t n;			/* Number of lines */
/* file operators */
  int (*close) (FILE * f);
  int (*seek) (FILE * f, long offset, int whence);
  void (*rewind) (FILE * f);
    size_t (*read) (void *ptr, size_t size, size_t nmemb, FILE * f);
  int (*eof) (FILE * f);
} fmeta;

#ifndef WIN32
/* gz wrapper functions */
int
z_close (FILE * f)
{
  return gzclose ((gzFile) f);
}

int
z_seek (FILE * f, long offset, int whence)
{
  return (int) gzseek ((gzFile) f, (z_off_t) offset, whence);
}

void
z_rewind (FILE * f)
{
  gzrewind ((gzFile) f);
}

size_t
z_read (void *ptr, size_t size, size_t nmemb, FILE * f)
{
  return (size_t) gzread ((gzFile) f, ptr, (unsigned) (size * nmemb));
}

int
z_eof (FILE * f)
{
  return gzeof ((gzFile) f);
}
#endif


void numlines (fmeta *);

SEXP
FREE (SEXP M)
{
  fmeta *addr = (fmeta *) R_ExternalPtrAddr (M);
  if (addr)
    {
      if (addr->f)
	addr->close (addr->f);
      if (addr->nl)
	free (addr->nl);
      free (addr);
    }
  return R_NilValue;
}

SEXP
OPEN (SEXP F, SEXP GZ)
{
  FILE *f;
  int gz = INTEGER (GZ)[0];
  const char *fname = CHAR (STRING_ELT (F, 0));
  fmeta *fm = (fmeta *) malloc (sizeof (fmeta));
  if (gz)
    {
#ifdef WIN32
      error ("Compressed files not supported on Windows, sorry!");
#else
      f = (FILE *) gzopen (fname, "r");
      fm->f = f;
      fm->close = &z_close;
      fm->seek = &z_seek;
      fm->rewind = &z_rewind;
      fm->read = &z_read;
      fm->eof = &z_eof;
#endif
    }
  else
    {
      f = fopen (fname, "r");
      fm->f = f;
      fm->close = &fclose;
      fm->seek = &fseek;
      fm->rewind = &rewind;
      fm->read = &fread;
      fm->eof = &feof;
    }
  numlines (fm);
  return R_MakeExternalPtr ((void *) fm, R_NilValue, R_NilValue);
}

SEXP
FOO (SEXP F)
{
  size_t j;
  fmeta *addr = (fmeta *) R_ExternalPtrAddr (F);
  SEXP ans = allocVector (REALSXP, addr->n);
  for (j = 0; j < addr->n; ++j)
    REAL (ans)[j] = (double) addr->nl[j];
  return ans;
}

SEXP
NUMLINES (SEXP F)
{
  fmeta *addr = (fmeta *) R_ExternalPtrAddr (F);
  return ScalarReal ((double) addr->n);
}

// XXX Here and below switch to R memory allocators to
// allow user interrupt.
/* Assumes start base row index 1 */
SEXP
RANGE (SEXP F, SEXP START, SEXP N, SEXP OUT)
{
  char *buf;
  size_t m;
  const char *fname = CHAR (STRING_ELT (OUT, 0));
  FILE *out = fopen (fname, "w+");
  fmeta *fm = (fmeta *) R_ExternalPtrAddr (F);
  size_t start = (size_t) REAL (START)[0] - 1;
  int n = INTEGER (N)[0];
  if (!out)
    error ("Invalid output file");
// XXX more bounds check
  if (start > fm->n)
    error ("Invalid range");
  if (start + n > fm->n)
    n = fm->n - start - 1;
  size_t pos = fm->nl[start];
  size_t end = fm->nl[start + n];
  fm->seek (fm->f, pos, SEEK_SET);
  buf = (char *) calloc (1, 1 + end - pos);
  m = fm->read (buf, 1, end - pos, fm->f);
  fprintf (out, "%s", buf);
  free (buf);
  fprintf (out, "\n");
  fclose (out);
  return ScalarReal ((double) m);
}

/* Assumes base row index 1 */
SEXP
LINES (SEXP F, SEXP IDX, SEXP OUT)
{
  char buf[BUFSZ];
  size_t m, p, q;
  int k, j;
  const char *fname = CHAR (STRING_ELT (OUT, 0));
  FILE *out = fopen (fname, "w+");
  fmeta *fm = (fmeta *) R_ExternalPtrAddr (F);
  int n = LENGTH (IDX);
  if (!out)
    error ("Invalid output file");
// XXX bounds check XXX !!!
  for (j = 0; j < n; ++j)
    {
      memset (buf, 0, BUFSZ);
      m = (size_t) (REAL (IDX)[j]) - 1;
      p = fm->nl[m];
      q = fm->nl[m + 1];
      k = (int) (q - p);
      fm->seek (fm->f, p, SEEK_SET);
      m = fm->read (buf, 1, k, fm->f);
      fprintf (out, "%s\n", buf);
    }
  fclose (out);
  return ScalarInteger (n);
}

/* XXX This function stores the position of each newline
 * in memory. If the data set is more than 100m lines or
 * so, this storage is excessive. Switch to either:
 * 1) storing a subset of newlines with a regular stride
 * 2) memory mapping the newlines to an index file
 */
void
numlines (fmeta * fm)
{
  char BUF[BUFSZ];
  char *s;
  void *addr;
  size_t *nl;
  FILE *f;
  int d, j, k;
  size_t p, q, l, m, n = 0;
  f = fm->f;
  p = IDXSZ;
  nl = (size_t *) malloc (p * sizeof (size_t));
  fm->rewind (f);
  k = (fm->eof (f) != 0);	// XXX should check file error too but interface
  // differs between zlib and stdlib
  q = 0;
  nl[0] = 0;
  while (k == 0)
    {
      m = fm->read (BUF, 1, BUFSZ, f);
      if (m < 1)
	break;
      s = memchr (BUF, 10, m);	// XXX Add newline character argument
      while (s != 0)
	{
	  d = (int) (s - BUF);
	  nl[n + 1] = q * BUFSZ + d;
	  n++;
	  if ((n + 1) > p)
	    {
	      p = p + IDXSZ;
	      addr = realloc (nl, (p * sizeof (size_t)));
	      if (addr)
		nl = addr;
// XXX else error...
	    }
	  l = m - d;
	  s++;
	  s = memchr (s, 10, l);
	}
      q++;
      k = (fm->eof (f) != 0);
    }
  fm->n = n;
  fm->nl = nl;
}

inline char *
get_col_val (char *buf, const char *delim, int col)
{
  int j = 0;
  char *s = strtok (buf, delim);
  while (s != 0)
    {
      j++;
      if (j == col)
	break;
      s = strtok (NULL, delim);
    }
  return s;
}

int
compare (double x, double y, int op)
{
/* comparisons: * 1 ==;  2 !=; 3 >=; 4 <=;5 >; 6 < */
  int k = 0;
  switch (op)
    {
    case 1:
      k = (x == y);
      break;
    case 2:
      k = (x != y);
      break;
    case 3:
      k = (x >= y);
      break;
    case 4:
      k = (x <= y);
      break;
    case 5:
      k = (x > y);
      break;
    case 6:
      k = (x < y);
      break;
    default:
      break;
    }
  return k;
}

int
compare_int (int x, int y, int op)
{
/* comparisons: * 1 ==;  2 !=; 3 >=; 4 <=;5 >; 6 < */
  int k = 0;
  switch (op)
    {
    case 1:
      k = (x == y);
      break;
    case 2:
      k = (x != y);
      break;
    case 3:
      k = (x >= y);
      break;
    case 4:
      k = (x <= y);
      break;
    case 5:
      k = (x > y);
      break;
    case 6:
      k = (x < y);
      break;
    default:
      break;
    }
  return k;
}

int
compare_str (char *s1, const char *s2, int op)
{
/* comparisons: * 1 ==;  2 !=; 3 >=; 4 <=;5 >; 6 < */
  int k = strcmp (s2, s1);
  switch (op)
    {
    case 1:
      k = (k == 0);
      break;
    case 2:
      k = (k != 0);
      break;
    case 3:
      k = (k >= 0);
      break;
    case 4:
      k = (k <= 0);
      break;
    case 5:
      k = (k > 0);
      break;
    case 6:
      k = (k < 0);
      break;
    default:
      break;
    }
  return k;
}

/* Cheaper strtod:  It's based on a nice comparison of string to numeric
 * conversions by Tino Didriksen (http://tinodidriksen.com), modified to
 * fall back to strtod fo trickier conversions.
 */
double
cheap_strtod (char *p, char decimal)
{
  char *check, *t = p;
  double x = 0.0;
  int neg = 0;
  while (*p == ' ')
    ++p;
  if (*p != '-' && *p < '0' && *p > '9')
    return NAN;
  if (*p == '-')
    {
      neg = 1;
      ++p;
    }
  while (*p >= '0' && *p <= '9')
    {
      x = (x * 10.0) + (*p - '0');
      ++p;
    }
  if (*p == 'e' || *p == 'E')
    {
      x = strtod (t, &check);
      if ((*check != 0) && (!isspace ((unsigned char) *check)))
	x = NAN;
      return x;
    }
  if (*p == decimal)
    {
      double f = 0.0;
      int n = 0;
      ++p;
      while (*p >= '0' && *p <= '9')
	{
	  f = (f * 10.0) + (*p - '0');
	  ++p;
	  ++n;
	}
      x += f / port_exp10 (n);
    }
  if (*p == 'e' || *p == 'E')
    {
      x = strtod (t, &check);
      if ((*check != 0) && (!isspace ((unsigned char) *check)))
	x = NAN;
      return x;
    }
  if (neg)
    return (-x);
  return x;
}

/* A very limited 'which'-like numeric-only single column filter.
 * Believe it or not, I started with a version that read the file
 * in blocks instead of line by line (it's in the project git
 * history). That version is **slower** than this one that runs
 * through line by line, at least with gzipped files (often used
 * for really big files). However, seek is more of a bottleneck
 * for uncompressed files, and that version may be faster in that
 * case. Certainly more optimization can be found here...
 * 
 * The strtod conversion is a big bottleneck. Our cheaper version
 * saves a little bit of time. Perhaps more speed could be gained.
 *
 */
// XXX Switch to R memory allocators to allow user interrupt.
// XXX add checks for memory allocation failures, read failures, etc.
SEXP
WHICH (SEXP F, SEXP COL, SEXP ROWNAMES, SEXP SKIP, SEXP SEP, SEXP OP,
       SEXP VAL)
{
  char buf[BUFSZ];
  char *s, *r, *t;
  size_t j, p, q;
  double x;
  int k, ix;
  struct lconv *fmt = localeconv ();
  char decimal = *(fmt->decimal_point);
  SEXP ans = R_NilValue;
  fmeta *fm = (fmeta *) R_ExternalPtrAddr (F);
  const char *delim = CHAR (STRING_ELT (SEP, 0));
  int col = INTEGER (COL)[0];
  int rownames = INTEGER (ROWNAMES)[0];
  int skip = INTEGER (SKIP)[0];
  int op = INTEGER (OP)[0];
  int setsz = IDXSZ;
  size_t *set = (size_t *) malloc (setsz * sizeof (size_t));
  int n = 0;
  if (rownames > 0)
    if (col >= rownames)
      col++;
  j = skip;
  while (j < fm->n - 1)
    {
      memset (buf, 0, BUFSZ);
      p = fm->nl[j];
      q = fm->nl[j + 1];
      fm->seek (fm->f, p + (j > 0), SEEK_SET);
      p = fm->read (buf, 1, q - p, fm->f);
      s = get_col_val (buf, delim, col);
      if (!s)
	break;
      k = 0;
      switch (TYPEOF (VAL))
	{
	case INTSXP:
	  {
	    ix = atoi (s);
	    k = compare_int (ix, INTEGER (VAL)[0], op);
	    break;
	  }
	case STRSXP:
	  {
// XXX strip nl cr better way?
	    r = strchr (s, 13);
	    if (r)
	      *r = 0;
	    r = strchr (s, 10);
	    if (r)
	      *r = 0;
// XXX strip quotes and leading/trailing space before or after quotes.
// XXX but leave escaped quotes in there...ugh.
	    r = strchr (s, '"');
	    if (r == s)
	      s = r + 1;
	    if (r && r > s)
	      {
		t = r - 1;
		if (*t != 92)
		  s = r + 1;
	      }
	    r = strrchr (s, '"');
	    if (r && r > s)
	      {
		t = r - 1;
		if (*t != 92)
		  *r = 0;
	      }
	    k = compare_str (s, CHAR (STRING_ELT (VAL, 0)), op);
	    break;
	  }
	case REALSXP:
	  {
	    x = cheap_strtod (s, decimal);
	    k = compare (x, REAL (VAL)[0], op);
	    break;
	  }
	default:
	  break;
	}
      if (k)
	{
	  set[n] = j - skip;
	  n++;
	  if (n < 0)
	    {
	      warning
		("Too many matching elements--only first 2147483647 returned.");
	      n = 2147483647;
	      break;
	    }
	  if (n >= setsz)
	    {
	      setsz = setsz + IDXSZ;
	      set = (size_t *) realloc (set, setsz * sizeof (size_t));
	    }
	}
      j++;
    }
  if (n < 1)
    {
      free (set);
      ans = allocVector (REALSXP, 0);
      return ans;
    }
  PROTECT (ans = allocVector (REALSXP, n));
  for (j = 0; j < n; ++j)
    REAL (ans)[j] = (double) set[j] + 1;
  free (set);
  UNPROTECT (1);
  return ans;
}
