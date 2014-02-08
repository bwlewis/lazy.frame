#define _GNU_SOURCE
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <locale.h>
#ifndef WIN32
#include <zlib.h>
#include <pthread.h>
#endif

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

#define MIN(a,b) (((a)<(b))?(a):(b))

#define BUFSZ 16384             /* stack-allocated buffers */
#define IDXSZ 16384             /* newline index buffer base size */

/* The exp10 function in GNU's math lib is apparently not widely available. */
double
port_exp10 (double arg)
{
  return exp (2.30258509299404568402 * arg);
}

typedef struct
{
  FILE *f;                      /* FILE pointer */
  char path[BUFSZ];             /* file path */
  size_t *nl;                   /* Byte position of newlines */
  int n;                        /* Number of lines, limited to int for now */
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
REOPEN (SEXP M, SEXP GZ)
{
  FILE *f;
  fmeta *fm = (fmeta *) R_ExternalPtrAddr (M);
  int gz = INTEGER (GZ)[0];
  if (!fm)
    {
      error ("Invalid external pointer");
    }
  const char *fname = fm->path;
  if (gz)
    {
#ifdef WIN32
      error ("Compressed files not supported on Windows, sorry!");
#else
      f = (FILE *) gzopen (fname, "r");
      if (!f)
        {
          FREE (M);
          error ("Invalid file");
        }
      fm->f = f;
#endif
    }
  else
    {
      f = fopen (fname, "rb");
      if (!f)
        {
          FREE (M);
          error ("Invalid file");
        }
      fm->f = f;
    }
  return M;
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
      if (!f)
        {
          free (fm);
          error ("Invalid file");
        }
      fm->f = f;
      strncpy (fm->path, fname, BUFSZ);
      fm->close = &z_close;
      fm->seek = &z_seek;
      fm->rewind = &z_rewind;
      fm->read = &z_read;
      fm->eof = &z_eof;
#endif
    }
  else
    {
      f = fopen (fname, "rb");
      if (!f)
        {
          free (fm);
          error ("Invalid file");
        }
      fm->f = f;
      strncpy (fm->path, fname, BUFSZ);
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
NUMLINES (SEXP F)
{
  fmeta *addr = (fmeta *) R_ExternalPtrAddr (F);
  return ScalarInteger ((int) addr->n);
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
  int start = (int) INTEGER (START)[0] - 1;
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
  m = fm->read (buf, sizeof (char), end - pos, fm->f);
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
  size_t p, q;
  int k, j, m;
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
      m = (int) (INTEGER (IDX)[j]) - 1;
      p = fm->nl[m];
      q = fm->nl[m + 1];
      k = (int) (q - p);
      fm->seek (fm->f, p, SEEK_SET);
      m = fm->read (buf, sizeof (char), k, fm->f);
      fprintf (out, "%s\n", buf);
    }
  fclose (out);
  return ScalarInteger (n);
}

/* This function stores the position of each newline in memory. If the data set
 * is more than 100m lines or so, this storage is excessive. Switch to either:
 * 1) storing a subset of newlines with a regular stride; 2) memory mapping the
 * newlines to an index file; 3) some other, better way!
 */
void
numlines (fmeta * fm)
{
  char BUF[BUFSZ];
  char *s;
  void *addr;
  size_t *nl;
  FILE *f;
  int d, k;
  size_t p, q, l, m, n = 0;
  f = fm->f;
  p = IDXSZ;
  nl = (size_t *) malloc (p * sizeof (size_t));
  fm->rewind (f);
  k = (fm->eof (f) != 0);       // XXX should check file error too but interface
  // differs between zlib and stdlib
  q = 0;
  nl[0] = 0;
  while (k == 0)
    {
      m = fm->read (BUF, sizeof (char), BUFSZ, f);
      if (m < 1)
        break;
      s = memchr (BUF, 10, m);
      while (s != 0)
        {
          d = (int) (s - BUF);
          nl[n + 1] = q * BUFSZ + d;
          n++;
// XXX put a bounds check on n here
          if ((n + 1) > p)
            {
              p = p + IDXSZ;
              addr = realloc (nl, (p * sizeof (size_t)));
              if (addr)
                nl = addr;
// XXX else OOM error...
            }
          l = m - d;
          s = memchr (++s, 10, l);
        }
      q++;
      k = (fm->eof (f) != 0);
    }
  fm->n = n;
  fm->nl = nl;
}

inline char *
get_col_val (char *buf, const char *delim, int col, char **saveptr)
{
  int j = 0;
  char *s = strtok_r (buf, delim, saveptr);
  while (s != 0)
    {
      j++;
      if (j == col)
        break;
      s = strtok_r (NULL, delim, saveptr);
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

inline int
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


/* This crude function replaces cr and nl with 0 (terminating the string),
 * and strips quotes and leading/trailing space outside quote symbols (if
 * any)--but leaves escaped quotes in. Ugh, this can be done better!
 * Note: The function modifies it's argument!
 */
char *
strip_nl_and_dequote (char *s)
{
  char *r, *t;
  r = strchr (s, 13);
  if (r)
    *r = 0;
  r = strchr (s, 10);
  if (r)
    *r = 0;
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
  return s;
}

#ifdef WIN32
/* A very limited 'which'-like  single column filter.
 * Certainly more optimization can be found here...
 * 
 * The strtod conversion is a big bottleneck. Our cheaper version
 * saves a little bit of time. Perhaps more speed could be gained.
 *
 */
SEXP
WHICH (SEXP F, SEXP COL, SEXP ROWNAMES, SEXP SKIP, SEXP SEP, SEXP OP,
       SEXP VAL, SEXP NP)
{
  char buf[BUFSZ];
  char *s, *saveptr;
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
  while (j < fm->n)
    {
      memset (buf, 0, BUFSZ);
      p = fm->nl[j];
      q = fm->nl[j + 1];
      fm->seek (fm->f, p + (j > 0), SEEK_SET);
      p = fm->read (buf, sizeof (char), q - p, fm->f);
      s = get_col_val (buf, delim, col, &saveptr);
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
            s = strip_nl_and_dequote (s);
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

#else

typedef struct
{
  fmeta *fm;
  int col;
  const char *delim;
  int skip;
  int op;
  void *val;
  int valtype;                  // 0:int, 1:double, 2:char
  int start;
  int end;
// return values:
  int *retval;
  int nret;
} pargs;

void *twhich (void *);

/* Experimental parallel version of which, useful for data that fits in
 * cache. POSIX only!
 */
SEXP
WHICH (SEXP F, SEXP COL, SEXP ROWNAMES, SEXP SKIP, SEXP SEP, SEXP OP,
       SEXP VAL, SEXP NP)
{
  int k, j, h, n;
  pargs *args;
  pthread_t *readers;
  SEXP ans = R_NilValue;
  int valtype = -1;
  void *val = 0;
  int rownames = INTEGER (ROWNAMES)[0];
  int col = INTEGER (COL)[0];
  int np = INTEGER (NP)[0];
  if (rownames > 0)
    if (col >= rownames)
      col++;
  switch (TYPEOF (VAL))
    {
    case INTSXP:
      valtype = 0;
      val = (void *) (INTEGER (VAL));
      break;
    case REALSXP:
      valtype = 1;
      val = (void *) (REAL (VAL));
      break;
    case STRSXP:
      valtype = 2;
      val = (void *) CHAR (STRING_ELT (VAL, 0));
//printf("VAL=%s val=%s\n",CHAR(STRING_ELT(VAL,0)),(char *)val);
//printf("address of val=%p\n",val);
      break;
    default:
      break;
    }
  n = (int) ((fmeta *) R_ExternalPtrAddr (F))->n;
  h = n / np;
  args = (pargs *) malloc (np * sizeof (pargs));
  readers = (pthread_t *) malloc (np * sizeof (pthread_t));
  for (j = 0; j < np; ++j)
    {
      args[j].fm = (fmeta *) R_ExternalPtrAddr (F);
      args[j].delim = CHAR (STRING_ELT (SEP, 0));
      args[j].col = INTEGER (COL)[0];
      args[j].skip = INTEGER (SKIP)[0];
      args[j].op = INTEGER (OP)[0];
      args[j].col = col;
      args[j].valtype = valtype;
      args[j].val = val;
      args[j].start = j * h + (j > 0);
      args[j].end = MIN ((j + 1) * h + 1, n);
//printf("start=%d end=%d\n",args[j].start, args[j].end);
      pthread_create (&readers[j], NULL, twhich, (void *) &args[j]);
    }
  n = 0;
  for (j = 0; j < np; ++j)
    {
      pthread_join (readers[j], NULL);
//printf("args[%d].nret=%d\n",j,args[j].nret);
      n += args[j].nret;
    }
  PROTECT (ans = allocVector (REALSXP, n));
  n = 0;
  for (j = 0; j < np; ++j)
    {
      for (k = 0; k < args[j].nret; ++k)
        {
          REAL (ans)[n] = (double) args[j].retval[k] + 1;
          ++n;
        }
      free (args[j].retval);
    }
  free (readers);
  free (args);
  UNPROTECT (1);
  return ans;
}

// col, start, end are assumed to already be adjusted for skip, rownames, etc.
// designed for use by threads
void *
twhich (void *args)
{
  pargs *a = (pargs *) args;
  char buf[BUFSZ];
  FILE *f;
  char *saveptr, *s;
  int j, k;
  ssize_t p, q;
  int setsz = IDXSZ;
  int n = 0;
  int *set = (int *) malloc (setsz * sizeof (int));
  struct lconv *fmt = localeconv ();
  char decimal = *(fmt->decimal_point);
//printf("path=%s\n",a->fm->path);
  f = fopen (a->fm->path, "rb");
//printf("twhich\n");
//printf("start %d end %d\n",a->start, a->end);
  for (j = a->start; j < a->end; ++j)
    {
      memset (buf, 0, BUFSZ);
      p = a->fm->nl[j];
      q = a->fm->nl[j + 1];
//printf("p=%ld q=%ld col=%d delim=%s op=%d\n",p,q,a->col,a->delim,a->op);
      fseek (f, p + (j > 0), SEEK_SET);
      p = fread (buf, sizeof (char), q - p, f);
//printf("buf=%s\n",buf);
      s = get_col_val (buf, a->delim, a->col, &saveptr);
      if (!s)
        break;
      k = 0;
      switch (a->valtype)
        {
        case 0:                // int
          k = compare_int (atoi (s), *((int *) a->val), a->op);
//printf("j=%d s=%s k=%d op=%d val=%d\n\n",j,s,k,a->op,*((int *)a->val));
          break;
        case 1:                // double
          k =
            compare (cheap_strtod (s, decimal), *((double *) a->val), a->op);
          break;
        case 2:                // char *
          s = strip_nl_and_dequote (s);
//printf("char j=%d s=%s k=%d op=%d val=%s %p\n\n",j,s,k,a->op,(char *)a->val, a->val);
          k = compare_str (s, (char *) a->val, a->op);
          break;
        default:
          break;
        }
      if (k)
        {
          set[n] = j - a->skip;
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
              set = (int *) realloc (set, setsz * sizeof (int));
            }
        }
    }
  fclose (f);
  a->retval = set;
  a->nret = n;
  return 0;
}
#endif
