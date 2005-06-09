/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "types.h"
#include "proto.h"

#include "libc-extension.hh"



void *
mymalloc (size_t size)
{
  void *p;
  if ((p = malloc (size)) == NULL)
    ttf_error ("Unable to allocate memory\n");
  return p;
}

void *
mycalloc (size_t nelem, size_t elsize)
{
  void *p;
  if ((p = calloc (nelem, elsize)) == NULL)
    ttf_error ("Unable to allocate memory\n");
  return p;
}

void
ttf_error (char *string)
{
  fprintf (stderr, "TTF tool: %s\n", string);
  exit (3);
 /*NOTREACHED*/
}

void
syserror (char *string)
{
  char *sys_err = strerror (errno);
  fprintf (stderr, "TTF tool: %s (%s)\n",
	   string,
	   sys_err);
  exit (3);
}

void *
myrealloc (void *ptr, size_t size)
{
  void *p;
  if ((p = realloc (ptr, size)) == NULL)
    ttf_error ("Unable to allocate memory\n");
  return p;
}

off_t
surely_lseek (int fildes, off_t offset, int whence)
{
  off_t result;
  if ((result = lseek (fildes, offset, whence)) < 0)
    {
      char s[100];
      sprintf (s, "Cannot seek to %d %ld", whence, offset);
      syserror (s);
    }
  return result;
}

ssize_t
surely_read (int fildes, void *buf, size_t nbyte)
{
  ssize_t n;
  if ((n = read (fildes, buf, nbyte)) < nbyte)
    {
      char s[100];
      sprintf (s, "read too little in surely_read(), expect %d got %d", nbyte, n);
      syserror  (s);
    }
  return n;
}

char *
unistrncpy (char *dst, char *str, size_t length)
{
  int i, j;

  for (i = j = 0; i < length; i += 2)
    if (str[i] == 0)
      dst[j++] = str[i + 1];
  dst[j] = '\0';
  return dst;
}

void
fputpss (char *s, void *stream)
{
  while (*s)
    {
      if ((*s & 0200) == 0 && *s >= 040 && *s != '(' && *s != ')')
 	lily_cookie_putc (*s, stream);
      else
	lily_cookie_fprintf (stream, "\\%03o", (unsigned char) *s);
      s++;
    }
}

/* Hashtables */

unsigned
hash (char *string)
{
  int i;
  unsigned u = 0;
  for (i = 0; string[i] != '\0'; i++)
    u = (u << 2) + string[i];
  return u;
}

struct hashtable *
make_hashtable (int size)
{
  struct hashtable *t;

  t = mymalloc (sizeof (struct hashtable));
  t->size = size;
  t->buckets = mycalloc (size, sizeof (struct hashtable_bucket *));

  return t;
}

int
puthash (struct hashtable *t, char *key, int value)
{
  int i;

  i = hash (key) % t->size;

  if (t->buckets[i] == 0)
    {
      t->buckets[i] = mymalloc (sizeof (struct hashtable_bucket));
      t->buckets[i]->entries = mymalloc (4 * sizeof (struct hashtable_entry));
      t->buckets[i]->size = 4;
      t->buckets[i]->nentries = 0;
    }

  if (t->buckets[i]->nentries >= t->buckets[i]->size)
    {
      t->buckets[i]->entries = myrealloc (t->buckets[i]->entries,
					  t->buckets[i]->size * 2 *
					  sizeof (struct hashtable_entry));
      t->buckets[i]->size *= 2;
    }

  t->buckets[i]->entries[t->buckets[i]->nentries].key = key;
  t->buckets[i]->entries[t->buckets[i]->nentries].value = value;
  t->buckets[i]->nentries++;

  return value;
}

int
gethash (struct hashtable *t, char *key)
{
  int i, j;

  i = hash (key) % t->size;
  if (t->buckets[i])
    for (j = 0; j < t->buckets[i]->nentries; j++)
      if (!strcmp (key, t->buckets[i]->entries[j].key))
	return t->buckets[i]->entries[j].value;
  return -1;
}
