/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

#include <sys/types.h>
#include <unistd.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"
#include "proto.h"

#define ALIAS_FILE_TO_FILECOOKIE

#include "libc-extension.hh"

void *
mymalloc (size_t size)
{
  void *p;
  if ((p = malloc (size)) == NULL)
    error ("Unable to allocate memory\n");
  return p;
}

void *
mycalloc (size_t nelem, size_t elsize)
{
  void *p;
  if ((p = calloc (nelem, elsize)) == NULL)
    error ("Unable to allocate memory\n");
  return p;
}

void *
myrealloc (void *ptr, size_t size)
{
  void *p;
  if ((p = realloc (ptr, size)) == NULL)
    error ("Unable to allocate memory\n");
  return p;
}

off_t
surely_lseek (int fildes, off_t offset, int whence)
{
  off_t result;
  if ((result = lseek (fildes, offset, whence)) < 0)
    error ("Bad TTF file");
  return result;
}

void
error (char *string)
{
  fprintf (stderr, "%s\n", string);
  exit (3);
 /*NOTREACHED*/}

void
syserror (char *string)
{
  perror (string);
  exit (3);
 /*NOTREACHED*/}

ssize_t
surely_read (int fildes, void *buf, size_t nbyte)
{
  ssize_t n;
  if ((n = read (fildes, buf, nbyte)) < nbyte)
    error ("Bad TTF file");
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
fputpss (char *s, FILE * stream)
{
  while (*s)
    {
      if ((*s & 0200) == 0 && *s >= 040 && *s != '(' && *s != ')')
	putc (*s, stream);
      else
	fprintf (stream, "\\%03o", (unsigned char) *s);
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
