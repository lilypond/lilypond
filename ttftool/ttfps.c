/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

#include <sys/types.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include "types.h"
#include "proto.h"

static void endianness_test (void);
static void usage (char *);

int verbosity = 0;

void
create_type42 (const char *infile, FILE * out)
{
  int fd, i;
  struct OffsetTable ot;
  struct HeadTable *ht;
  struct PostTable *pt;
  struct TableDirectoryEntry *td;
  void *loca = NULL;
  struct HheaTable *hhea = NULL;
  struct Box *bbox = NULL;
  longHorMetric *hmtx = NULL;
  char **strings = NULL;
  struct GlyphName *gnt = NULL;
  struct KernEntry0 **ke;
  int *nke;
  int nglyphs, postType, nkern;
  off_t headOff = 0, maxpOff = 0, postOff = 0, nameOff = 0,
    locaOff = 0, glyfOff = 0, hheaOff = 0, hmtxOff = 0, kernOff = 0;

  extern char *optarg;
  extern int optind;
  int c;

  endianness_test ();

  if ((fd = open (infile, O_RDONLY)) < 0)
    syserror ("Error opening input file");

  td = readDirectory (fd, &ot);
  if (verbosity >= 2)
    fprintf (stderr, "True type version %d.%u\n",
	     ot.version.mantissa, ot.version.fraction);

  for (i = 0; i < ot.numTables; i++)
    {
      if (verbosity >= 2)
	fprintf (stderr, "Found `%c%c%c%c' table\n",
		 (char) (td[i].tag >> 24),
		 (char) (td[i].tag >> 16) & 255,
		 (char) (td[i].tag >> 8) & 255, (char) td[i].tag & 255);
      switch (td[i].tag)
	{
	case MAKE_ULONG ('m', 'a', 'x', 'p'):
	  maxpOff = td[i].offset;
	  break;
	case MAKE_ULONG ('h', 'e', 'a', 'd'):
	  headOff = td[i].offset;
	  break;
	case MAKE_ULONG ('p', 'o', 's', 't'):
	  postOff = td[i].offset;
	  break;
	case MAKE_ULONG ('n', 'a', 'm', 'e'):
	  nameOff = td[i].offset;
	  break;
	case MAKE_ULONG ('l', 'o', 'c', 'a'):
	  locaOff = td[i].offset;
	  break;
	case MAKE_ULONG ('g', 'l', 'y', 'f'):
	  glyfOff = td[i].offset;
	  break;
	case MAKE_ULONG ('h', 'h', 'e', 'a'):
	  hheaOff = td[i].offset;
	  break;
	case MAKE_ULONG ('h', 'm', 't', 'x'):
	  hmtxOff = td[i].offset;
	  break;
	case MAKE_ULONG ('k', 'e', 'r', 'n'):
	  kernOff = td[i].offset;
	  break;
	}
    }
  if (maxpOff == 0 || headOff == 0 || postOff == 0 || nameOff == 0)
    error ("Incomplete TTF file\n");

  if (verbosity >= 1)
    fprintf (stderr, "Processing `maxp' table\n");
  surely_lseek (fd, maxpOff, SEEK_SET);
  nglyphs = readMaxpTable (fd);
  if (verbosity >= 1)
    fprintf (stderr, "  %d glyphs\n", nglyphs);

  if (verbosity >= 1)
    fprintf (stderr, "Processing `head' table\n");
  surely_lseek (fd, headOff, SEEK_SET);
  ht = mymalloc (sizeof (struct HeadTable));
  readHeadTable (fd, ht);

  if (verbosity >= 1)
    fprintf (stderr, "Processing `post' table\n");
  surely_lseek (fd, postOff, SEEK_SET);
  pt = mymalloc (sizeof (struct PostTable));
  postType = readPostTable (fd, nglyphs, pt, &gnt);

  if (verbosity >= 1)
    fprintf (stderr, "Processing `name' table\n");
  surely_lseek (fd, nameOff, SEEK_SET);
  strings = readNamingTable (fd);

  if (verbosity >= 1)
    fprintf (stderr, "Generating PS file\n");
  printPSFont (out, ht, strings, nglyphs, postType, pt, gnt, fd);
  fclose (out);
  if (verbosity >= 1)
    fprintf (stderr, "Done.\n");
  close (fd);
}


static void
endianness_test ()
{
  union
  {
    BYTE b[4];
    ULONG l;
  } x;
  ULONG v;

  x.b[0] = 1;
  x.b[1] = 2;
  x.b[2] = 3;
  x.b[3] = 4;

  v = UL (x.l);

  if (v != (((((1 << 8) + 2) << 8) + 3) << 8) + 4)
    {
      fprintf (stderr, "Code badly compiled for this architecture\n");
      fprintf (stderr, "Please set SMALLENDIAN and recompile\n");
      exit (2);
    }
}
