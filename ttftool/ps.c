/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

#include <sys/types.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "types.h"
#include "proto.h"

#define ALIAS_FILE_TO_FILECOOKIE

#include "libc-extension.hh"

#define CHUNKSIZE 65534

#define NAMEOF(i) \
        ((i)==0?\
         ".notdef":\
         ((postType==2)?\
          ((gnt[i].type==0)?\
           (gnt[i].name.index==0?NULL:macGlyphEncoding[gnt[i].name.index]):\
           gnt[i].name.name):\
          ((i)<258?macGlyphEncoding[i]:NULL)))


void
printPSFont (FILE * out, struct HeadTable *ht,
	     char **strings, int nglyphs, int postType,
	     struct PostTable *pt, struct GlyphName *gnt, int fd)
{
  printPSHeader (out, ht, strings, pt);
  printPSData (out, fd);
  printPSTrailer (out, nglyphs, postType, gnt);
}

void
printPSHeader (FILE * out, struct HeadTable *ht,
	       char **strings, struct PostTable *pt)
{
  fprintf (out, "%%!PS-TrueTypeFont\n");
  if (pt->maxMemType42)
    fprintf (out, "%%%%VMUsage: %ld %ld\n", pt->minMemType42,
	     pt->maxMemType42);
  fprintf (out, "%d dict begin\n", 11);
  fprintf (out, "/FontName /%s def\n", strings[6] ? strings[6] : "Unknown");
  fprintf (out, "/Encoding StandardEncoding def\n");
  fprintf (out, "/PaintType 0 def\n/FontMatrix [1 0 0 1 0 0] def\n");
  fprintf (out, "/FontBBox [%ld %ld %ld %ld] def\n",
	   ht->xMin * 1000L / ht->unitsPerEm,
	   ht->yMin * 1000L / ht->unitsPerEm,
	   ht->xMax * 1000L / ht->unitsPerEm,
	   ht->yMax * 1000L / ht->unitsPerEm);
  fprintf (out, "/FontType 42 def\n");
  fprintf (out, "/FontInfo 8 dict dup begin\n");
  fprintf (out, "/version (%d.%d) def\n",
	   ht->fontRevision.mantissa, ht->fontRevision.fraction);
  if (strings[0])
    {
      fprintf (out, "/Notice (");
      fputpss (strings[0], out);
      fprintf (out, ") def\n");
    }
  if (strings[4])
    {
      fprintf (out, "/FullName (");
      fputpss (strings[4], out);
      fprintf (out, ") def\n");
    }
  if (strings[1])
    {
      fprintf (out, "/FamilyName (");
      fputpss (strings[1], out);
      fprintf (out, ") def\n");
    }
  fprintf (out, "/isFixedPitch %s def\n",
	   pt->isFixedPitch ? "true" : "false");
  fprintf (out, "/UnderlinePosition %ld def\n",
	   pt->underlinePosition * 1000L / ht->unitsPerEm);
  fprintf (out, "/UnderlineThickness %ld def\n",
	   pt->underlineThickness * 1000L / ht->unitsPerEm);
  fprintf (out, "end readonly def\n");
}

void
printPSData (FILE * out, int fd)
{
  static char xdigits[] = "0123456789ABCDEF";

  unsigned char *buffer;
  int i, j;

  surely_lseek (fd, 0, SEEK_SET);

  buffer = mymalloc (CHUNKSIZE);

  fprintf (out, "/sfnts [");
  for (;;)
    {
      i = read (fd, buffer, CHUNKSIZE);
      if (i == 0)
	break;
      fprintf (out, "\n<");
      for (j = 0; j < i; j++)
	{
	  if (j != 0 && j % 36 == 0)
	    putc ('\n', out);
	  /* fprintf(out,"%02X",(int)buffer[j]) is too slow */
	  putc (xdigits[(buffer[j] & 0xF0) >> 4], out);
	  putc (xdigits[buffer[j] & 0x0F], out);
	}
      fprintf (out, "00>");	/* Adobe bug? */
      if (i < CHUNKSIZE)
	break;
    }
  fprintf (out, "\n] def\n");
  free (buffer);
}

void
printPSTrailer (FILE * out, int nglyphs, int postType, struct GlyphName *gnt)
{
  int i, n;
  char *name;

  fprintf (out, "/CharStrings %d dict dup begin\n", nglyphs);
  switch (postType)
    {
    case 2:
      for (n = i = 0; i < nglyphs; i++)
	{
	  if (n != 0 && n % 4 == 0)
	    fprintf (out, "\n");
	  name = NAMEOF (i);
	  if (name)
	    {
	      fprintf (out, "/%s %d def ", name, i);
	      n++;
	    }
	}
      break;
    default:
      if (postType != 1)
	{
	  if (verbosity > -2)
	    fprintf (stderr,
		     "No glyph name table; assuming MacGlyphEncoding\n");
	}
      for (i = 0; i < 258 && i < nglyphs; i++)
	{
	  fprintf (out, "/%s %d def ", macGlyphEncoding[i], i);
	  if (i != 0 && i % 4 == 0)
	    fprintf (out, "\n");
	}
      break;
    }
  fprintf (out, "end readonly def\n");
  fprintf (out, "FontName currentdict end definefont pop\n");
}
