/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

#include <sys/types.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "types.h"
#include "proto.h"

#include "ttftool.h"
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
printPSFont (void *out, struct HeadTable *ht,
	     char **strings, int nglyphs, int postType,
	     struct PostTable *pt, struct GlyphName *gnt, int fd)
{
  printPSHeader (out, ht, strings, pt);
  printPSData (out, fd);
  printPSTrailer (out, nglyphs, postType, gnt);
}

void
printPSHeader (void *out, struct HeadTable *ht,
	       char **strings, struct PostTable *pt)
{
  lily_cookie_fprintf (out, "%%!PS-TrueTypeFont\n");
  if (pt->maxMemType42)
    lily_cookie_fprintf (out, "%%%%VMUsage: %ld %ld\n", pt->minMemType42,
	     pt->maxMemType42);
  lily_cookie_fprintf (out, "%d dict begin\n", 11);
  lily_cookie_fprintf (out, "/FontName /%s def\n", strings[6] ? strings[6] : "Unknown");
  lily_cookie_fprintf (out, "/Encoding StandardEncoding def\n");
  lily_cookie_fprintf (out, "/PaintType 0 def\n/FontMatrix [1 0 0 1 0 0] def\n");
  lily_cookie_fprintf (out, "/FontBBox [%ld %ld %ld %ld] def\n",
	   ht->xMin * 1000L / ht->unitsPerEm,
	   ht->yMin * 1000L / ht->unitsPerEm,
	   ht->xMax * 1000L / ht->unitsPerEm,
	   ht->yMax * 1000L / ht->unitsPerEm);
  lily_cookie_fprintf (out, "/FontType 42 def\n");
  lily_cookie_fprintf (out, "/FontInfo 8 dict dup begin\n");
  lily_cookie_fprintf (out, "/version (%d.%d) def\n",
	   ht->fontRevision.mantissa, ht->fontRevision.fraction);
  if (strings[0])
    {
      lily_cookie_fprintf (out, "/Notice (");
      fputpss (strings[0], out);
      lily_cookie_fprintf (out, ") def\n");
    }
  if (strings[4])
    {
      lily_cookie_fprintf (out, "/FullName (");
      fputpss (strings[4], out);
      lily_cookie_fprintf (out, ") def\n");
    }
  if (strings[1])
    {
      lily_cookie_fprintf (out, "/FamilyName (");
      fputpss (strings[1], out);
      lily_cookie_fprintf (out, ") def\n");
    }
  lily_cookie_fprintf (out, "/isFixedPitch %s def\n",
	   pt->isFixedPitch ? "true" : "false");
  lily_cookie_fprintf (out, "/UnderlinePosition %ld def\n",
	   pt->underlinePosition * 1000L / ht->unitsPerEm);
  lily_cookie_fprintf (out, "/UnderlineThickness %ld def\n",
	   pt->underlineThickness * 1000L / ht->unitsPerEm);
  lily_cookie_fprintf (out, "end readonly def\n");
}

void
printPSData (void *out, int fd)
{
  static char xdigits[] = "0123456789ABCDEF";

  unsigned char *buffer;
  int i, j;

  surely_lseek (fd, 0, SEEK_SET);

  buffer = mymalloc (CHUNKSIZE);

  lily_cookie_fprintf (out, "/sfnts [");
  for (;;)
    {
      i = read (fd, buffer, CHUNKSIZE);
      if (i == 0)
	break;
      lily_cookie_fprintf (out, "\n<");
      for (j = 0; j < i; j++)
	{
	  if (j != 0 && j % 36 == 0)
	    lily_cookie_putc ('\n', out);
	  /* lily_cookie_fprintf (out,"%02X",(int)buffer[j]) is too slow */
	  lily_cookie_putc (xdigits[(buffer[j] & 0xF0) >> 4], out);
	  lily_cookie_putc (xdigits[buffer[j] & 0x0F], out);
	}
      lily_cookie_fprintf (out, "00>");	/* Adobe bug? */
      if (i < CHUNKSIZE)
	break;
    }
  lily_cookie_fprintf (out, "\n] def\n");
  free (buffer);
}

void
printPSTrailer (void *out, int nglyphs, int postType, struct GlyphName *gnt)
{
  int i, n;
  char *name;

  lily_cookie_fprintf (out, "/CharStrings %d dict dup begin\n", nglyphs);
  switch (postType)
    {
    case 2:
      for (n = i = 0; i < nglyphs; i++)
	{
	  if (n != 0 && n % 4 == 0)
	    lily_cookie_fprintf (out, "\n");
	  name = NAMEOF (i);
	  if (name)
	    {
	      lily_cookie_fprintf (out, "/%s %d def ", name, i);
	      n++;
	    }
	}
      break;
    default:
      if (postType != 1)
	{
	  if (ttf_verbosity > -2)
	    fprintf (stderr,
		     "No glyph name table; assuming MacGlyphEncoding\n");
	}
      for (i = 0; i < 258 && i < nglyphs; i++)
	{
	  lily_cookie_fprintf (out, "/%s %d def ", macGlyphEncoding[i], i);
	  if (i != 0 && i % 4 == 0)
	    lily_cookie_fprintf (out, "\n");
	}
      break;
    }
  lily_cookie_fprintf (out, "end readonly def\n");
  lily_cookie_fprintf (out, "FontName currentdict end definefont pop\n");
}
