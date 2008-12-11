/*
  pfb.cc -- implement pfb conversion.

  source file of the GNU LilyPond music typesetter

  (c) 2004--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <cstdlib>
#include <cstdio>
#include <cstring>
using namespace std;

#include "program-option.hh"
#include "source-file.hh"
#include "memory-stream.hh"
#include "open-type-font.hh"
#include "main.hh"
#include "warn.hh"

char *
pfb2pfa (Byte const *pfb, int length)
{
  char *out = (char*) malloc(sizeof(char));
  int olen = 0;

  Byte const *p = pfb;
  while (p < pfb + length)
    {
      if (*p++ != 128)
	break;

      Byte type = *p++;
      if (type == 3)
	break;

      unsigned seglen
	= p[0] | (p[1] << 8)
	| (p[2] << 16) | (p[3] << 24);

      p += 4;
      if (type == 1)
	{
	  out = (char *)realloc (out, olen + seglen + 1);
	  char *outp = out + olen;
	  memcpy (outp, p, seglen);
	  olen += seglen;
	  p += seglen;
	}
      else if (type == 2)
	{
	  unsigned outlength = (seglen * 2) + (seglen / 32) + 2;

	  out = (char *)realloc (out, olen + outlength + 1);

	  char *outp = out + olen;
	  for (int i = seglen; i--;)
	    {
	      sprintf (outp, "%02x", *p++);
	      outp += 2;
	      if (! (i % 32))
		*outp++ = '\n';
	    }

	  olen = outp - out;
	}
    }
  out[olen] = 0;

  return out;
}

