/*
  pfb.cc -- implement pfb conversion.

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <cstdlib>
#include <cstdio>
#include <cstring>

#include "source-file.hh"
#include "memory-stream.hh"
#include "ttftool.h"

char *
pfb2pfa (Byte const *pfb, int length)
{
  char *out = new char[1];
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
		{
		  *outp++ = '\n';
		}
	    }

	  olen = outp - out;
	}

    }
  out[olen] = 0;

  return out;
}

LY_DEFINE (ly_pfb_to_pfa, "ly:pfb->pfa",
	   1, 0, 0, (SCM pfb_file_name),
	   "Convert the contents of a PFB file to PFA.")
{
  SCM_ASSERT_TYPE (scm_is_string (pfb_file_name), pfb_file_name,
		   SCM_ARG1, __FUNCTION__, "string");

  String file_name = ly_scm2string (pfb_file_name);
  int len;
  char *str = gulp_file (file_name, &len);
  char *pfa = pfb2pfa ((Byte *)str, len);

  SCM pfa_scm = scm_makfrom0str (pfa);
  free (pfa);
  delete str;
  return pfa_scm;
}

LY_DEFINE (ly_ttf_to_pfa, "ly:ttf->pfa",
	   1, 0, 0, (SCM ttf_file_name),
	   "Convert the contents of a TTF file to Type42 PFA, returning it as "
	   " a string.")
{
  SCM_ASSERT_TYPE (scm_is_string (ttf_file_name), ttf_file_name,
		   SCM_ARG1, __FUNCTION__, "string");

  String file_name = ly_scm2string (ttf_file_name);

  Memory_out_stream stream;
  create_type42 (file_name.to_str0 (), stream.get_file ());
  SCM asscm = scm_from_locale_stringn (stream.get_string (),
				       stream.get_length ());

  return asscm;
}
