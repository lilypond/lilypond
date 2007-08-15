/*
  pfb.cc -- implement pfb conversion.

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
		*outp++ = '\n';
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

  string file_name = ly_scm2string (pfb_file_name);

  if (be_verbose_global)
    progress_indication ("[" + file_name);
  
  vector<char> pfb_string = gulp_file (file_name, 0);
  char *pfa = pfb2pfa ((Byte *) &pfb_string[0], pfb_string.size ());
  
  SCM pfa_scm = scm_makfrom0str (pfa);
  free (pfa);

  if (be_verbose_global)
    progress_indication ("]");

  return pfa_scm;
}

LY_DEFINE (ly_otf_to_cff, "ly:otf->cff",
	   1, 0, 0, (SCM otf_file_name),
	   "Convert the contents of a OTF file to CFF file, returning it as "
	   " a string.")
{
  SCM_ASSERT_TYPE (scm_is_string (otf_file_name), otf_file_name,
		   SCM_ARG1, __FUNCTION__, "string");

  string file_name = ly_scm2string (otf_file_name);
  if (be_verbose_global)
    progress_indication ("[" + file_name);

  FT_Face face = open_ft_face (file_name);
  string table = get_otf_table (face, "CFF ");

  SCM asscm = scm_from_locale_stringn ((char *) table.data (),
				       table.length ());

  if (be_verbose_global)
    progress_indication ("]");

  return asscm;
}

