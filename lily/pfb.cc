/* 
  pfb.cc --  implement pfb conversion.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "string.hh"
#include "lily-guile.hh"
#include "source-file.hh"

char *
pfb2pfa (Byte const * pfb, int length)
{
  char * out = new char[1];
  int olen = 0;
  
  Byte const * p = pfb;
  while (p  < pfb + length)  
    {
      if (*p++ != 128)
	break;

      Byte type = *p++;
      if (type == 3)
	break ;

      unsigned seglen =
	p[0] | (p[1] << 8)
	| (p[2] << 16) | (p[3] << 24);

      p += 4;
      if (type == 1)
	{
	  out = (char*)realloc (out, olen + seglen + 1);
	  char* outp = out + olen ;
	  memcpy (outp, p, seglen);
	  olen += seglen; 
	  p += seglen;
	}
      else if (type == 2)
	{
	  unsigned outlength =  (seglen * 2) + (seglen / 32) + 2;

	  out = (char*)realloc (out, olen + outlength + 1);

	  char * outp = out + olen;
	  for (int i = seglen; i--;)
	    {
	      sprintf (outp, "%02x", *p++);
	      outp += 2;
	      if (!(i % 32))
		{
		  *outp ++ = '\n';
		}
	    }

	  olen = outp - out;
	}
      
    }
  out[olen] = 0;

  return out;
}

LY_DEFINE(ly_pfb_to_pfa, "ly:pfb->pfa",
	  1,0,0, (SCM pfb_path),
	  "Convert the contents of a PFB file to PFA."
	  )
{
  SCM_ASSERT_TYPE(ly_c_string_p (pfb_path), pfb_path,
		  SCM_ARG1, __FUNCTION__, "string");

  String path = ly_scm2string (pfb_path);
  int len ; 
  char *str = gulp_file (path, &len);
  char *pfa = pfb2pfa ((Byte*)str, len);
  
  SCM pfa_scm = scm_makfrom0str(pfa);
  free (pfa);
  delete str;
  return pfa_scm;
}
	  
