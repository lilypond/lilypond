/*   
  kpath.cc -- glue kpathsea to lily. Need some ugly kludges for gcc 2.96

  source file of the GNU LilyPond music typesetter

  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>
#include <string.h>

#include "config.h"
#include "string.hh"

#define popen REALLYUGLYKLUDGE
#define pclose ANOTHERREALLYUGLYKLUDGE

#if HAVE_KPATHSEA_KPATHSEA_H
extern "C" {
#include <kpathsea/kpathsea.h>
#include <kpathsea/tex-file.h>
}
#endif

#include "kpath.hh"
#include "lily-version.hh"


char *
ly_find_afm (char const * name)
{
#if (KPATHSEA && HAVE_KPSE_FIND_FILE)
  return kpse_find_file (name, kpse_afm_format, true);
#endif
  return 0;
}

char *
ly_find_tfm (char const * name)
{
#if (KPATHSEA && HAVE_KPSE_FIND_FILE)
  return kpse_find_file (name, kpse_tfm_format, true);
#endif
  return 0;
}


void
ly_init_kpath (char *av0)
{
#if KPATHSEA && HAVE_KPATHSEA_KPATHSEA_H

  /*
    Remove the setting for TFMFONTS if we have kpathsea, because
    kpathsea can find TFM fonts anyway.

    If we don't lily will want to make tfms for cmr fonts, even if
    there is a :: entry in the TFMFONTS path.

    This will fail if a user has special fonts (outside of feta) that
    can not be found by kpath.

    If TFMFONTS is unset, TFMs of feta will be generated on the
    fly. The risk is that this will cause checksum mismatch errors,
    but MF is reasonably deterministic (so we hope not).

    The advantage is that the PK font will also be generated under
    /var/texmf/fonts, reducing clutter and compilation time.
    
  */
  unsetenv ("TFMFONTS");
  
  /*
   initialize kpathsea
   */
  kpse_set_program_name (av0, NULL);
  kpse_maketex_option ("tfm", TRUE);

  String my_tfm = "$VARTEXFONTS/tfm/lilypond/";
  my_tfm += version_str () + "/";

  char * mypath = kpse_expand (my_tfm.ch_C ());
  String prog = "mktextfm --destdir ";
  prog += mypath;
  
  kpse_format_info[kpse_tfm_format].program = strdup (prog.ch_C ());
  kpse_format_info[kpse_tfm_format].client_path = mypath;
#endif
}


