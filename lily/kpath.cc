/*   
  kpath.cc -- glue kpathsea to lily. Need some ugly kludges for gcc 2.96

  source file of the GNU LilyPond music typesetter

  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>
#include <string.h>

#include "config.h"

#define popen REALLYUGLYKLUDGE
#define pclose ANOTHERREALLYUGLYKLUDGE

#if HAVE_KPATHSEA_KPATHSEA_H
extern "C" {
#include <kpathsea/kpathsea.h>
#include <kpathsea/tex-file.h>
}
#endif

#include "file-path.hh"
#include "string.hh"
#include "main.hh"
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

String
ly_find_tfm (char const * name)
{
  String p = global_path.find (String (name) + ".tfm");

  if (p.length_i ())
    return p;
  
#if (KPATHSEA && HAVE_KPSE_FIND_FILE)
  return kpse_find_file (name, kpse_tfm_format, true);
#endif
  return "";
}


void
ly_init_kpath (char *av0)
{
#if KPATHSEA && HAVE_KPATHSEA_KPATHSEA_H
  /*
    We take two pronged approach to tfms:

    * the lilypond tfms (feta*.tfm) are found through our own routines.

    * the TeX tfms are found through vanilla kpathsea.

    (* other TFMs are not found, i.e. don't use them. )

    PRO:
 
    - TFM and AFM checksums always match in Lily.

    - less hassle, no kpathsea spaghetti

    CON:

    - feta PK files are often recreated, locally
    Solution: cache PK files locally?

    - need env. vars to make sure that TeX finds the TFMs

    - Outdated PK (TFM?) font files are not automatically removed,
    since VERSION is not part of the standard location.


    ALTERNATIVE

    we have tried to come up with schemes that leave this kind of work
    to kpathsea with objective of fixing the CONs, but miserably
    failed. TeX installations and kpathsea itself form a buggy,
    inconsistent, and unorderly mess.
    
  */

  /*
   initialize kpathsea
   */
  kpse_set_program_name (av0, NULL);
  kpse_maketex_option ("tfm", TRUE);

#if  0


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

#ifndef __CYGWIN__  /* mktextfm/mktexpk does not work on windows */
  unsetenv ("TFMFONTS");
#endif

#ifdef DEBIAN
  String my_tfm = "$VARTEXFONTS/tfm/public/lilypond";
#else
  String my_tfm = "$VARTEXFONTS/tfm/lilypond/";
  my_tfm += version_str () + "/";
#endif

#ifdef DEBIAN
  char * mypath = strdup ((my_tfm + ":").ch_C());
  kpse_format_info[kpse_tfm_format].client_path = mypath;
#else
  char * mypath = kpse_expand (my_tfm.ch_C ());
			   
  String prog = "mktextfm --destdir ";
  prog += mypath;
  
  kpse_format_info[kpse_tfm_format].program = strdup (prog.ch_C ());
#endif
#endif
#endif
  
}


