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
#include "warn.hh"

String
ly_find_afm (char const * name)
{
#if (KPATHSEA && HAVE_KPSE_FIND_FILE)
  char * name_ptr =  kpse_find_file (name, kpse_afm_format, true);

  if(!name_ptr)
    {
  /*
    don't mutter about afms, since we try to find them first, and lots of
    TFMs don't have AFMs. 
   */
      //      warning (_f("kpathsea couldn't find AFM file `%s'", name));
    }
  else
    return name_ptr;
  
#endif
  return "";
}

String
ly_find_tfm (char const * name)
{
  String p = global_path.find (String (name) + ".tfm");

  if (p.length_i ())
    return p;
  
#if (KPATHSEA && HAVE_KPSE_FIND_FILE)
  char * name_ptr =  kpse_find_file (name, kpse_tfm_format, true);
  if(!name_ptr)
    {
      warning (_f("Kpathsea couldn't find TFM file `%s'", name));
    }
  else
    return name_ptr;
  
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
#endif
}


