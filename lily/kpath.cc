/*   
kpath.cc --  glue kpathsea to lily. Need some ugly kludges for gcc 2.96

source file of the GNU LilyPond music typesetter

(c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include <stdio.h>
#include "config.h"

#define popen REALLYUGLYKLUDGE
#define pclose ANOTHERREALLYUGLYKLUDGE

#if HAVE_KPATHSEA_KPATHSEA_H
extern "C" {
#include <kpathsea/kpathsea.h>
#include <kpathsea/tex-file.h>
}
#endif

#include "kpath.hh"



char * ly_find_afm (char const * name)
{
#if (KPATHSEA && HAVE_KPSE_FIND_FILE)
  return kpse_find_file (name, kpse_afm_format, true);
#endif
  return 0;
}

char * ly_find_tfm (char const * name)
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
   initialize kpathsea
   */
  kpse_set_program_name(av0, NULL);
  kpse_maketex_option("tfm", TRUE);
#endif
}
