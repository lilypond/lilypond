/*   
kpath.cc --  glue kpathsea to lily. Need some ugly kludges for gcc 2.96

source file of the GNU LilyPond music typesetter

(c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include <stdio.h>

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
#include "version.hh"


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

  /*
    ugh: apparently the program_args is non-functional.
   */
#define VERSION MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL
  
#define MY_TFMPATH "$VARTEXFONTS/tfm/lilypond/" VERSION "/"

  char * mypath = kpse_expand (MY_TFMPATH);
  String prog = "mktextfm --destdir ";
  prog += mypath;
  
  kpse_format_info[kpse_tfm_format].program = strdup (prog.ch_C());
  kpse_format_info[kpse_tfm_format].client_path = mypath;
#endif
}


