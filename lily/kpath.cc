/*   
kpath.cc --  glue kpathsea to lily. Need some ugly kludges for gcc 2.96

source file of the GNU LilyPond music typesetter

(c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

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

#include <sys/types.h>
#include <fcntl.h>
#if HAVE_SYS_STAT_H 
#include <sys/stat.h>
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

  /*
    UGH: should not use DIR_DATADIR, but /var,

    hmm, but where to get /var?
    
   */

  int fd;
  struct stat stat_buf;
  if (stat (DIR_DATADIR "/tfm", &stat_buf) == 0
      && (S_ISDIR (stat_buf.st_mode) || S_ISLNK (stat_buf.st_mode))
      // ugh, howto test if we can write there?
      //      && (stat_buf.st_mode & S_IWUSR))
      && ((fd = open (DIR_DATADIR "/tfm/lily", O_CREAT)) != -1))
    {
      close (fd);
      unlink (DIR_DATADIR "/tfm/lily");
      kpse_format_info[kpse_tfm_format].program ="mktextfm --destdir " DIR_DATADIR "/tfm";

      kpse_format_info[kpse_tfm_format].client_path =
	(DIR_DATADIR "/tfm" );
    }
  
#endif
}


