/*
  kpath-private.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef KPATH_PRIVATE_HH
#define KPATH_PRIVATE_HH

#include "config.hh"

#define popen KPATHSEA_HAS_POPEN_PROTOTYPE_PROBLEM
#define pclose KPATHSEA_HAS_PCLOSE_PROTOTYPE_PROBLEM
#define getopt KPATHSEA_HAS_GETOPT_PROTOTYPE_PROBLEM

#if HAVE_KPATHSEA_KPATHSEA_H
extern "C" {
  #include <kpathsea/kpathsea.h>
  #include <kpathsea/tex-file.h>
  extern void *kpathsea_handle;
  extern char *(*dl_kpse_find_file) (char const*, kpse_file_format_type,
				     boolean);
  extern char const *(*dl_kpse_init_format) (kpse_file_format_type);
  extern void (*dl_kpse_maketex_option) (char const*, boolean);
  extern void (*dl_kpse_set_program_name) (char const*);
  extern char *(*dl_kpse_var_expand) (char const*);
  extern kpse_format_info_type (*dl_kpse_format_info)[kpse_last_format];
}
#endif

kpse_file_format_type kpathsea_find_format (String name);

#endif /* KPATH_PRIVATE_HH */



