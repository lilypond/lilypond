/*
  kpath.c --  implement kpathsea bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/


#include <libguile.h>

#include "config.hh"
#include "guile-compatibility.hh"

#if KPATHSEA && HAVE_KPATHSEA_KPATHSEA_H

#include <stdio.h>
#include <string.h>

/*

The problem, as far as I can tell, is that MacOS X has its getopt
prototype in <unistd.h>, while I think other operating systems have it
in other places. <unistd.h> is included by kpathsea.h, so you end up
renaming both conflicting prototypes to YAKLUDGE.

I found a somewhat more elegant patch for this: Just #include
<unistd.h> before defining YAKLUDGE.

*/

#include <unistd.h>	

#define popen REALLYUGLYKLUDGE
#define pclose ANOTHERREALLYUGLYKLUDGE
#define getopt YAKLUDGE

#if HAVE_KPATHSEA_KPATHSEA_H
#include <kpathsea/kpathsea.h>
#include <kpathsea/tex-file.h>
#endif


#if KPATHSEA
/* FIXME: this should be part of kpathsea */

kpse_file_format_type
kpathsea_find_format (const char* name)
{
  int i;
  int len = strlen (name);
  for (i = 0; i < kpse_last_format; i++)
    {
      if (!kpse_format_info[i].type)
        kpse_init_format ((kpse_file_format_type) i);

      char const **suffixes[] = { kpse_format_info[i].suffix,
				  kpse_format_info[i].alt_suffix };
      for (int j = 0; j < 2; j++)
	for (char const **p = suffixes[j]; p && *p; p++)
	  {
	    int suflen = strlen (*p);
	    
	    if (!strncmp (name + len - suflen, *p, suflen))
	      return (kpse_file_format_type) i;
	  }
    }
  return kpse_last_format;
}
#endif






//	   "Return the absolute file name of @var{name}, "
//	   "or @code{#f} if not found.")
SCM
ly_kpathsea_find_file(SCM name)
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");

  char const * nm = scm_i_string_chars (name);
  char *p = kpse_find_file (nm, kpathsea_find_format (nm),
			    true);
  if (p)
    return scm_makfrom0str (p);
  return SCM_BOOL_F;
}

//   "Return the expanded version  @var{var}.")
SCM ly_kpathsea_expand_variable(SCM var)
{
  SCM_ASSERT_TYPE (scm_is_string (var), var, SCM_ARG1, __FUNCTION__, "string");

  char const * nm = scm_i_string_chars (var);
  char *result =  kpse_var_expand (nm);
  SCM ret =  scm_makfrom0str (result);
  free (result);

  return ret;
}



void
initialize_kpathsea ()
{
  /*
   initialize kpathsea
   */
  kpse_set_program_name ("lilypond", NULL);
  kpse_maketex_option ("tfm", TRUE);

  SCM find = scm_c_define_gsubr ("ly:kpathsea-find-file", 1, 0, 0, ly_kpathsea_find_file);
  scm_c_export ("ly:kpathsea-find-file", NULL);
  SCM expand = scm_c_define_gsubr ("ly:kpathsea-expand-variable", 1, 0, 0, ly_kpathsea_find_file);
  scm_c_export ("ly:kpathsea-expand-variable", NULL);
}

#endif
