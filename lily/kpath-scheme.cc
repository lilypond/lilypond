/*
  kpath-scheme.cc --  implement kpathsea bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "kpath.hh"

#include <cstdio>
#include <cstring>

/*

The problem, as far as I can tell, is that MacOS X has its getopt
prototype in <unistd.h>, while I think other operating systems have it
in other places. <unistd.h> is included by kpathsea.h, so you end up
renaming both conflicting prototypes to YAKLUDGE.

I found a somewhat more elegant patch for this: Just #include
<unistd.h> before defining YAKLUDGE.

*/

#include <unistd.h>	

#include "config.hh"

#define popen REALLYUGLYKLUDGE
#define pclose ANOTHERREALLYUGLYKLUDGE
#define getopt YAKLUDGE

#if HAVE_KPATHSEA_KPATHSEA_H
extern "C" {
#include <kpathsea/kpathsea.h>
#include <kpathsea/tex-file.h>
}
#endif

#include "file-path.hh"
#include "main.hh"
#include "source-file.hh"
#include "warn.hh"
#include "kpath-private.hh"

LY_DEFINE (ly_find_file, "ly:find-file",
	   1, 0, 0, (SCM name),
	   "Return the absolute file name of @var{name}, "
	   "or @code{#f} if not found.")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");

  String nm = ly_scm2string (name);
  String file_name = global_path.find (nm);
  if (file_name.is_empty ())
    return SCM_BOOL_F;
  
  return scm_makfrom0str (file_name.to_str0 ());
}

LY_DEFINE (ly_kpathsea_find_file, "ly:kpathsea-find-file",
	   1, 0, 0, (SCM name),
	   "Return the absolute file name of @var{name}, "
	   "or @code{#f} if not found.")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");

  String nm = ly_scm2string (name);
  String file_name = global_path.find (nm);
  if (file_name.is_empty ())
    {
      if (char *p = kpse_find_file (nm.to_str0 (), kpathsea_find_format (nm),
				    true))
	return scm_makfrom0str (p);
      return SCM_BOOL_F;
    }
  return scm_makfrom0str (file_name.to_str0 ());
}

LY_DEFINE (ly_kpathsea_expand_variable, "ly:kpathsea-expand-variable",
	   1, 0, 0, (SCM var),
	   "Return the expanded version  @var{var}.")
{
  SCM_ASSERT_TYPE (scm_is_string (var), var, SCM_ARG1, __FUNCTION__, "string");

  String nm = ly_scm2string (var);
  char *result =  kpse_var_expand (nm.to_str0 ());
  SCM ret =  scm_makfrom0str (result);
  delete[] result;

  return ret;
}
