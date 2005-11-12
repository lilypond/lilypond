/*
  kpath.c --  implement kpathsea bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/


#include <libguile.h>

#include "config.hh"

#if KPATHSEA

#include "guile-compatibility.hh"

#if !HAVE_GETTEXT
inline char *
gettext (char const *s)
{
  return (char *)s;
}
#else
#include <libintl.h>
#endif

#define _(x) gettext (x)

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>

/* The (?) problem, as far as I (?) can tell, is that MacOS X has its
   getopt prototype in <unistd.h>, while I think other operating
   systems have it in other places. <unistd.h> is included by
   kpathsea.h, so you end up renaming both conflicting prototypes to
   KPATHSEA_HAS_GETOPT_PROTOTYPE_PROBLEM.

   I (?) found a somewhat more elegant patch for this: Just #include
   <unistd.h> before defining KPATHSEA_HAS_GETOPT_PROTOTYPE_PROBLEM.  */

#include <unistd.h>	

#define popen KPATHSEA_HAS_POPEN_PROTOTYPE_PROBLEM
#define pclose KPATHSEA_HAS_PCLOSE_PROTOTYPE_PROBLEM
#define getopt KPATHSEA_HAS_GETOPT_PROTOTYPE_PROBLEM

#if HAVE_KPATHSEA_KPATHSEA_H
#include <kpathsea/kpathsea.h>
#include <kpathsea/tex-file.h>
#endif

static void *kpathsea_handle = 0;
static char *(*dl_kpse_find_file) (char const*, kpse_file_format_type, boolean) = 0;
static void (*dl_kpse_maketex_option) (char const*, boolean) = 0;
static void (*dl_kpse_set_program_name) (char const*, char const*) = 0;
static char const *(*dl_kpse_init_format) (kpse_file_format_type) = 0;
static char *(*dl_kpse_var_expand) (char const*) = 0;
static kpse_format_info_type (*dl_kpse_format_info)[kpse_last_format] = 0;

kpse_file_format_type
kpathsea_find_format (char const* name)
{
  int i;
  int len = strlen (name);
  for (i = 0; i < kpse_last_format; i++)
    {
       if (!(*dl_kpse_format_info)[i].type)
        (*dl_kpse_init_format) ((kpse_file_format_type) i);

       char const **suffixes[] = { (*dl_kpse_format_info)[i].suffix,
				   (*dl_kpse_format_info)[i].alt_suffix };
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

//	   "Return the absolute file name of @var{name}, "
//	   "or @code{#f} if not found.")
SCM
ly_kpathsea_find_file (SCM name)
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");

  char const *nm = scm_i_string_chars (name);
  char *p = (*dl_kpse_find_file) (nm, kpathsea_find_format (nm), true);
  if (p)
    return scm_makfrom0str (p);
  return SCM_BOOL_F;
}

//   "Return the expanded version  @var{var}.")
SCM ly_kpathsea_expand_variable (SCM var)
{
  SCM_ASSERT_TYPE (scm_is_string (var), var, SCM_ARG1, __FUNCTION__, "string");

  char const *nm = scm_i_string_chars (var);
  char *result = (*dl_kpse_var_expand) (nm);
  SCM ret = scm_makfrom0str (result);
  free (result);

  return ret;
}

#ifndef DYNAMIC_OBJECT_EXTENSION
#define DYNAMIC_OBJECT_EXTENSION ".so"
#endif


static char const* LIBKPATHSEA = "libkpathsea" DYNAMIC_OBJECT_EXTENSION;

int
open_library ()
{
#if HAVE_LIBKPATHSEA_SO
  struct
  {
    void **func_pointer;
    char const *name; 
  } symbols[] = {
    {(void*)&dl_kpse_find_file, "kpse_find_file"},
    {(void*)&dl_kpse_set_program_name, "kpse_set_program_name"},
    {(void*)&dl_kpse_format_info, "kpse_format_info"},
    {(void*)&dl_kpse_init_format, "kpse_init_format"},
    {(void*)&dl_kpse_maketex_option, "kpse_maketex_option"},
    {(void*)&dl_kpse_var_expand, "kpse_var_expand"},
    {0,0}
  };


  dlerror ();
  kpathsea_handle = dlopen (LIBKPATHSEA, RTLD_LAZY);
  if (!kpathsea_handle)
    {
      /*
	todo i18n.
       */
      fprintf (stderr, _ ("can't dlopen: %s: %s"), LIBKPATHSEA, dlerror ());
      fprintf (stderr, _ ("install package: %s or %s"),
	       "libkpathsea3 (teTeX 2.x)",
	       "libkpathsea4 (teTeX 3.x)");

      return 1;
    }

  for (int i = 0; symbols[i].func_pointer; i++)
    {
      dlerror ();
      *symbols[i].func_pointer = dlsym (kpathsea_handle, symbols[i].name);
      if (!symbols[i].func_pointer)
	{
	  fprintf (stderr, _ ("no such symbol: %s: %s"),
		   symbols[i].name,
		   dlerror ());
	  return 1;
	}
    }
  return 0;
#else
  dl_kpse_find_file = &kpse_find_file;
  dl_kpse_set_program_name = &kpse_set_program_name;
  dl_kpse_format_info = &kpse_format_info;
  dl_kpse_init_format = &kpse_init_format;
  dl_kpse_maketex_option = &kpse_maketex_option;
  dl_kpse_var_expand = &kpse_var_expand;
  return 0;
#endif
}

void
initialize_kpathsea ()
{
  if (open_library ())
    {
      fprintf (stderr, _ ("error opening kpathsea library"));
      fprintf (stderr, _ ("aborting"));
      exit (1);
    }

  (*dl_kpse_set_program_name) ("lilypond", "lilypond");
  (*dl_kpse_maketex_option) ("tfm", TRUE);
  
  SCM find = scm_c_define_gsubr ("ly:kpathsea-find-file", 1, 0, 0,
				 ly_kpathsea_find_file);
  scm_c_export ("ly:kpathsea-find-file", NULL);
  SCM expand = scm_c_define_gsubr ("ly:kpathsea-expand-variable", 1, 0, 0,
				   ly_kpathsea_expand_variable);
  scm_c_export ("ly:kpathsea-expand-variable", NULL);
}

#endif
