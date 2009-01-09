/*
  music-function.cc -- implement music_function

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music-function.hh"

#include "music.hh"

static scm_t_bits music_function_tag;

/* Print a textual represenation of the smob to a given port.  */
static int
print_music_function (SCM b, SCM port, scm_print_state *)
{
  SCM value = SCM_CELL_OBJECT_1 (b);

  scm_puts ("#<Music function ", port);
  scm_write (value, port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}

bool
is_music_function (SCM music_function)
{
  return (SCM_NIMP (music_function) && SCM_CELL_TYPE (music_function) == music_function_tag);
}

SCM
get_music_function_transform (SCM music_function)
{
  if (!is_music_function (music_function))
    return SCM_UNDEFINED;

  return SCM_CELL_OBJECT_1 (music_function);
}

static void
init_music_function (void)
{
  music_function_tag = scm_make_smob_type ("music-function", 0);
  scm_set_smob_mark (music_function_tag, scm_markcdr);
  scm_set_smob_print (music_function_tag, print_music_function);
}

SCM
make_music_function (SCM signature, SCM func)
{
  scm_set_object_property_x (func, ly_symbol2scm ("music-function-signature"),
			     signature);

  SCM_RETURN_NEWSMOB (music_function_tag, func);
}

ADD_SCM_INIT_FUNC (music_function_tag, init_music_function);

