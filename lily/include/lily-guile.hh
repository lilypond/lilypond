/*
  lily-guile.hh encapsulate guile

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LILY_GUILE_HH
#define LILY_GUILE_HH

#include "config.hh"

#ifdef HAVE_LIBGUILE
extern "C" { 
#include <guile/gh.h> 
}
#else
typedef long SCM;
#endif
#endif // LILY_GUILE_HH
