/*
  text-spanner.hh -- declare Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef TEXT_SPANNER_HH
#define TEXT_SPANNER_HH

#include "lily-guile.hh"

class Grob;

class Text_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob *);
};

#endif /* TEXT_SPANNER_HH */
