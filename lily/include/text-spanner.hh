/*
  text-spanner.hh -- declare Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef TEXT_SPANNER_HH
#define TEXT_SPANNER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

class Text_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_GROB_INTERFACE();
};

#endif /* TEXT_SPANNER_HH */
