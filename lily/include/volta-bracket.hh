/*
  volta-spanner.hh -- part of GNU LilyPond

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef VOLTA_SPANNER_HH
#define VOLTA_SPANNER_HH

#include "spanner.hh"
#include "lily-proto.hh"

class Volta_bracket_interface
{
public:
  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static void modify_edge_height (Spanner *);
  static void add_bar (Grob *me, Item *bar);
};

#endif // VOLTA_SPANNER_HH

