/*
  rhythmic-head.hh -- declare Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

class Rhythmic_head
{
public:
  static int balltype_i (Grob*) ;
  static void set_dots (Grob*,Item *);
  static Item * stem_l (Grob*) ;
  static Item * dots_l (Grob*) ;
  static int dot_count (Grob*) ;
  DECLARE_SCHEME_CALLBACK(after_line_breaking, (SCM ));
  static bool has_interface (Grob*);
  static void set_interface (Grob*);
};

#endif // RHYTHMIC_HEAD_HH
