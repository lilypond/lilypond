/*
  rhythmic-head.hh -- declare Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/*
  Properties
  
  duration-log -- 2-log of the notehead duration

  dot -- reference to Dots object.

*/
class Rhythmic_head
{
public:
  static int balltype_i (Score_element*) ;
  static void set_dots (Score_element*,Item *);
  static Item * stem_l (Score_element*) ;
  static Item * dots_l (Score_element*) ;
  static int dot_count (Score_element*) ;
  DECLARE_SCHEME_CALLBACK(after_line_breaking, (SCM ));
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);
};

#endif // RHYTHMIC_HEAD_HH
