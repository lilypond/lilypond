/*
  rhythmic-head.hh -- declare Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "item.hh"

/*
  Properties
  
  duration-log -- 2-log of the notehead duration

  dot -- reference to Dots object.

*/
class Rhythmic_head : public Item
{
public:

  /*
    Typically not used, since Rhythmic_head is not breakable.
   */
  VIRTUAL_COPY_CONS(Rhythmic_head);
  int balltype_i () const;

  void add_dots (Item *);
  Stem * stem_l () const;
  Item * dots_l () const;
  int dot_count () const;

  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
  Rhythmic_head (SCM s);
};

#endif // RHYTHMIC_HEAD_HH
