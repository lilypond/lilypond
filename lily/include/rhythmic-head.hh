/*
  rhythmic-head.hh -- declare Rhythmic_head

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RHYTHMIC_HEAD_HH
#define RHYTHMIC_HEAD_HH

#include "item.hh"

/*
  Why don't I have a 
  VIRTUAL_COPY_CONS (Score_element);
  see also note-head, rest.
  ?
*/

class Rhythmic_head : public Item
{
public:
  int balltype_i () const;

  void add_dots (Dots *);
  Stem * stem_l () const;
  Dots * dots_l () const;
  int dot_count () const;
protected:
  virtual void after_line_breaking ();
};

#endif // RHYTHMIC_HEAD_HH
