/*
  dot-column.hh -- declare Dot_column Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOT_COLUMN_HH
#define DOT_COLUMN_HH

#include "item.hh"


/**
  Group dots.  This is needed because, the dots have to be aligned per voice
 */
class Dot_column : public Item
{
  static int compare (Dots * const&,Dots * const&);
  void add_dots (Dots*);
public:
  VIRTUAL_COPY_CONS (Score_element);
  void add_head (Rhythmic_head*);
  Dot_column (SCM);


  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
};
#endif // DOT_COLUMN_HH
