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
  static int compare (Score_element * const&,Score_element * const&);
public:
  VIRTUAL_COPY_CONS (Score_element);
  void add_head (Rhythmic_head*);
  Dot_column (SCM);
  
  static Real force_shift_callback (Score_element const* , Axis);
  static SCM do_shifts (SCM dotlist);
};
#endif // DOT_COLUMN_HH
