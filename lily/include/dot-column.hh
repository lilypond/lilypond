/*
  dot-column.hh -- declare Dot_column Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOT_COLUMN_HH
#define DOT_COLUMN_HH

#include "axis-group-item.hh"
#include "note-head-side.hh"

/**
  Group dots.  This is needed because, the dots have to be aligned per voice
 */
class Dot_column : public Axis_group_item, public Note_head_side
{
  Link_array<Dots> dot_l_arr_;
  static int compare (Dots * const&,Dots * const&);
public:
  VIRTUAL_COPY_CONS (Score_element);
  void add_head (Rhythmic_head*);
  void add_dots (Dots*);
  Dot_column ();

protected:

  virtual void do_pre_processing ();
  virtual void do_post_processing ();
  virtual void do_substitute_element_pointer (Score_element *o, Score_element*n);
};
#endif // DOT_COLUMN_HH
