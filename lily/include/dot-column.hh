/*
  dot-column.hh -- declare Dot_column Dot_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOT_COLUMN_HH
#define DOT_COLUMN_HH

#include "horizontal-group-item.hh"

/**
  Group dots.  This is needed because, the dots have to be aligned per voice
 */
class Dot_column : public Horizontal_group_item
{
  Link_array<Rhythmic_head> head_l_arr_;
  Link_array<Dots> dot_l_arr_;

public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  void add_head (Rhythmic_head*);
  void add_dots (Dots*);

protected:
  virtual void do_pre_processing ();
  virtual void do_substitute_dependency (Score_element *o, Score_element*n);
};
#endif // DOT_COLUMN_HH
