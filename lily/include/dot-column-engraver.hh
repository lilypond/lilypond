/*
  dot-column-engraver.hh -- declare Dot_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DOT_COLUMN_GRAV_HH
#define DOT_COLUMN_GRAV_HH

#include "engraver.hh"

class Dot_column_engraver : public Engraver
{
  Dot_column *dotcol_p_ ;
  Link_array<Rhythmic_head> head_l_arr_;
public:
  TRANSLATOR_CLONE(Dot_column_engraver);
  Dot_column_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();  
};

#endif // DOT_COLUMN_GRAV_HH
