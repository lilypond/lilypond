/*
  rhythmic-column-grav.hh -- declare Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef RHYTHMIC_COLUMN_GRAV_HH
#define RHYTHMIC_COLUMN_GRAV_HH

#include "engraver.hh"
#include "parray.hh"

class Rhythmic_column_engraver :public Engraver {
  Link_array<Script> script_l_arr_;
  Stem * stem_l_;
  Note_column *ncol_p_;
  Dot_column *dotcol_p_;
protected:
  TRANSLATOR_CLONE(Rhythmic_column_engraver);
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
public:
  Rhythmic_column_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // RHYTHMIC_COLUMN_GRAV_HH



