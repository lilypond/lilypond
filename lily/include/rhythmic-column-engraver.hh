/*
  rhythmic-column-engraver.hh -- declare Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RHYTHMIC_COLUMN_GRAV_HH
#define RHYTHMIC_COLUMN_GRAV_HH

#include "engraver.hh"
#include "parray.hh"

class Rhythmic_column_engraver :public Engraver {
  Link_array<Script> script_l_arr_;
  Link_array<Rhythmic_head> rhead_l_arr_;
  Stem * stem_l_;
  Note_column *ncol_p_;
  Dot_column *dotcol_l_;

protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
public:
  Rhythmic_column_engraver();
  
};
#endif // RHYTHMIC_COLUMN_GRAV_HH



