/*
  bar-column-engraver.hh -- declare Bar_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BAR_COLUMN_GRAV_HH
#define BAR_COLUMN_GRAV_HH

#include "engraver.hh"
#include "parray.hh"

/// couple bars and appropriate scripts
class Bar_column_engraver :public Engraver {
  Bar_column *barcol_p_;
  Link_array<Script>  script_l_arr_;
  int break_priority_i_;

  Bar *bar_l_;
  void create_column ();
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing ();
  virtual void do_creation_processing ();
  virtual void do_process_requests ();
  virtual void do_post_move_processing();
public:
  VIRTUAL_COPY_CONS(Translator);
  Bar_column_engraver();
  
};

#endif // BAR_COLUMN_GRAV_HH
