/*
  bar-column-grav.hh -- declare Bar_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef BAR_COLUMN_GRAV_HH
#define BAR_COLUMN_GRAV_HH

#include "engraver.hh"
#include "parray.hh"

/// couple bars and appropriate scripts
class Bar_column_engraver :public Engraver {
  Bar_column *barcol_p_;
  Link_array<Script>  script_l_arr_;

  Bar *bar_l_;
    
protected:
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
public:
  TRANSLATOR_CLONE(Bar_column_engraver);
  Bar_column_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // BAR_COLUMN_GRAV_HH
