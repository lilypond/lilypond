/*   
  dot-column-engraver.cc -- implement Dot_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "rhythmic-head.hh"
#include "dot-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"

class Dot_column_engraver : public Engraver
{
  Score_element *dotcol_p_ ;
  Link_array<Item> head_l_arr_;
public:
  VIRTUAL_COPY_CONS(Translator);
  Dot_column_engraver();
  
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing ();  
};


Dot_column_engraver::Dot_column_engraver ()
{
  dotcol_p_ =0;
}

void
Dot_column_engraver::do_pre_move_processing ()
{
  if (dotcol_p_)
    {
      typeset_element (dotcol_p_);
      dotcol_p_ =0;
    }
  head_l_arr_.clear ();
}

void
Dot_column_engraver::acknowledge_element (Score_element_info info)
{
  Score_element *d = unsmob_element (info.elem_l_->get_elt_property ("dot"));
  if (d)
    {
      if (!dotcol_p_)
	{
	  dotcol_p_ = new Item(get_property ("basicDotColumnProperties"));

	  Dot_column::set_interface (dotcol_p_);
	  Side_position::set_axis (dotcol_p_, X_AXIS);
	  Side_position::set_direction (dotcol_p_, RIGHT);      
	  announce_element (Score_element_info (dotcol_p_, 0));
	}

      Dot_column::add_head (dotcol_p_, info.elem_l_);
    }
}


ADD_THIS_TRANSLATOR(Dot_column_engraver);

