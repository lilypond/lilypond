/*   
  dot-column-engraver.cc -- implement Dot_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "rhythmic-head.hh"
#include "dot-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"

class Dot_column_engraver : public Engraver
{
  Grob *dotcol_p_ ;
  Link_array<Item> head_l_arr_;
public:
  VIRTUAL_COPY_CONS (Translator);
  Dot_column_engraver ();
  
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();  
};


Dot_column_engraver::Dot_column_engraver ()
{
  dotcol_p_ =0;
}

void
Dot_column_engraver::stop_translation_timestep ()
{
  if (dotcol_p_)
    {
      typeset_grob (dotcol_p_);
      dotcol_p_ =0;
    }
  head_l_arr_.clear ();
}

void
Dot_column_engraver::acknowledge_grob (Grob_info info)
{
  Grob *d = unsmob_grob (info.elem_l_->get_grob_property ("dot"));
  if (d)
    {
      if (!dotcol_p_)
	{
	  dotcol_p_ = new Item (get_property ("DotColumn"));

	  Dot_column::set_interface (dotcol_p_);
	  Side_position_interface::set_axis (dotcol_p_, X_AXIS);
	  Side_position_interface::set_direction (dotcol_p_, RIGHT);      
	  announce_grob (dotcol_p_, 0);
	}

      Dot_column::add_head (dotcol_p_, info.elem_l_);
    }
}


ADD_THIS_TRANSLATOR (Dot_column_engraver);

