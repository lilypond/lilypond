/*
  vertical-align-grav.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "translator-group.hh"
#include "paper-column.hh"
#include "align-interface.hh"
#include "span-bar.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "spanner.hh"

class Vertical_align_engraver : public Engraver
{
  Spanner * valign_p_;
  bool qualifies_b (Grob_info) const;  
public:
  VIRTUAL_COPY_CONS(Translator);
  Vertical_align_engraver();
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void do_creation_processing();
  virtual void do_removal_processing();
};

Vertical_align_engraver::Vertical_align_engraver()
{
  valign_p_ =0;
}

void
Vertical_align_engraver::do_creation_processing()
{
  valign_p_ =new Spanner (get_property ("VerticalAlignment"));
  valign_p_->set_bound(LEFT,unsmob_element (get_property ("currentCommandColumn")));
  announce_grob (valign_p_ , 0);
}

void
Vertical_align_engraver::do_removal_processing()
{
  valign_p_->set_bound(RIGHT,unsmob_element (get_property ("currentCommandColumn")));
  typeset_grob (valign_p_);
  valign_p_ =0;
}


bool
Vertical_align_engraver::qualifies_b (Grob_info i) const
{
  int sz = i.origin_trans_l_arr ((Translator*)this).size()  ;

  return sz > 1 && Axis_group_interface::has_interface (i.elem_l_)
    && !i.elem_l_->parent_l (Y_AXIS) && Axis_group_interface::axis_b (i.elem_l_, Y_AXIS);
}

void
Vertical_align_engraver::acknowledge_grob (Grob_info i)
{
  if (qualifies_b (i))
    {
      Align_interface::add_element (valign_p_,i.elem_l_);
    }
}

ADD_THIS_TRANSLATOR(Vertical_align_engraver);
