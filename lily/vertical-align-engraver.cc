/*
  vertical-align-grav.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Spanner * valign_;
  bool qualifies_b (Grob_info) const;  
public:
  TRANSLATOR_DECLARATIONS(Vertical_align_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void initialize ();
  virtual void finalize ();
};

Vertical_align_engraver::Vertical_align_engraver ()
{
  valign_ =0;
}

void
Vertical_align_engraver::initialize ()
{
  valign_ =new Spanner (get_property ("VerticalAlignment"));
  valign_->set_bound (LEFT,unsmob_grob (get_property ("currentCommandColumn")));
  announce_grob(valign_ , SCM_EOL);
}

void
Vertical_align_engraver::finalize ()
{
  valign_->set_bound (RIGHT,unsmob_grob (get_property ("currentCommandColumn")));
  typeset_grob (valign_);
  valign_ =0;
}


bool
Vertical_align_engraver::qualifies_b (Grob_info i) const
{
  int sz = i.origin_transes ((Translator*)this).size ()  ;

  return sz > 1 && Axis_group_interface::has_interface (i.grob_)
    && !i.grob_->get_parent (Y_AXIS) && Axis_group_interface::axis_b (i.grob_, Y_AXIS);
}

void
Vertical_align_engraver::acknowledge_grob (Grob_info i)
{
  if (qualifies_b (i))
    {
      Align_interface::add_element (valign_,i.grob_, get_property ("verticalAlignmentChildCallback"));
    }
}


ENTER_DESCRIPTION(Vertical_align_engraver,
/* descr */       "Catch Vertical axis groups and stack them.",
/* creats*/       "VerticalAlignment",
/* accepts */     "",
/* acks  */      "axis-group-interface",
/* reads */       "",
/* write */       "");
