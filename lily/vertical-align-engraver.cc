/*
  vertical-align-grav.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "context.hh"
#include "paper-column.hh"
#include "align-interface.hh"
#include "span-bar.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "spanner.hh"

class Vertical_align_engraver : public Engraver
{
  Spanner * valign_;
  bool qualifies (Grob_info) const;  
public:
  TRANSLATOR_DECLARATIONS (Vertical_align_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void process_music ();
  virtual void finalize ();
};

Vertical_align_engraver::Vertical_align_engraver ()
{
  valign_ = 0;
}

void
Vertical_align_engraver::process_music ()
{
  if (!valign_)
    {
      valign_ = make_spanner ("VerticalAlignment", SCM_EOL);
      valign_->set_bound (LEFT,unsmob_grob (get_property ("currentCommandColumn")));
    }
}

void
Vertical_align_engraver::finalize ()
{
  if (valign_)
    {
      valign_->set_bound (RIGHT,unsmob_grob (get_property ("currentCommandColumn")));
      valign_ = 0;
    }
}

bool
Vertical_align_engraver::qualifies (Grob_info i) const
{
  int sz = i.origin_contexts ((Translator*)this).size ()  ;

  return sz > 0 && Axis_group_interface::has_interface (i.grob_)
    && !i.grob_->get_parent (Y_AXIS) && Axis_group_interface::has_axis (i.grob_, Y_AXIS);
}

void
Vertical_align_engraver::acknowledge_grob (Grob_info i)
{
  if (qualifies (i))
    {
      Align_interface::add_element (valign_,i.grob_, get_property ("verticalAlignmentChildCallback"));
    }
}


ADD_TRANSLATOR (Vertical_align_engraver,
/* descr */       "Catch Vertical axis groups and stack them.",
/* creats*/       "VerticalAlignment",
/* accepts */     "",
/* acks  */      "axis-group-interface",
/* reads */       "",
/* write */       "");
