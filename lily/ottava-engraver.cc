/*
  text-spanner-engraver.cc -- implement Ottava_spanner_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2004 Han-Wen Nienhuys
*/

#include "protected-scm.hh"
#include "note-column.hh"
#include "item.hh"
#include "side-position-interface.hh"
#include "engraver.hh"

class Ottava_spanner_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Ottava_spanner_engraver);  
protected:
  virtual void finalize ();
  virtual void acknowledge_grob (Grob_info);
  virtual void process_music ();
  virtual void stop_translation_timestep ();
private:
  Spanner *span_;
  Spanner *finished_;
  
  Protected_scm last_ottavation_;
  
  void typeset_all ();
};


Ottava_spanner_engraver::Ottava_spanner_engraver ()
{
  finished_ = 0;
  span_ =0;
  last_ottavation_ = SCM_EOL;
}

void
Ottava_spanner_engraver::process_music ()
{
  SCM ott = get_property ("ottavation");
  if (ott != last_ottavation_)
    {
      finished_= span_;
      span_ = 0;
      if (ly_string_p (ott))
	{
	  span_  = make_spanner ("OttavaBracket");
	  span_->set_property ("text", ott);
	  announce_grob (span_, SCM_EOL);

	  SCM c0 (get_property ("middleCPosition"));
	  SCM oc0 (get_property ("originalCentralCPosition"));
	  if (scm_less_p (oc0, c0) == SCM_BOOL_T)
	    span_->set_property ("direction", scm_int2num (DOWN));
	}
    }
  last_ottavation_ = ott;
}

void
Ottava_spanner_engraver::acknowledge_grob (Grob_info info)
{
  Item *it = dynamic_cast<Item*> (info.grob_);
  if (span_ && it && Note_column::has_interface (info.grob_))
    {
      Side_position_interface::add_support (span_, it);

      if (!span_->get_bound (LEFT))
	span_->set_bound (LEFT, it);
      span_->set_bound (RIGHT, it);
    }
}

void
Ottava_spanner_engraver::typeset_all ()
{  
  if (finished_)
    {
      Direction d = LEFT;
      do
	{
	  if (!finished_->get_bound (RIGHT))
	    {
	      Grob* e = unsmob_grob (get_property ("currentMusicalColumn"));
	      finished_->set_bound (d, e);
	    }
	}
      while (flip (&d) != LEFT);
      
      typeset_grob (finished_);
      finished_ = 0;
    }
}

void
Ottava_spanner_engraver::stop_translation_timestep ()
{
  if (span_ && !span_->get_bound (LEFT))
    {
      Grob* e = unsmob_grob (get_property ("currentMusicalColumn"));
      span_->set_bound (LEFT, e);
    }

  typeset_all ();
}

void
Ottava_spanner_engraver::finalize ()
{
  typeset_all ();
  if (span_)
    finished_ = span_;
  typeset_all ();
  last_ottavation_ = SCM_EOL;
}

ENTER_DESCRIPTION (Ottava_spanner_engraver,
/* descr */       "Create a text spanner when the ottavation property changes..",
/* creats*/       "OttavaBracket",
/* accepts */     "",
/* acks  */      "note-column-interface",
/* reads */       "ottavation",
/* write */       "");
