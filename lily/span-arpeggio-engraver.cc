/*
  span-arpeggio-engraver.cc -- implement Span_arpeggio_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"
#include "lily-guile.hh"
#include "item.hh"
#include "arpeggio.hh"
#include "group-interface.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"


/** 
  Make arpeggios that span multiple staffs.  Catch arpeggios, and span a
  Span_arpeggio over them if we find more than two arpeggios.
  */
class Span_arpeggio_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Span_arpeggio_engraver ();

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();

private:
  Item *span_arpeggio_;
  Link_array<Grob> arpeggios_;
};


Span_arpeggio_engraver::Span_arpeggio_engraver ()
{
  span_arpeggio_ = 0;
}

void
Span_arpeggio_engraver::acknowledge_grob (Grob_info info)
{
    if (info.origin_trans_l_arr (this).size ()
        && Arpeggio::has_interface (info.elem_l_))
    {
      arpeggios_.push (info.elem_l_);
    }
}

void
Span_arpeggio_engraver::create_grobs ()
{
  /*
    connectArpeggios is slightly brusque; we should really read a elt
    property of the caught non-span arpeggios. That way, we can have

    both non-connected and connected arps in one pianostaff.
    

  */
  if (!span_arpeggio_ && arpeggios_.size () > 1
      && to_boolean (get_property ("connectArpeggios")))
    {
      span_arpeggio_ = new Item (get_property ("Arpeggio"));
      announce_grob (span_arpeggio_, 0);      
    }
}

void
Span_arpeggio_engraver::stop_translation_timestep ()
{
  if (span_arpeggio_) 
    {
      /*
	we do this very late, to make sure we also catch `extra'
	side-pos support like accidentals.
       */
      for (int i=0; i < arpeggios_.size (); i ++)
	{
	  for (SCM s = arpeggios_[i]->get_grob_property ("stems");
	       gh_pair_p (s); s = gh_cdr (s))
	    Group_interface::add_thing (span_arpeggio_, "stems", gh_car (s));
	  for (SCM s = arpeggios_[i]->get_grob_property ("side-support-elements");
	       gh_pair_p (s); s = gh_cdr (s))
	    Group_interface::add_thing (span_arpeggio_, "side-support-elements", gh_car (s));

	  /*
	    we can't kill the children, since we don't want to the
	    previous note to bump into the span arpeggio; so we make
	    it transparent.  */
	  arpeggios_[i]->set_grob_property ("molecule-callback", SCM_BOOL_T);
	}
      
      typeset_grob (span_arpeggio_);
      span_arpeggio_ = 0;
    }
  arpeggios_.clear ();
}

ADD_THIS_TRANSLATOR (Span_arpeggio_engraver);

