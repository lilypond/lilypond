/*
  span-arpeggio-engraver.cc -- implement Span_arpeggio_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "arpeggio.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"

/**
   Make arpeggios that span multiple staves.  Catch arpeggios, and span a
   Span_arpeggio over them if we find more than two arpeggios.
*/
class Span_arpeggio_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Span_arpeggio_engraver);
  DECLARE_ACKNOWLEDGER (arpeggio);

protected:
  void process_acknowledged ();
  void stop_translation_timestep ();

private:
  Item *span_arpeggio_;
  vector<Grob*> arpeggios_;
};

Span_arpeggio_engraver::Span_arpeggio_engraver ()
{
  span_arpeggio_ = 0;
}

void
Span_arpeggio_engraver::acknowledge_arpeggio (Grob_info info)
{
  if (info.origin_contexts (this).size ()) // huh? what's this test for? 
    arpeggios_.push_back (info.grob ());
}

void
Span_arpeggio_engraver::process_acknowledged ()
{
  /*
    connectArpeggios is slightly brusque; we should really read a grob
    property of the caught non-span arpeggios. That way, we can have

    both non-connected and connected arps in one pianostaff.

  */
  if (!span_arpeggio_ && arpeggios_.size () > 1
      && to_boolean (get_property ("connectArpeggios")))
    {
      span_arpeggio_ = make_item ("Arpeggio", SCM_EOL);
      span_arpeggio_->set_property ("cross-staff", SCM_BOOL_T);
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
      for (vsize j = 0; j < arpeggios_.size (); j++)
	{
	  extract_grob_set (arpeggios_[j], "stems", stems);
	  for (vsize i = 0; i < stems.size (); i++)
	    Pointer_group_interface::add_grob (span_arpeggio_, ly_symbol2scm ("stems"),
					       stems[i]);

	  extract_grob_set (arpeggios_[j], "side-support-elements", sses);
	  for (vsize i = 0; i < sses.size (); i++)
	    Pointer_group_interface::add_grob (span_arpeggio_, ly_symbol2scm ("side-support-elements"),
					       sses[i]);

	  /*
	    we can't kill the children, since we don't want to the
	    previous note to bump into the span arpeggio; so we make
	    it transparent.  */
	  arpeggios_[j]->set_property ("transparent", SCM_BOOL_T);
	}


      span_arpeggio_->set_parent (arpeggios_[0]->get_parent (Y_AXIS), Y_AXIS);
      span_arpeggio_ = 0;
    }
  arpeggios_.clear ();
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Span_arpeggio_engraver, arpeggio);
ADD_TRANSLATOR (Span_arpeggio_engraver,
		/* doc */
		"Make arpeggios that span multiple staves.",

		/* create */
		"Arpeggio ",

		/* read */
		"connectArpeggios ",

		/* write */
		""
		);
