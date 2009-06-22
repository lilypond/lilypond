/*
  measure-grouping-engraver.cc -- implement Measure_grouping_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "warn.hh"
#include "side-position-interface.hh"
#include "global-context.hh"
#include "engraver.hh"
#include "spanner.hh"
#include "beam-settings.hh"

#include "translator.icc"

class Measure_grouping_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Measure_grouping_engraver);

protected:
  Spanner *grouping_;
  Rational stop_grouping_mom_;

  void process_music ();
  virtual void finalize ();
  DECLARE_ACKNOWLEDGER (note_column);
};

void
Measure_grouping_engraver::finalize ()
{
  if (grouping_)
    {
      grouping_->set_bound (RIGHT, unsmob_grob (get_property ("currentCommandColumn")));
      grouping_->suicide ();
      grouping_ = 0;
    }
}

void
Measure_grouping_engraver::acknowledge_note_column (Grob_info gi)
{
  if (grouping_)
    Side_position_interface::add_support (grouping_, gi.grob ());
}

void
Measure_grouping_engraver::process_music ()
{
  Moment now = now_mom ();
  if (grouping_ && now.main_part_ >= stop_grouping_mom_ && !now.grace_part_)
    {
      grouping_->set_bound (RIGHT,
			    unsmob_grob (get_property ("currentMusicalColumn")));

      grouping_ = 0;
    }

  if (now.grace_part_)
    return;

  SCM settings = get_property ("beamSettings");
  SCM grouping = SCM_EOL;
  if (scm_is_pair (settings))
    {
      SCM time_signature_fraction = get_property ("timeSignatureFraction");
      grouping = ly_beam_grouping (settings,
                                   time_signature_fraction,
                                   ly_symbol2scm ("end"),
                                   ly_symbol2scm ("*"));
    }
  if (scm_is_pair (grouping))
    {
      Moment *measpos = unsmob_moment (get_property ("measurePosition"));
      Rational mp = measpos->main_part_;

      Moment *beatlen_mom = unsmob_moment (get_property ("beatLength"));
      Rational beat_length = beatlen_mom->main_part_;

      Rational where (0);
      for (SCM s = grouping; scm_is_pair (s);
	   where += Rational ((int) scm_to_int (scm_car (s))) * beat_length,
	     s = scm_cdr (s))
	{
	  int grouplen = scm_to_int (scm_car (s));
	  if (where == mp)
	    {
	      if (grouping_)
		{
		  programming_error ("last grouping not finished yet");
		  continue;
		}
              if (grouplen > 1)
               {
	         grouping_ = make_spanner ("MeasureGrouping", SCM_EOL);
	         grouping_->set_bound (LEFT, unsmob_grob (get_property ("currentMusicalColumn")));

	         stop_grouping_mom_ = now.main_part_ + Rational (grouplen - 1) * beat_length;
	         get_global_context ()->add_moment_to_process (Moment (stop_grouping_mom_));

	         if (grouplen == 3)
		   grouping_->set_property ("style", ly_symbol2scm ("triangle"));
	         else
		   grouping_->set_property ("style", ly_symbol2scm ("bracket"));

	         break;
               }
	    }
	}
    }
}

Measure_grouping_engraver::Measure_grouping_engraver ()
{
  grouping_ = 0;
}

ADD_ACKNOWLEDGER (Measure_grouping_engraver, note_column);
ADD_TRANSLATOR (Measure_grouping_engraver,
		/* doc */
		"Create @code{MeasureGrouping} to indicate beat subdivision.",

		/* create */
		"MeasureGrouping ",

		/* read */
		"beatLength "
		"currentMusicalColumn "
		"measurePosition "
		"beamSettings ",

		/* write */
		""
		);
