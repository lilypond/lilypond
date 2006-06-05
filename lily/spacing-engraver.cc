/*
  spacing-engraver.cc -- implement Spacing_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-column.hh"
#include "engraver.hh"
#include "pqueue.hh"
#include "note-spacing.hh"
#include "staff-spacing.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"

#include "translator.icc"

struct Rhythmic_tuple
{
  Grob_info info_;
  Moment end_;

  Rhythmic_tuple ()
  {
  }
  Rhythmic_tuple (Grob_info i, Moment m)
  {
    info_ = i;
    end_ = m;
  }
  static int time_compare (Rhythmic_tuple const &, Rhythmic_tuple const &);
};

inline int
compare (Rhythmic_tuple const &a, Rhythmic_tuple const &b)
{
  return Rhythmic_tuple::time_compare (a, b);
}

int
Rhythmic_tuple::time_compare (Rhythmic_tuple const &h1,
			      Rhythmic_tuple const &h2)
{
  return (h1.end_ - h2.end_).main_part_.sign ();
}

/****************************************************************/

/*
  Acknowledge rhythmic elements, for initializing spacing fields in
  the columns.
*/
class Spacing_engraver : public Engraver
{
  PQueue<Rhythmic_tuple> playing_durations_;
  vector<Rhythmic_tuple> now_durations_;
  vector<Rhythmic_tuple> stopped_durations_;
  Moment now_;
  Spanner *spacing_;
  Music *start_section_;
  
  TRANSLATOR_DECLARATIONS (Spacing_engraver);

protected:
  DECLARE_ACKNOWLEDGER (staff_spacing);
  DECLARE_ACKNOWLEDGER (note_spacing);
  DECLARE_ACKNOWLEDGER (rhythmic_head);

  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
  
  virtual void finalize ();
  virtual bool try_music (Music *m);

  void start_spanner ();
  void stop_spanner ();
};

Spacing_engraver::Spacing_engraver ()
{
  spacing_ = 0;
  start_section_ = 0;
}

bool
Spacing_engraver::try_music (Music *m)
{
  start_section_ = m;
  return true;  
}

void
Spacing_engraver::process_music ()
{
  if (start_section_ && spacing_)
    stop_spanner ();
  
  if (!spacing_)
    start_spanner ();
}

void
Spacing_engraver::start_spanner ()
{
  assert (!spacing_);

  spacing_ = make_spanner ("SpacingSpanner", SCM_EOL);
  spacing_->set_bound (LEFT,
		       unsmob_grob (get_property ("currentCommandColumn")));
}

void
Spacing_engraver::finalize ()
{
  stop_spanner ();
}

void
Spacing_engraver::stop_spanner ()
{
  if (spacing_)
    {
      Grob *p = unsmob_grob (get_property ("currentCommandColumn"));

      spacing_->set_bound (RIGHT, p);
      spacing_ = 0;
    }
}

void
Spacing_engraver::acknowledge_note_spacing (Grob_info i)
{
  Pointer_group_interface::add_grob (spacing_, ly_symbol2scm ("wishes"), i.grob ());
}

void
Spacing_engraver::acknowledge_staff_spacing (Grob_info i)
{
  Pointer_group_interface::add_grob (spacing_, ly_symbol2scm ("wishes"), i.grob ());
}

void
Spacing_engraver::acknowledge_rhythmic_head (Grob_info i)
{
  if (i.grob ()->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface"))
      || i.grob ()->internal_has_interface (ly_symbol2scm ("multi-measure-interface")))
    return;

  /*
    only pay attention to durations that are not grace notes.
  */
  if (!now_.grace_part_)
    {
      Music *r = i.music_cause ();
      if (r && r->is_mus_type ("rhythmic-event"))
	{
	  Moment len = r->get_length ();
	  Rhythmic_tuple t (i, now_mom () + len);
	  now_durations_.push_back (t);
	}
    }
}

void
Spacing_engraver::stop_translation_timestep ()
{
  Paper_column *musical_column
    = dynamic_cast<Paper_column *> (unsmob_grob (get_property ("currentMusicalColumn")));

  SCM proportional = get_property ("proportionalNotationDuration");
  if (unsmob_moment (proportional))
    {
      musical_column->set_property ("shortest-playing-duration", proportional);
      musical_column->set_property ("shortest-starter-duration", proportional);
      return;
    }

  Moment shortest_playing;
  shortest_playing.set_infinite (1);
  for (vsize i = 0; i < playing_durations_.size (); i++)
    {
      Music *mus = playing_durations_[i].info_.music_cause ();
      if (mus)
	{
	  Moment m = mus->get_length ();
	  shortest_playing = min (shortest_playing, m);
	}
    }
  Moment starter;
  starter.set_infinite (1);

  for (vsize i = 0; i < now_durations_.size (); i++)
    {
      Moment m = now_durations_[i].info_.music_cause ()->get_length ();
      if (m.to_bool ())
	{
	  starter = min (starter, m);
	  playing_durations_.insert (now_durations_[i]);
	}
    }
  now_durations_.clear ();

  shortest_playing = min (shortest_playing, starter);

  assert (starter.to_bool ());
  SCM sh = shortest_playing.smobbed_copy ();
  SCM st = starter.smobbed_copy ();

  musical_column->set_property ("shortest-playing-duration", sh);
  musical_column->set_property ("shortest-starter-duration", st);
}

void
Spacing_engraver::start_translation_timestep ()
{
  start_section_ = 0;

  now_ = now_mom ();
  stopped_durations_.clear ();
  
  while (playing_durations_.size () && playing_durations_.front ().end_ < now_)
    playing_durations_.delmin ();
  while (playing_durations_.size () && playing_durations_.front ().end_ == now_)
    stopped_durations_.push_back (playing_durations_.get ());
}

ADD_ACKNOWLEDGER (Spacing_engraver, staff_spacing);
ADD_ACKNOWLEDGER (Spacing_engraver, note_spacing);
ADD_ACKNOWLEDGER (Spacing_engraver, rhythmic_head);

ADD_TRANSLATOR (Spacing_engraver,
		"make a SpacingSpanner and do "
		"bookkeeping of shortest starting and playing notes  ",

		/* create */ "SpacingSpanner",
		/* accept */
		"spacing-section-event ",
		/* read */
		"currentMusicalColumn "
		"currentCommandColumn "
		"proportionalNotationDuration",
		
		/* write */ "");
