/*
  tuplet-engraver.cc -- implement Tuplet_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "tuplet-bracket.hh"
#include "note-column.hh"
#include "beam.hh"
#include "engraver.hh"
#include "spanner.hh"

#include "translator.icc"

struct Tuplet_description
{
  Music *music_;
  Rational stop_;
  Rational span_stop_;
  Rational length_;
  Spanner *bracket_;
  Spanner *number_;
  Tuplet_description ()
  {
    music_ = 0;
    bracket_ = 0;
    number_ = 0;
  }
  static int compare (Tuplet_description const &a, Tuplet_description const &b)
  {
    return ::compare (a.length_, b.length_);
  }
};

class Tuplet_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tuplet_engraver);

protected:
  std::vector<Tuplet_description> tuplets_;
  Link_array<Spanner> last_tuplets_;
  DECLARE_ACKNOWLEDGER (note_column);
  virtual bool try_music (Music *r);
  virtual void finalize ();
  void start_translation_timestep ();
  void process_music ();
};

bool
Tuplet_engraver::try_music (Music *music)
{
  if (music->is_mus_type ("time-scaled-music"))
    {
      Music *el = unsmob_music (music->get_property ("element"));
      if (el && !el->is_mus_type ("event-chord"))
	{
	  Tuplet_description d;
	  d.music_ = music;
	  d.length_ = music->get_length ().main_part_;
	  d.stop_ = now_mom ().main_part_ + d.length_;
	  d.span_stop_ = d.stop_;

	  SCM s = get_property ("tupletSpannerDuration");
	  if (unsmob_moment (s))
	    d.span_stop_ = min (d.span_stop_, (now_mom () + *unsmob_moment (s)).main_part_);

	  tuplets_.push_back (d);
	}
      return true;
    }
  return false;
}

void
Tuplet_engraver::process_music ()
{
  if (!tuplets_.size ())
    return;

  tuplets_.sort (&Tuplet_description::compare);
  for (vsize i = 0; i < tuplets_.size (); i++)
    {
      if (tuplets_[i].bracket_)
	continue;

      tuplets_[i].bracket_ = make_spanner ("TupletBracket",
					   tuplets_[i].music_->self_scm ());
      tuplets_[i].number_ = make_spanner ("TupletNumber",
					  tuplets_[i].music_->self_scm ());
      tuplets_[i].number_->set_object ("bracket", tuplets_[i].bracket_->self_scm ());
      tuplets_[i].bracket_->set_object ("tuplet-number", tuplets_[i].number_->self_scm ());
      
      if (i > 0 && tuplets_[i - 1].bracket_)
	Tuplet_bracket::add_tuplet_bracket (tuplets_[i].bracket_, tuplets_[i - 1].bracket_);

      if (i < tuplets_.size () - 1 && tuplets_[i + 1].bracket_)
	Tuplet_bracket::add_tuplet_bracket (tuplets_[i + 1].bracket_, tuplets_[i].bracket_);

      SCM proc = get_property ("tupletNumberFormatFunction");
      if (ly_is_procedure (proc))
	{
	  SCM t = scm_apply_0 (proc, scm_list_1 (tuplets_[i].music_->self_scm ()));
	  tuplets_[i].number_->set_property ("text", t);
	}
    }
}

void
Tuplet_engraver::acknowledge_note_column (Grob_info inf)
{
  for (vsize j = 0; j < tuplets_.size (); j++)
    if (tuplets_[j].bracket_)
      {
	Item *i = dynamic_cast<Item *> (inf.grob ());
	Tuplet_bracket::add_column (tuplets_[j].bracket_, i);
	add_bound_item (tuplets_[j].number_, i);
      }
}

void
Tuplet_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();

  last_tuplets_.clear ();
  if (tuplets_.empty ())
    return;

  Moment tsdmom = robust_scm2moment (get_property ("tupletSpannerDuration"), Moment (0));
  bool full_length = to_boolean (get_property ("tupletFullLength"));

  for (vsize i = tuplets_.size (); i--;)
    {
      Rational tsd = tsdmom.main_part_;

      if (now.main_part_ >= tuplets_[i].span_stop_)
	{
	  if (tuplets_[i].bracket_)
	    {
	      if (full_length)
		{
		  Item *col = unsmob_item (get_property ("currentMusicalColumn"));

		  tuplets_[i].bracket_->set_bound (RIGHT, col);
		  tuplets_[i].number_->set_bound (RIGHT, col);
		}
	      else if (!tuplets_[i].bracket_->get_bound (RIGHT))
		{
		  tuplets_[i].bracket_->set_bound (RIGHT,
						   tuplets_[i].bracket_->get_bound (LEFT));
		  tuplets_[i].number_->set_bound (RIGHT,
						  tuplets_[i].bracket_->get_bound (LEFT));
		}
	      last_tuplets_.push_back (tuplets_[i].bracket_);
	      last_tuplets_.push_back (tuplets_[i].number_);
	      
	      tuplets_[i].bracket_ = 0;
	      tuplets_[i].number_ = 0;
	    }

	  if (tsd)
	    tuplets_[i].span_stop_ += tsd;
	}

      if (now.main_part_ >= tuplets_[i].stop_)
	tuplets_.erase (tuplets_.begin () + i);
    }
}

void
Tuplet_engraver::finalize ()
{
  if (to_boolean (get_property ("tupletFullLength")))
    {
      for (vsize i = 0; i < last_tuplets_.size (); i++)
	{
	  Item *col = unsmob_item (get_property ("currentCommandColumn"));
	  last_tuplets_[i]->set_bound (RIGHT, col);
	}
    }
}

Tuplet_engraver::Tuplet_engraver ()
{
}

ADD_ACKNOWLEDGER (Tuplet_engraver, note_column);
ADD_TRANSLATOR (Tuplet_engraver,
		/* doc */ "Catch Time_scaled_music and generate appropriate bracket  ",
		/* create */ "TupletBracket TupletNumber",
		/* accept */ "time-scaled-music",
		/* read */ "tupletNumberFormatFunction tupletSpannerDuration tupletFullLength",
		/* write */ "");
