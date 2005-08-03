/*
  tuplet-engraver.cc -- implement Tuplet_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Spanner *spanner_;
  Tuplet_description ()
  {
    music_ = 0;
    spanner_ = 0;
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
  Array<Tuplet_description> tuplets_;

  DECLARE_ACKNOWLEDGER (note_column);
  virtual bool try_music (Music *r);
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

	  tuplets_.push (d);
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
  for (int i = 0; i < tuplets_.size (); i++)
    {
      if (tuplets_[i].spanner_)
	continue;

      Spanner *spanner = make_spanner ("TupletBracket",
				       tuplets_[i].music_->self_scm ());
      tuplets_[i].spanner_ = spanner;

      if (i > 0 && tuplets_[i-1].spanner_)
	Tuplet_bracket::add_tuplet_bracket (tuplets_[i].spanner_, tuplets_[i-1].spanner_);
      if (i < tuplets_.size()-1 && tuplets_[i+1].spanner_)
	Tuplet_bracket::add_tuplet_bracket (tuplets_[i+1].spanner_, tuplets_[i].spanner_);

      SCM proc = get_property ("tupletNumberFormatFunction");
      if (ly_is_procedure (proc))
	{
	  SCM t = scm_apply_0 (proc, scm_list_1 (tuplets_[i].music_->self_scm ()));
	  spanner->set_property ("text", t);
	}
    }
}

void
Tuplet_engraver::acknowledge_note_column (Grob_info i)
{
  for (int j = 0; j < tuplets_.size (); j++)
    if (tuplets_[j].spanner_)
      Tuplet_bracket::add_column (tuplets_[j].spanner_,
				  dynamic_cast<Item *> (i.grob ()));
}

void
Tuplet_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();

  for (int i = tuplets_.size (); i--;)
    {
      Moment tsdmom = robust_scm2moment (get_property ("tupletSpannerDuration"), Moment (0));

      Rational tsd = tsdmom.main_part_;

      if (now.main_part_ >= tuplets_[i].span_stop_)
	{
	  if (Spanner *sp = tuplets_[i].spanner_)
	    {
	      if (!sp->get_bound (RIGHT))
		sp->set_bound (RIGHT, sp->get_bound (LEFT));

	      tuplets_[i].spanner_ = 0;
	    }

	  if (tsd)
	    tuplets_[i].span_stop_ += tsd;
	}

      if (now.main_part_ >= tuplets_[i].stop_)
	{
	  tuplets_.del (i);
	}
    }
}

Tuplet_engraver::Tuplet_engraver ()
{
}

ADD_ACKNOWLEDGER (Tuplet_engraver,note_column);
ADD_TRANSLATOR (Tuplet_engraver,
		/* descr */ "Catch Time_scaled_music and generate appropriate bracket  ",
		/* creats*/ "TupletBracket",
		/* accepts */ "time-scaled-music",
		/* reads */ "tupletNumberFormatFunction tupletSpannerDuration",
		/* write */ "");
