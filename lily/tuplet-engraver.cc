/*   
  tuplet-engraver.cc --  implement Tuplet_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "tuplet-bracket.hh"
#include "note-column.hh"
#include "time-scaled-music.hh"
#include "beam.hh"
#include "music-list.hh"
#include "engraver.hh"
#include "spanner.hh"

struct Tuplet_description
{
  Music *music_;
  Rational stop_;
  Rational span_stop_;
  Spanner *spanner_;
  Tuplet_description()
  {
    music_ = 0;
    spanner_ = 0;
  }
};

class Tuplet_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tuplet_engraver);

protected:
  Array<Tuplet_description> tuplets_;

  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*r);
  virtual void start_translation_timestep ();
  virtual void process_music ();
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
	  d.stop_  = now_mom ().main_part_ + music->get_length ().main_part_;
	  d.span_stop_ = d.stop_;
	  
	  SCM s = get_property ("tupletSpannerDuration");
	  if (unsmob_moment (s))
	    d.span_stop_ = d.span_stop_ <? (now_mom () + *unsmob_moment (s)).main_part_;
	  
	  tuplets_.push (d);
	}
      return true;
    }
  return false;
}

void
Tuplet_engraver::process_music ()
{
  for (int i= 0; i < tuplets_.size (); i++)
    {
      if (tuplets_[i].spanner_)
	continue;

      Spanner* spanner = make_spanner ("TupletBracket",
				       tuplets_[i].music_->self_scm ());
      tuplets_[i].spanner_ = spanner;

      SCM proc = get_property ("tupletNumberFormatFunction");
      if (ly_c_procedure_p (proc))
	{
	  SCM t = scm_apply_0 (proc, scm_list_1 (tuplets_[i].music_->self_scm ()));
	  spanner->set_property ("text", t);
	}
    }
}

void
Tuplet_engraver::acknowledge_grob (Grob_info i)
{
  if (Note_column::has_interface (i.grob_))
    {
      for (int j =0; j < tuplets_.size (); j++)
	if (tuplets_[j].spanner_) 
	  Tuplet_bracket::add_column (tuplets_[j].spanner_,
				      dynamic_cast<Item*> (i.grob_));
    }
}

void
Tuplet_engraver::start_translation_timestep ()
{
  Moment now = now_mom ();

  Moment tsd;
  SCM s = get_property ("tupletSpannerDuration");
  if (unsmob_moment (s))
    tsd = unsmob_moment (s)->main_part_;

  for (int i = tuplets_.size (); i--;)
    {
      if (now.main_part_ >= tuplets_[i].span_stop_)
	{
	  if (Spanner *sp = tuplets_[i].spanner_)
	    {
	      if (!sp->get_bound (RIGHT))
		sp->set_bound (RIGHT, sp->get_bound (LEFT));
	      
	      tuplets_[i].spanner_ = 0;
	    }
	  
	  if (tsd.to_bool ())
	    tuplets_[i].span_stop_ += tsd.main_part_;
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

ENTER_DESCRIPTION (Tuplet_engraver,
/* descr */       "Catch Time_scaled_music and generate appropriate bracket  ",
/* creats*/       "TupletBracket",
/* accepts */     "time-scaled-music",
/* acks  */      "note-column-interface",
/* reads */       "tupletNumberFormatFunction tupletSpannerDuration",
/* write */       "");
