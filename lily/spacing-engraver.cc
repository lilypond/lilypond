/*   
  spacing-engraver.cc --  implement Spacing_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "request.hh"
#include "paper-column.hh"
#include "engraver.hh"
#include "pqueue.hh"
#include "note-spacing.hh"
#include "staff-spacing.hh"
#include "group-interface.hh"
#include "spanner.hh"

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

/**
   Acknowledge rhythmic elements, for initializing spacing fields in
   the columns.

   should be the  last one of the toplevel context
*/
class Spacing_engraver : public Engraver
{
  PQueue<Rhythmic_tuple> playing_durations_;
  Array<Rhythmic_tuple> now_durations_;
  Array<Rhythmic_tuple> stopped_durations_;
  Moment now_;
  Spanner * spacing_;
  
  TRANSLATOR_DECLARATIONS(Spacing_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void start_translation_timestep ();
  virtual void stop_translation_timestep ();
  virtual void initialize ();
  virtual void finalize ();
};

inline int
compare (Rhythmic_tuple const &a, Rhythmic_tuple const &b)
{
  return Rhythmic_tuple::time_compare (a,b);
}

int
Rhythmic_tuple::time_compare (Rhythmic_tuple const &h1,
			      Rhythmic_tuple const &h2)
{
  return (h1.end_ - h2.end_).main_part_.sign ();
}

Spacing_engraver::Spacing_engraver ()
{
  spacing_ = 0;
}

void
Spacing_engraver::initialize ()
{
  spacing_  =new Spanner (get_property ("SpacingSpanner"));
  spacing_->set_bound (LEFT, unsmob_grob (get_property ("currentCommandColumn")));  
  announce_grob(spacing_, SCM_EOL);
}

void
Spacing_engraver::finalize ()
{
  Grob * p = unsmob_grob (get_property ("currentCommandColumn"));
  spacing_->set_bound (RIGHT, p);
  typeset_grob (spacing_);
  spacing_ =0;
}

void
Spacing_engraver::acknowledge_grob (Grob_info i)
{
  if (Note_spacing::has_interface (i.grob_) || Staff_spacing::has_interface (i.grob_))
    {
      Pointer_group_interface::add_grob (spacing_, ly_symbol2scm  ("wishes"), i.grob_);
    }
  
  if (i.grob_->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    return;

  /*
    only pay attention to durations that are not grace notes. 
   */
  if (!now_.grace_part_)
    {
      Music *r = i.music_cause ();
      if (r && r->is_mus_type ("rhythmic-event"))
	{
	  Moment len = r->length_mom ();
	  Rhythmic_tuple t (i, now_mom () + len);
	  now_durations_.push (t);
	}
    }
}

void
Spacing_engraver::stop_translation_timestep ()
{
  Moment shortest_playing;
  shortest_playing.set_infinite (1);
  for (int i=0; i < playing_durations_.size (); i++)
    {
      Moment m = (playing_durations_[i].info_.music_cause ())->length_mom ();
      shortest_playing = shortest_playing <? m;
    }
  
  Moment starter;
  starter.set_infinite (1);

  for (int i=0; i < now_durations_.size (); i++)
    {
      Moment m = now_durations_[i].info_.music_cause ()->length_mom ();
      if (m.to_bool ())
	{
	  starter = starter <? m;
	  playing_durations_.insert (now_durations_[i]);

	}
    }
  now_durations_.clear ();
  
  shortest_playing = shortest_playing <? starter;
  
  Paper_column * sc
    = dynamic_cast<Paper_column*> (unsmob_grob (get_property ("currentMusicalColumn")));

  assert (starter.to_bool ());
  SCM sh = shortest_playing.smobbed_copy ();
  SCM st = starter.smobbed_copy ();

  sc->set_grob_property ("shortest-playing-duration", sh);  
  sc->set_grob_property ("shortest-starter-duration", st);
}

void
Spacing_engraver::start_translation_timestep ()
{
  now_ = now_mom ();
  stopped_durations_.clear ();
  while (playing_durations_.size () && playing_durations_.front ().end_ < now_)
    playing_durations_.delmin ();
  while (playing_durations_.size () && playing_durations_.front ().end_ == now_)
    stopped_durations_.push (playing_durations_.get ());
}




ENTER_DESCRIPTION(Spacing_engraver,
/* descr */       "make a SpacingSpanner and do bookkeeping of shortest starting and playing notes  ",
/* creats*/       "SpacingSpanner",
/* accepts */     "",
/* acks  */      "grob-interface",
/* reads */       "",
/* write */       "");
