/*
  time-scaled-music-iterator.cc -- implement Time_scaled_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>
*/

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "music-wrapper-iterator.hh"
#include "stream-event.hh"

/*
  Iterates \times, by sending TupletSpanEvents at the start/end of each
  tuplet bracket. Extra stop/start events are sent at regular
  intervals if tupletSpannerDuration is set.
*/
class Time_scaled_music_iterator : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  DECLARE_CLASSNAME (Time_scaled_music_iterator);
  Time_scaled_music_iterator ();
protected:
  virtual void process (Moment m);
  virtual void construct_children ();
  virtual void derived_mark () const;
  virtual Moment pending_moment () const;

  Music *create_event (Direction d);
  
private:

  /* tupletSpannerDuration */
  Moment spanner_duration_;

  /* next time to add a stop/start pair */
  Moment next_split_mom_;
  
  /* Recycle start/stop events if tupletSpannerDuration is set. */
  SCM synthesized_events_;
  
  Context_handle tuplet_handler_;
};

Music*
Time_scaled_music_iterator::create_event (Direction d)
{
  SCM ev_scm = scm_call_2 (ly_lily_module_constant ("make-span-event"),
			   ly_symbol2scm ("TupletSpanEvent"),
			   scm_from_int (d));
  
  Music *mus = get_music ();

  Music *ev = unsmob_music (ev_scm);
  ev->set_spot (*mus->origin ());
  if (d == START)
    {
      ev->set_property ("numerator", mus->get_property ("numerator"));
      ev->set_property ("denominator", mus->get_property ("denominator"));
      ev->set_property ("tweaks", mus->get_property ("tweaks"));
      ev->set_property ("length", spanner_duration_.smobbed_copy ()); 
    }  

  synthesized_events_ = scm_cons (ev_scm, synthesized_events_);
  return ev;
}


Time_scaled_music_iterator::Time_scaled_music_iterator ()
{
  spanner_duration_ = next_split_mom_ = 0;
  synthesized_events_ = SCM_EOL;
}


Moment
Time_scaled_music_iterator::pending_moment () const
{
  Moment next_mom = Music_wrapper_iterator::pending_moment ();
  next_mom = min (next_mom, next_split_mom_);

  return next_mom;
}


void
Time_scaled_music_iterator::process (Moment m)
{
  if (spanner_duration_.to_bool () &&
      m.main_part_ == next_split_mom_)
    {
      descend_to_bottom_context ();
      if (tuplet_handler_.get_outlet ())
	create_event (STOP)->send_to_context (tuplet_handler_.get_outlet ());

      if (m.main_part_ < music_get_length ().main_part_)
	{
	  tuplet_handler_.set_context (get_outlet ());
	  report_event (create_event (START));
      
	  next_split_mom_ += spanner_duration_;
	}
      else
	{
	  tuplet_handler_.set_context (0);
	}
    }
  Music_wrapper_iterator::process (m);
  if (child_iter_ && child_iter_->ok ())
    descend_to_child (child_iter_->get_outlet ());
  
}

void
Time_scaled_music_iterator::construct_children ()
{
  spanner_duration_ = music_get_length ();

  Moment *mp = unsmob_moment (get_outlet ()->get_property ("tupletSpannerDuration"));
  if (mp)
    spanner_duration_ = min (mp->main_part_, spanner_duration_);
  
  Music_wrapper_iterator::construct_children ();

  if (child_iter_ && child_iter_->ok ())
    descend_to_child (child_iter_->get_outlet ());
}

void
Time_scaled_music_iterator::derived_mark () const
{
  scm_gc_mark (synthesized_events_);
  Music_wrapper_iterator::derived_mark ();
}

IMPLEMENT_CTOR_CALLBACK (Time_scaled_music_iterator);
