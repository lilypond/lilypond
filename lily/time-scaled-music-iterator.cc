/*
  time-scaled-music-iterator.cc -- implement Time_scaled_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>
*/

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "music-wrapper-iterator.hh"

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
  DECLARE_CLASSNAME(Time_scaled_music_iterator);
  Time_scaled_music_iterator ();
protected:
  virtual void process (Moment m);
  virtual void construct_children ();
  virtual void derived_mark () const;
  virtual Moment pending_moment () const;
private:

  /* tupletSpannerDuration */
  Moment spanner_duration_;

  /* next time to add a stop/start pair */
  Moment next_split_mom_;
  
  /* Recycle start/stop events if tupletSpannerDuration is set. */
  Music *start_;
  Music *stop_;

  bool done_first_;
  
  Context_handle tuplet_handler_;
};

Time_scaled_music_iterator::Time_scaled_music_iterator ()
{
  spanner_duration_ = next_split_mom_ = 0;
  done_first_ = 0;
}


Moment
Time_scaled_music_iterator::pending_moment () const
{
  if (!done_first_)
    return Moment (0);
  
  Moment next_mom = Music_wrapper_iterator::pending_moment ();

  if (spanner_duration_.to_bool () &&
      next_mom.main_part_ > next_split_mom_)
    {
      next_mom = next_split_mom_;
    }

  return next_mom;
}


void
Time_scaled_music_iterator::process (Moment m)
{
  if (!done_first_)
    {
      done_first_ = true;
      descend_to_bottom_context ();
      report_event (start_);
      tuplet_handler_.set_context (get_outlet());
    }

  if (spanner_duration_.to_bool () &&
      m.main_part_ == next_split_mom_)
    {
      descend_to_bottom_context ();
      stop_->send_to_context (tuplet_handler_.get_outlet ());
      
      tuplet_handler_.set_context (get_outlet ());
      report_event (start_);
      
      next_split_mom_ += spanner_duration_;
      /* avoid sending events twice at the end */
      if (next_split_mom_ == get_music ()->get_length ().main_part_)
	next_split_mom_.set_infinite (1);
    }

  Music_wrapper_iterator::process(m);
  if (child_iter_ && child_iter_->ok ())
    descend_to_child (child_iter_->get_outlet ());
  
  if (m.main_part_ == music_get_length ().main_part_)
    {
      stop_->send_to_context (tuplet_handler_.get_outlet ());
      tuplet_handler_.set_context (0);
    }
}

void
Time_scaled_music_iterator::construct_children ()
{
  /*
    Inheritance trickery:
    Time_scaled_music_iterator::construct_children initialises start_
    and stop_, and calls Sequential_music::construct_children, which
    in turn calls Time_scaled_music_iterator::get_music which reads
    start_ and stop_.
   */

  Music *mus = get_music ();
  Input *origin = mus->origin ();

  SCM tuplet_symbol = ly_symbol2scm ("TupletSpanEvent");
  SCM start_scm = scm_call_2 (ly_lily_module_constant ("make-span-event"), tuplet_symbol, scm_from_int (START));
  start_ = unsmob_music (start_scm);
  start_->set_spot (*origin);
  start_->set_property ("numerator", mus->get_property ("numerator"));
  start_->set_property ("denominator", mus->get_property ("denominator"));
  start_->set_property ("tweaks", mus->get_property ("tweaks"));
  

  SCM stop_scm = scm_call_2 (ly_lily_module_constant ("make-span-event"), tuplet_symbol, scm_from_int (STOP));
  stop_ = unsmob_music (stop_scm);
  stop_->set_spot (*origin);

  Moment *mp = unsmob_moment (get_outlet ()->get_property ("tupletSpannerDuration"));

  if (mp)
    {
      spanner_duration_ = mp->main_part_;
      next_split_mom_ = spanner_duration_;
    }

  Music_wrapper_iterator::construct_children ();

  if (child_iter_ && child_iter_->ok ())
    descend_to_child (child_iter_->get_outlet ());
}

void
Time_scaled_music_iterator::derived_mark () const
{
  if (start_)
    scm_gc_mark (start_->self_scm ());
  if (stop_)
    scm_gc_mark (stop_->self_scm ());

  Music_wrapper_iterator::derived_mark ();
}

IMPLEMENT_CTOR_CALLBACK (Time_scaled_music_iterator);
