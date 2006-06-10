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
#include "sequential-iterator.hh"

/*
  Iterates \times, by sending TupletEvents at the start/end of each
  tuplet bracket. Extra stop/start events are sent at regular
  intervals if tupletSpannerDuration is set.
*/
class Time_scaled_music_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  DECLARE_CLASSNAME(Time_scaled_music_iterator);
  Time_scaled_music_iterator ();
protected:
  virtual SCM get_music_list () const;
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
};

Time_scaled_music_iterator::Time_scaled_music_iterator ()
{
  spanner_duration_ = next_split_mom_ = 0;
}


Moment
Time_scaled_music_iterator::pending_moment () const
{
  Moment next_mom = Sequential_iterator::pending_moment ();

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
  if (spanner_duration_.to_bool () &&
      m.main_part_ == next_split_mom_)
    {
      report_event (stop_);
      report_event (start_);
      next_split_mom_ += spanner_duration_;
      /* avoid sending events twice at the end */
      if (next_split_mom_ == get_music ()->get_length ().main_part_)
	next_split_mom_.set_infinite (1);
    }
  Sequential_iterator::process(m);
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

  SCM tuplet_symbol = ly_symbol2scm ("TupletEvent");
  SCM start_scm = scm_call_2 (ly_lily_module_constant ("make-span-event"), tuplet_symbol, scm_from_int (START));
  start_ = unsmob_music (start_scm);
  start_->set_spot (*origin);
  start_->set_property ("numerator", mus->get_property ("numerator"));
  start_->set_property ("denominator", mus->get_property ("denominator"));

  SCM stop_scm = scm_call_2 (ly_lily_module_constant ("make-span-event"), tuplet_symbol, scm_from_int (STOP));
  stop_ = unsmob_music (stop_scm);
  stop_->set_spot (*origin);

  Moment *mp = unsmob_moment (get_outlet ()->get_property ("tupletSpannerDuration"));

  if (mp)
    {
      spanner_duration_ = mp->main_part_;
      next_split_mom_ = spanner_duration_;
    }

  Sequential_iterator::construct_children ();
}

SCM
Time_scaled_music_iterator::get_music_list () const
{
  Music *mus = get_music ();
  SCM child = mus->get_property ("element");

  return scm_list_3 (start_->self_scm (), child, stop_->self_scm ());
}

void
Time_scaled_music_iterator::derived_mark () const
{
  if (start_)
    scm_gc_mark (start_->self_scm ());
  if (stop_)
    scm_gc_mark (stop_->self_scm ());

  Sequential_iterator::derived_mark ();
}

IMPLEMENT_CTOR_CALLBACK (Time_scaled_music_iterator);
