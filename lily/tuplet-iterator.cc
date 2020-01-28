/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "music-wrapper-iterator.hh"
#include "music.hh"
#include "stream-event.hh"

/*
  Iterates \times, by sending TupletSpanEvents at the start/end of each
  tuplet bracket.  Extra stop/start events are sent at regular
  intervals if tupletSpannerDuration is set.
*/
class Tuplet_iterator : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  OVERRIDE_CLASS_NAME (Tuplet_iterator);
  Tuplet_iterator ();

protected:
  void process (Moment m) override;
  void construct_children () override;
  void derived_mark () const override;
  Moment pending_moment () const override;

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

Music *
Tuplet_iterator::create_event (Direction d)
{
  SCM ev_scm = Lily::make_span_event (ly_symbol2scm ("TupletSpanEvent"),
                                      scm_from_int (d));

  Music *mus = get_music ();

  Music *ev = unsmob<Music> (ev_scm);
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

Tuplet_iterator::Tuplet_iterator ()
{
  spanner_duration_ = next_split_mom_ = 0;
  synthesized_events_ = SCM_EOL;
}

Moment
Tuplet_iterator::pending_moment () const
{
  Moment next_mom = Music_wrapper_iterator::pending_moment ();
  next_mom = std::min (next_mom, next_split_mom_);

  return next_mom;
}

void
Tuplet_iterator::process (Moment m)
{
  if (spanner_duration_.to_bool () && m.main_part_ == next_split_mom_)
    {
      descend_to_bottom_context ();
      if (tuplet_handler_.get_context ())
        create_event (STOP)->send_to_context (tuplet_handler_.get_context ());

      if (m.main_part_ < music_get_length ().main_part_)
        {
          spanner_duration_ = std::min (music_get_length () - next_split_mom_,
                                        spanner_duration_);
          tuplet_handler_.set_context (get_outlet ());
          report_event (create_event (START));

          next_split_mom_ += spanner_duration_;
        }
      else
        tuplet_handler_.set_context (0);
    }
  Music_wrapper_iterator::process (m);
  if (child_iter_ && child_iter_->ok ())
    descend_to_child (child_iter_->get_outlet ());
}

void
Tuplet_iterator::construct_children ()
{
  if (Duration *d = unsmob<Duration> (get_music ()->get_property ("duration")))
    spanner_duration_ = d->get_length ();
  else if (Moment *mp = unsmob<Moment> (
               get_outlet ()->get_property ("tupletSpannerDuration")))
    spanner_duration_ = mp->main_part_;
  else
    spanner_duration_.set_infinite (1);

  Music_wrapper_iterator::construct_children ();

  if (child_iter_ && child_iter_->ok ())
    descend_to_child (child_iter_->get_outlet ());
}

void
Tuplet_iterator::derived_mark () const
{
  scm_gc_mark (synthesized_events_);
  Music_wrapper_iterator::derived_mark ();
}

IMPLEMENT_CTOR_CALLBACK (Tuplet_iterator);
