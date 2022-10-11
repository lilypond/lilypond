/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
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
#include "music.hh"
#include "music-wrapper-iterator.hh"
#include "stream-event.hh"
#include "lily-imports.hh"

/*
  Iterates \times, by sending TupletSpanEvents at the start/end of each
  tuplet bracket.  Extra stop/start events are sent at regular
  intervals if tupletSpannerDuration is set.
*/
class Tuplet_iterator final : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  OVERRIDE_CLASS_NAME (Tuplet_iterator);
  Tuplet_iterator () = default;

protected:
  void process (Moment m) override;
  void create_contexts () override;
  Moment pending_moment () const override;

  SCM create_event (Direction d);

private:
  /* tupletSpannerDuration */
  Moment spanner_duration_ {-1};

  /* next time to add a stop/start pair */
  Moment next_split_mom_;

  Context_handle tuplet_handler_;
};

SCM
Tuplet_iterator::create_event (Direction d)
{
  SCM ev_scm
    = Lily::make_span_event (ly_symbol2scm ("TupletSpanEvent"), to_scm (d));

  Music *mus = get_music ();

  Music *ev = unsmob<Music> (ev_scm);
  ev->set_spot (*mus->origin ());
  if (d == START)
    {
      set_property (ev, "numerator", get_property (mus, "numerator"));
      set_property (ev, "denominator", get_property (mus, "denominator"));
      set_property (ev, "tweaks", get_property (mus, "tweaks"));
      set_property (ev, "length", spanner_duration_.smobbed_copy ());
    }

  return ev_scm;
}

Moment
Tuplet_iterator::pending_moment () const
{
  auto next_mom = Music_wrapper_iterator::pending_moment ();
  if (next_mom < Moment::infinity ())
    {
      next_mom = std::min (next_mom, next_split_mom_);
    }
  return next_mom;
}

void
Tuplet_iterator::process (Moment m)
{
  if (spanner_duration_.main_part_ < 0) // first time
    {
      if (auto *d = unsmob<Duration> (get_property (get_music (), "duration")))
        {
          spanner_duration_ = Moment (d->get_length ());
        }
      else
        {
          SCM d_scm = get_property (get_context (), "tupletSpannerDuration");
          if (auto *mp = unsmob<Moment> (d_scm))
            spanner_duration_ = Moment (mp->main_part_); // discard grace part
          else
            spanner_duration_ = Moment::infinity ();
        }
    }

  if (spanner_duration_ && Moment (m.main_part_) == next_split_mom_)
    {
      if (tuplet_handler_)
        {
          SCM ev = create_event (STOP);
          unsmob<Music> (ev)->send_to_context (tuplet_handler_.get ());
          scm_remember_upto_here_1 (ev);
        }

      if (m.main_part_ < music_get_length ().main_part_)
        {
          spanner_duration_ = std::min (music_get_length () - next_split_mom_,
                                        spanner_duration_);
          tuplet_handler_ = get_context ();
          SCM ev = create_event (START);
          report_event (unsmob<Music> (ev));
          scm_remember_upto_here_1 (ev);

          next_split_mom_ += spanner_duration_;
        }
      else
        tuplet_handler_ = nullptr;
    }
  Music_wrapper_iterator::process (m);

  auto *child = get_child ();
  if (child && child->ok ())
    descend_to_child (child->get_context ());
}

void
Tuplet_iterator::create_contexts ()
{
  Music_wrapper_iterator::create_contexts ();

  auto *child = get_child ();
  if (child && child->ok ())
    descend_to_child (child->get_context ());
}

IMPLEMENT_CTOR_CALLBACK (Tuplet_iterator);
