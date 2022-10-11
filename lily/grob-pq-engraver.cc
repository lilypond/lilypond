/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "engraver.hh"
#include "grob.hh"
#include "warn.hh"

#include "translator.icc"

#include <algorithm>
#include <vector>

using std::vector;

struct Grob_pq_entry
{
  Grob *grob_;
  Moment end_;
};

bool
operator<(Grob_pq_entry const &a, Grob_pq_entry const &b)
{
  return a.end_ < b.end_;
}

class Grob_pq_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Grob_pq_engraver);

protected:
  void initialize () override;
  void acknowledge_grob (Grob_info) override;
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_acknowledged ();

  vector<Grob_pq_entry> started_now_;
};

Grob_pq_engraver::Grob_pq_engraver (Context *c)
  : Engraver (c)
{
}

void
Grob_pq_engraver::initialize ()
{
  set_property (context (), "busyGrobs", SCM_EOL);
}

LY_DEFINE (ly_grob_pq_less_p, "ly:grob-pq<?", 2, 0, 0, (SCM a, SCM b),
           R"(
Compare two grob priority queue entries.  This is an internal function.
           )")
{
  if (Moment::compare (*unsmob<Moment> (scm_car (a)),
                       *unsmob<Moment> (scm_car (b)))
      < 0)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

void
Grob_pq_engraver::acknowledge_grob (Grob_info gi)
{
  Stream_event *ev = gi.event_cause ();

  if (ev
      && !gi.grob ()->internal_has_interface (
        ly_symbol2scm ("multi-measure-interface")))
    {
      const auto now = now_mom ();
      const auto len = get_event_length (ev, now);
      if (len)
        started_now_.emplace_back (Grob_pq_entry {gi.grob (), now + len});
    }
}

void
Grob_pq_engraver::process_acknowledged ()
{
  std::sort (started_now_.begin (), started_now_.end ());
  SCM lst = SCM_EOL;
  SCM *tail = &lst;
  for (vsize i = 0; i < started_now_.size (); i++)
    {
      *tail = scm_acons (started_now_[i].end_.smobbed_copy (),
                         started_now_[i].grob_->self_scm (), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  SCM busy = get_property (this, "busyGrobs");
  busy = scm_merge_x (lst, busy, ly_grob_pq_less_p_proc);
  set_property (context (), "busyGrobs", busy);

  started_now_.clear ();
}

void
Grob_pq_engraver::stop_translation_timestep ()
{
  auto now = now_mom ();
  SCM start_busy = get_property (this, "busyGrobs");
  SCM busy = start_busy;
  while (scm_is_pair (busy) && *unsmob<Moment> (scm_caar (busy)) == now)
    busy = scm_cdr (busy);
}

void
Grob_pq_engraver::start_translation_timestep ()
{
  auto now = now_mom ();

  SCM start_busy = get_property (this, "busyGrobs");
  SCM busy = start_busy;
  while (scm_is_pair (busy) && *unsmob<Moment> (scm_caar (busy)) < now)
    {
      /*
        The grob-pq-engraver is not water tight, and stuff like
        tupletSpannerDuration confuses it.
      */
      busy = scm_cdr (busy);
    }

  if (!scm_is_eq (start_busy, busy))
    set_property (context (), "busyGrobs", busy);
}

void
Grob_pq_engraver::boot ()
{
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Grob_pq_engraver,
                /* doc */
                R"(
Administrate when certain grobs (e.g., note heads) stop playing.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
busyGrobs
                )",

                /* write */
                R"(
busyGrobs
                )");
