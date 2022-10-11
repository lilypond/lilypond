/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Juergen Reuter <reuter@ipd.uka.de>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"
#include "spanner.hh"
#include "note-head.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "pitch.hh"
#include "stream-event.hh"
#include "item.hh"

#include "translator.icc"

using std::vector;

class Cluster_spanner_engraver : public Engraver
{

protected:
  TRANSLATOR_DECLARATIONS (Cluster_spanner_engraver);
  void listen_cluster_note (Stream_event *);
  void acknowledge_note_column (Grob_info_t<Item>);
  void stop_translation_timestep ();
  void process_music ();
  void finalize () override;

private:
  vector<Stream_event *> cluster_notes_;
  Item *beacon_;

  void typeset_grobs ();
  Spanner *spanner_;
  Spanner *finished_spanner_;
};

Cluster_spanner_engraver::Cluster_spanner_engraver (Context *c)
  : Engraver (c)
{
  spanner_ = 0;
  finished_spanner_ = 0;
  beacon_ = 0;
}

void
Cluster_spanner_engraver::finalize ()
{
  typeset_grobs ();
  finished_spanner_ = spanner_;
  spanner_ = 0;
  typeset_grobs ();
}

void
Cluster_spanner_engraver::typeset_grobs ()
{
  if (finished_spanner_)
    {
      if (!finished_spanner_->get_bound (RIGHT))
        {
          finished_spanner_->set_bound (RIGHT,
                                        finished_spanner_->get_bound (LEFT));
        }

      finished_spanner_ = 0;
    }
  beacon_ = 0;
}

void
Cluster_spanner_engraver::listen_cluster_note (Stream_event *ev)
{
  cluster_notes_.push_back (ev);
}

void
Cluster_spanner_engraver::process_music ()
{
  if (cluster_notes_.size ())
    {
      SCM c0scm = get_property (this, "middleCPosition");

      int c0 = scm_is_number (c0scm) ? from_scm<int> (c0scm) : 0;
      int pmax = INT_MIN;
      int pmin = INT_MAX;

      for (vsize i = 0; i < cluster_notes_.size (); i++)
        {
          Pitch *pit
            = unsmob<Pitch> (get_property (cluster_notes_[i], "pitch"));

          int p = (pit ? pit->steps () : 0) + c0;

          pmax = std::max (pmax, p);
          pmin = std::min (pmin, p);
        }

      beacon_
        = make_item ("ClusterSpannerBeacon", cluster_notes_[0]->self_scm ());
      set_property (beacon_, "positions",
                    scm_cons (to_scm (pmin), to_scm (pmax)));
    }

  if (beacon_ && !spanner_)
    spanner_ = make_spanner ("ClusterSpanner", cluster_notes_[0]->self_scm ());

  if (beacon_ && spanner_)
    {
      add_bound_item (spanner_, beacon_);
      Pointer_group_interface::add_grob (spanner_, ly_symbol2scm ("columns"),
                                         beacon_);
    }
}

void
Cluster_spanner_engraver::stop_translation_timestep ()
{
  typeset_grobs ();
  cluster_notes_.clear ();
}

void
Cluster_spanner_engraver::acknowledge_note_column (Grob_info_t<Item>)
{
  if (!beacon_)
    {
      finished_spanner_ = spanner_;
      spanner_ = 0;
    }
}

void
Cluster_spanner_engraver::boot ()
{
  ADD_LISTENER (cluster_note);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Cluster_spanner_engraver,
                /* doc */
                R"(
Engrave a cluster using @code{Spanner} notation.
                )",

                /* create */
                R"(
ClusterSpanner
ClusterSpannerBeacon
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
