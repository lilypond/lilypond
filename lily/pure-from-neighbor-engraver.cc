/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Mike Solomon <mike@apollinemike.com>

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

#include <map>
#include <algorithm>

#include "grob.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "pure-from-neighbor-interface.hh"
#include "engraver.hh"

class Pure_from_neighbor_engraver : public Engraver
{
  vector<Grob *> items_then_;
  vector<Grob *> items_now_;
  vector<Grob *> pures_then_;
  vector<Grob *> pures_now_;

public:
  TRANSLATOR_DECLARATIONS (Pure_from_neighbor_engraver);
protected:
  DECLARE_ACKNOWLEDGER (pure_from_neighbor);
  DECLARE_ACKNOWLEDGER (item);
  void stop_translation_timestep ();
};

Pure_from_neighbor_engraver::Pure_from_neighbor_engraver ()
{
}

void
Pure_from_neighbor_engraver::acknowledge_item (Grob_info i)
{
  SCM pure_relevant_p = ly_lily_module_constant ("pure-relevant?");
  if (!Pure_from_neighbor_interface::has_interface (i.grob ())
      && to_boolean (scm_call_1 (pure_relevant_p, i.item ()->self_scm ())))
    items_now_.push_back (i.item ());
}

// note that this can get out of hand if there are lots of vertical axis groups...

void
Pure_from_neighbor_engraver::acknowledge_pure_from_neighbor (Grob_info i)
{
  pures_now_.push_back (i.item ());
}

void
Pure_from_neighbor_engraver::stop_translation_timestep ()
{
  if (pures_now_.size ())
    {
      for (vsize i = 0; i < pures_now_.size (); i++)
        for (vsize j = 0; j < items_then_.size (); j++)
          Pointer_group_interface::add_grob (pures_now_[i], ly_symbol2scm ("elements"), items_then_[j]);

      for (vsize i = 0; i < pures_then_.size (); i++)
        for (vsize j = 0; j < items_now_.size (); j++)
          Pointer_group_interface::add_grob (pures_then_[i], ly_symbol2scm ("elements"), items_now_[j]);

      items_then_.clear ();
      items_then_.insert (items_then_.end (), items_now_.begin (), items_now_.end ());
      items_now_.clear ();

      pures_then_.clear ();
      pures_then_.insert (pures_then_.end (), pures_now_.begin (), pures_now_.end ());
      pures_now_.clear ();
    }
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Pure_from_neighbor_engraver, item);
ADD_ACKNOWLEDGER (Pure_from_neighbor_engraver, pure_from_neighbor);
ADD_TRANSLATOR (Pure_from_neighbor_engraver,
                /* doc */
                "Coordinates items that get their pure heights from their neighbors.",

                /* create */
                "",

                /* read */
                "",

                /* write */
                ""
               );
