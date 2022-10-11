/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "smobs.hh"

using std::vector;

bool parsed_objects_should_be_dead = false;

// These are not protected since the means of protecting them would be
// problematic to trigger during the mark pass where the array element
// references get set.  However, they get set only when in the mark
// pass when checking for parsed elements that should be dead, and we
// query and clear them immediately afterwards.  So there should be no
// way in which the references would have become unprotected in the
// mean time.

vector<parsed_dead *> parsed_dead::elements;

SCM
parsed_dead::readout ()
{
  SCM result = SCM_EOL;
  for (vsize i = 0; i < elements.size (); i++)
    {
      while (1)
        {
          SCM elt = elements[i]->readout_one ();
          if (SCM_UNBNDP (elt))
            break;

          // Guile puts the garbage collector into "Java finalization" mode.
          // This means to-be-finalized objects are marked to keep dependent
          // objects around until the next collection, in case the finalizer
          // puts them into a global variable and makes them live again. Just
          // ignore these cases since the smob is gone from our point of view.
          if (SCM_SMOBNUM (elt) == 0)
            continue;

          result = scm_cons (elt, result);
        }
    }
  return result;
}

LY_DEFINE (ly_parsed_undead_list_x, "ly:parsed-undead-list!", 0, 0, 0, (),
           R"(
Return the list of objects that have been found alive but should have been
dead, and clear that list.
           )")
{
  return parsed_dead::readout ();
}
