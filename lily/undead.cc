/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

class Undead : public Simple_smob<Undead>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
private:
  SCM object_;
public:
  SCM object () const { return object_; }
  Undead (SCM object = SCM_UNDEFINED) : object_ (object) { };
};

SCM
Undead::mark_smob () const
{
  bool saved = parsed_objects_should_be_dead;
  parsed_objects_should_be_dead = false;
  scm_gc_mark (object ());
  parsed_objects_should_be_dead = saved;
  return SCM_UNDEFINED;
}

int
Undead::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Undead ", port);
  scm_display (object (), port);
  scm_puts (" >", port);
  return 1;
}

const char *const Undead::type_p_name_ = "ly:undead?";

LY_DEFINE (ly_make_undead, "ly:make-undead",
           1, 0, 0, (SCM object),
           "This packages @var{object} in a manner that keeps it from"
           " triggering \"Parsed object should be dead\" messages.")
{
  Undead undead (object);
  return undead.smobbed_copy ();
}

LY_DEFINE (ly_get_undead, "ly:get-undead",
           1, 0, 0, (SCM undead),
           "Get back object from @var{undead}.")
{
  auto *const u = LY_ASSERT_SMOB (Undead, undead, 1);
  return u->object ();
}

// '
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
      SCM elt = elements[i]->readout_one ();
      if (!SCM_UNBNDP (elt))
        result = scm_cons (elt, result);
    }
  return result;
}

LY_DEFINE (ly_parsed_undead_list_x, "ly:parsed-undead-list!",
           0, 0, 0, (),
           "Return the list of objects that have been found live"
           " that should have been dead, and clear that list.")
{
  return parsed_dead::readout ();
}
