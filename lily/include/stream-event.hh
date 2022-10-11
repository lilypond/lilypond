/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Erik Sandberg <mandolaerik@gmail.com>

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

#ifndef STREAM_EVENT_HH
#define STREAM_EVENT_HH

#include "lily-proto.hh"
#include "diagnostics.hh"
#include "smobs.hh"
#include "prob.hh"

class Stream_event : public Prob, public Diagnostics
{
public:
  Stream_event ();
  OVERRIDE_CLASS_NAME (Stream_event);
  virtual Stream_event *clone () const { return new Stream_event (*this); }

  Stream_event (SCM event_class, SCM immutable_props = SCM_EOL);
  Stream_event (SCM class_name, Input *);

  Input *origin () const override;
  void set_spot (Input *i);
  bool internal_in_event_class (SCM class_name);
  void make_transposable ();

  SCM copy_mutable_properties () const override;

  DECLARE_SCHEME_CALLBACK (undump, (SCM));
  DECLARE_SCHEME_CALLBACK (dump, (SCM));
};

#define in_event_class(class_name)                                             \
  internal_in_event_class (ly_symbol2scm (class_name))

SCM ly_event_deep_copy (SCM);

void warn_reassign_event_ptr (Stream_event &old_ev, Stream_event *new_ev);

// Assign to old_ev at most once, returning true when it happens, and warning
// (with some exceptions) about attempted reassignments.
inline bool
assign_event_once (Stream_event *&old_ev, Stream_event *new_ev)
{
  if (!old_ev)
    {
      old_ev = new_ev;
      return old_ev;
    }
  else
    {
      warn_reassign_event_ptr (*old_ev, new_ev);
      return false;
    }
}

#endif /* STREAM_EVENT_HH */
