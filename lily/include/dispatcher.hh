/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Erik Sandberg  <mandolaerik@gmail.com>

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

#ifndef DISPATCHER_HH
#define DISPATCHER_HH

#include "listener.hh"
#include "stream-event.hh"
#include "smobs.hh"

class Dispatcher : public Smob<Dispatcher>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Dispatcher ();

private:
  /* Hash table. Each event-class maps to a list of listeners. */
  SCM listeners_;
  /* alist of dispatchers that we listen to. Each entry is a
     (dist . priority) pair. */
  SCM dispatchers_;
  SCM listen_classes_;
  void dispatch (SCM);
  /* priority counter. Listeners with low priority receive events
     first. */
  int priority_count_;
  void internal_add_listener (SCM callback, SCM event_class, int priority);

public:
  Dispatcher ();
  void broadcast (Stream_event *ev);
  bool is_listened_class (SCM);
  SCM listened_types ();
  void add_listener (Listener, SCM event_class);
  void add_listener (SCM callback, SCM event_class);
  void remove_listener (Listener, SCM event_class);
  void register_as_listener (Dispatcher *dist);
  void unregister_as_listener (Dispatcher *dist);
};

#endif // DISPATCHER_HH
