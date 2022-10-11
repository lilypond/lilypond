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

#include "dispatcher.hh"
#include "input.hh"
#include "international.hh"
#include "warn.hh"
#include "lily-imports.hh"

#if defined(__MINGW32__)
#include <malloc.h>
#elif defined(__DragonFly__) || defined(__FreeBSD__) || defined(__NetBSD__)    \
  || defined(__OpenBSD__)
#include <stdlib.h>
#else
#include <alloca.h>
#endif

const char *const Dispatcher::type_p_name_ = "ly:dispatcher?";

Dispatcher::~Dispatcher ()
{
}

Dispatcher::Dispatcher ()
{
  listeners_ = SCM_EOL;
  dispatchers_ = SCM_EOL;
  listen_classes_ = SCM_EOL;
  smobify_self ();
  listeners_ = scm_c_make_hash_table (17);
  priority_count_ = 0;
}

SCM
Dispatcher::mark_smob () const
{
  scm_gc_mark (dispatchers_);
  scm_gc_mark (listen_classes_);
  return listeners_;
}

int
Dispatcher::print_smob (SCM p, scm_print_state *) const
{
  scm_puts ("#<Dispatcher ", p);
  scm_write (Lily::hash_table_to_alist (listeners_), p);
  scm_puts (">", p);
  return 1;
}

/*
Event dispatching:
- Collect a list of listeners for each relevant class
- Send the event to each of these listeners, in increasing priority order.
  This is done by keeping a priority queue of listener lists,
  and iteratively send the event to the lowest-priority listener.
- An event is never sent twice to listeners with equal priority.
  The only case where listeners with equal priority may exist is when
  two dispatchers are connected for more than one event type.  In that
  case, the respective listeners all have the same priority, making
  sure that any event is only dispatched at most once for that
  combination of dispatchers, even if it matches more than one event
  type.
*/
void
Dispatcher::dispatch (SCM sev)
{
  Stream_event *ev = unsmob<Stream_event> (sev);
  SCM class_list = get_property (ev, "class");
  if (!scm_is_pair (class_list))
    {
      ev->warning (_ ("Event class should be a list"));
      return;
    }

  long num_classes = scm_ilength (class_list);

  /*
    For each event class there is a list of listeners, which is
    ordered by priority. Our next task is to call these listeners, in
    priority order.  A priority queue stores the next element in each
    listener list, and the lowest priority element is repeatedly
    extracted and called.

    The priority queue is implemented as an insertion-sorted C
    array. Using the stack instead of native Scheme datastructures
    avoids overheads for memory allocation. The queue is usually small
    (around 2 elements), so the quadratic sorting time is not a
    problem. (if this changes, it's easy to rewrite this routine using
    a heap)

    The first step is to collect all listener lists and to initially
    insert them in the priority queue.
  */
  struct Entry
  {
    int prio;
    SCM list;
  };

  auto *const lists
    = reinterpret_cast<Entry *> (alloca (sizeof (Entry) * (num_classes + 1)));

  int i = 0;
  for (SCM cl = class_list; scm_is_pair (cl); cl = scm_cdr (cl))
    {
      SCM list = scm_hashq_ref (listeners_, scm_car (cl), SCM_EOL);
      if (!scm_is_pair (list))
        num_classes--;
      else
        {
          // insertion sort.
          int prio = from_scm<int> (scm_caar (list));
          int j;
          for (j = i; j > 0 && lists[j - 1].prio > prio; j--)
            lists[j] = lists[j - 1];
          lists[j].prio = prio;
          lists[j].list = list;
          i++;
        }
    }
  lists[num_classes].prio = INT_MAX;

  // Never send an event to two listeners with equal priority.
  int last_priority = -1;
  /*
    Each iteration extracts the lowest-priority element, which is a
    list of listeners. The first listener is called, and the tail of
    the list is pushed back into the priority queue.
  */
  while (num_classes)
    {
      // Send the event, if we haven't already sent it to this target.
      if (lists[0].prio != last_priority)
        {
          // process the listener
          assert (lists[0].prio > last_priority);
          last_priority = lists[0].prio;

          SCM l = scm_cdar (lists[0].list);
          ly_call (l, ev->self_scm ());
        }
      // go to the next listener; bubble-sort the class list.
      SCM next = scm_cdr (lists[0].list);
      if (!scm_is_pair (next))
        num_classes--;
      int prio
        = (scm_is_pair (next)) ? from_scm<int> (scm_caar (next)) : INT_MAX;
      for (i = 0; prio > lists[i + 1].prio; i++)
        lists[i] = lists[i + 1];
      lists[i].prio = prio;
      lists[i].list = next;
    }
}

bool
Dispatcher::is_listened_class (SCM cl)
{
  for (; scm_is_pair (cl); cl = scm_cdr (cl))
    {
      SCM list = scm_hashq_ref (listeners_, scm_car (cl), SCM_EOL);
      if (scm_is_pair (list))
        return true;
    }
  return false;
}

SCM
Dispatcher::listened_types ()
{
  auto accumulate_types
    = [] (void * /* closure */, SCM key, SCM val, SCM result) {
        if (scm_is_pair (val))
          return scm_cons (key, result);
        return result;
      };

  return ly_scm_hash_fold (accumulate_types, nullptr, SCM_EOL, listeners_);
}

void
Dispatcher::broadcast (Stream_event *ev)
{
  dispatch (ev->self_scm ());
}

// add_listener will always assign a new priority for each call
void
Dispatcher::add_listener (Listener l, SCM ev_class)
{
  add_listener (l.smobbed_copy (), ev_class);
}

void
Dispatcher::add_listener (SCM callback, SCM ev_class)
{
  internal_add_listener (callback, ev_class, ++priority_count_);
}

inline void
Dispatcher::internal_add_listener (SCM callback, SCM ev_class, int priority)
{
  SCM handle = scm_hashq_create_handle_x (listeners_, ev_class, SCM_EOL);
  SCM list = scm_cdr (handle);
  // if ev_class is not yet listened to, we go through our list of
  // source dispatchers and register ourselves there with the priority
  // we have reserved for this dispatcher.  The priority system
  // usually distributes events in the order events are registered.
  // The reuse of a previous priority when registering another event
  // for a dispatcher/dispatcher connection bypasses the normal
  // ordering, but it is the mechanism by which duplicate broadcasts
  // of the same event from one dispatcher to another are avoided.
  if (!scm_is_pair (list))
    {
      /* Tell all dispatchers that we listen to, that we want to hear ev_class
         events */
      for (SCM disp = dispatchers_; scm_is_pair (disp); disp = scm_cdr (disp))
        {
          int priority = from_scm<int> (scm_cdar (disp));
          Dispatcher *d = unsmob<Dispatcher> (scm_caar (disp));
          d->internal_add_listener (
            GET_LISTENER (this, dispatch).smobbed_copy (), ev_class, priority);
        }
      listen_classes_ = scm_cons (ev_class, listen_classes_);
    }
  SCM entry = scm_cons (to_scm (priority), callback);
  list = scm_merge (list, ly_list (entry), Lily::car_less);
  scm_set_cdr_x (handle, list);
}

void
Dispatcher::remove_listener (Listener l, SCM ev_class)
{
  SCM handle = scm_hashq_get_handle (listeners_, ev_class);

  if (scm_is_false (handle))
    {
      programming_error ("remove_listener called with incorrect class.");
      return;
    }

  SCM list = scm_cdr (handle);
  // We just remove the listener once.
  bool first = true;

  SCM dummy = scm_cons (SCM_EOL, list);
  SCM e = dummy;
  while (scm_is_pair (scm_cdr (e)))
    if (*unsmob<Listener> (scm_cdadr (e)) == l && first)
      {
        scm_set_cdr_x (e, scm_cddr (e));
        first = false;
        break;
      }
    else
      e = scm_cdr (e);
  list = scm_cdr (dummy);
  scm_set_cdr_x (handle, list);

  if (first)
    warning (_ ("Attempting to remove nonexisting listener."));
  else if (!scm_is_pair (list))
    {
      /* Unregister with all dispatchers. */
      for (SCM disp = dispatchers_; scm_is_pair (disp); disp = scm_cdr (disp))
        {
          Dispatcher *d = unsmob<Dispatcher> (scm_caar (disp));
          d->remove_listener (GET_LISTENER (this, dispatch), ev_class);
        }
      listen_classes_ = scm_delq_x (ev_class, listen_classes_);
    }
}

/* Register as a listener to another dispatcher. */
void
Dispatcher::register_as_listener (Dispatcher *disp)
{
  // We are creating and remembering the priority _we_ have with the
  // foreign dispatcher.  All events are dispatched with the same
  // priority.  The result is that, for example, a single event class
  // will only trigger an event listener once.
  int priority = ++disp->priority_count_;

  // Don't register twice to the same dispatcher.
  if (scm_is_true (scm_assq (disp->self_scm (), dispatchers_)))
    {
      warning (_ ("Already listening to dispatcher, ignoring request"));
      return;
    }

  dispatchers_ = scm_acons (disp->self_scm (), to_scm (priority), dispatchers_);

  SCM list = GET_LISTENER (this, dispatch).smobbed_copy ();
  for (SCM cl = listen_classes_; scm_is_pair (cl); cl = scm_cdr (cl))
    {
      disp->internal_add_listener (list, scm_car (cl), priority);
    }
}

/* Unregister as a listener to another dispatcher. */
void
Dispatcher::unregister_as_listener (Dispatcher *disp)
{
  dispatchers_ = scm_assq_remove_x (dispatchers_, disp->self_scm ());

  Listener listener = GET_LISTENER (this, dispatch);
  for (SCM cl = listen_classes_; scm_is_pair (cl); cl = scm_cdr (cl))
    {
      disp->remove_listener (listener, scm_car (cl));
    }
}
