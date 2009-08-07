/*
  dispatcher.cc -- implement Dispatcher

  source file of the GNU LilyPond music typesetter

  (c) 2005-2006 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "dispatcher.hh"
#include "input.hh"
#include "international.hh"
#include "ly-smobs.icc"
#include "warn.hh"

IMPLEMENT_SMOBS (Dispatcher);
IMPLEMENT_TYPE_P (Dispatcher, "ly:dispatcher?");
IMPLEMENT_DEFAULT_EQUAL_P (Dispatcher);

Dispatcher::~Dispatcher ()
{
}

Dispatcher::Dispatcher ()
{
  self_scm_ = SCM_EOL;
  listeners_ = SCM_EOL;
  dispatchers_ = SCM_EOL;
  listen_classes_ = SCM_EOL;
  smobify_self ();
// TODO: use resizable hash (guile 1.8)
//  listeners_ = scm_c_make_hash_table (0);
  listeners_ = scm_c_make_hash_table (17);
  priority_count_ = 0;
}

SCM
Dispatcher::mark_smob (SCM sm)
{
  Dispatcher *me = (Dispatcher *) SCM_CELL_WORD_1 (sm);
  scm_gc_mark (me->dispatchers_);
  scm_gc_mark (me->listen_classes_);
  return me->listeners_;
}

int
Dispatcher::print_smob (SCM s, SCM p, scm_print_state*)
{
  Dispatcher *me = (Dispatcher *) SCM_CELL_WORD_1 (s);
  scm_puts ("#<Dispatcher ", p);
  scm_write (scm_vector_to_list (me->listeners_), p);
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
*/
IMPLEMENT_LISTENER (Dispatcher, dispatch);
void
Dispatcher::dispatch (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  SCM class_symbol = ev->get_property ("class");
  if (!scm_symbol_p (class_symbol))
    {
      warning (_ ("Event class should be a symbol"));
      return;
    }

  SCM class_list = scm_call_1 (ly_lily_module_constant ("ly:make-event-class"), class_symbol);
  if (!scm_is_pair (class_list))
    {
      ev->origin ()->warning (_f ("Unknown event class %s", ly_symbol2string (class_symbol).c_str ()));
      return;
    }
  bool sent = false;
  int num_classes = scm_ilength (class_list);

  /*
    For each event class there is a list of listeners, which is
    ordered by priority. Our next task is to call these listeners, in
    priority order.  A priority queue stores the next element in each
    listener list, and the lowest priority element is repeatedly
    extracted and called.

    The priority queue is implemented as a bubble-sorted C
    array. Using the stack instead of native Scheme datastructures
    avoids overheads for memory allocation. The queue is usually small
    (around 2 elements), so the quadratic sorting time is not a
    problem. (if this changes, it's easy to rewrite this routine using
    a heap)

    The first step is to collect all listener lists and to initially
    insert them in the priority queue.
  */
  struct { int prio; SCM list; } lists[num_classes+1];
  int i = 0;
  for (SCM cl = class_list; scm_is_pair (cl); cl = scm_cdr (cl))
    {
      SCM list = scm_hashq_ref (listeners_, scm_car (cl), SCM_EOL);
      if (!scm_is_pair (list))
	num_classes--;
      else
	{
          // bubblesort.
          int prio = scm_to_int (scm_caar (list));
	  int j;
	  for (j = i; j > 0 && lists[j-1].prio > prio; j--)
	    lists[j] = lists[j-1];
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

          Listener *l = unsmob_listener (scm_cdar (lists[0].list));
          l->listen (ev->self_scm ());
          sent = true;
        }
      // go to the next listener; bubble-sort the class list.
      SCM next = scm_cdr (lists[0].list);
      if (!scm_is_pair (next))
        num_classes--;
      int prio = (scm_is_pair (next)) ? scm_to_int (scm_caar (next)) : INT_MAX;
      for (i = 0; prio > lists[i+1].prio; i++)
        lists[i] = lists[i+1];
      lists[i].prio = prio;
      lists[i].list = next;
    }

/* TODO: Uncomment.
  if (!sent)
    warning (_f ("Junking event: %s", ly_symbol2string (class_symbol).c_str ()));
*/
}

void
Dispatcher::broadcast (Stream_event *ev)
{
  dispatch (ev->self_scm ());
}

void
Dispatcher::add_listener (Listener l, SCM ev_class)
{
  internal_add_listener (l, ev_class, ++priority_count_);
}

inline void
Dispatcher::internal_add_listener (Listener l, SCM ev_class, int priority)
{
  SCM list = scm_hashq_ref (listeners_, ev_class, SCM_EOL);
  if (!scm_is_pair (list))
    {
      /* Tell all dispatchers that we listen to, that we want to hear ev_class 
         events */
      for (SCM disp = dispatchers_; scm_is_pair (disp); disp = scm_cdr (disp))
	{
	  int priority = scm_to_int (scm_cdar (disp));
	  Dispatcher *d = unsmob_dispatcher (scm_caar (disp));
	  d->internal_add_listener (GET_LISTENER (dispatch), ev_class, priority);
	}
      listen_classes_ = scm_cons (ev_class, listen_classes_);
    }
  SCM entry = scm_cons (scm_int2num (priority), l.smobbed_copy ());
  list = scm_merge (list, scm_list_1 (entry), ly_lily_module_constant ("car<"));
  scm_hashq_set_x (listeners_, ev_class, list);
}

void
Dispatcher::remove_listener (Listener l, SCM ev_class)
{
  SCM list = scm_hashq_ref (listeners_, ev_class, SCM_EOL);

  if (list == SCM_EOL)
    {
      programming_error ("remove_listener called with incorrect class.");
      return;
    }

  // We just remove the listener once.
  bool first = true;

  SCM dummy = scm_cons (SCM_EOL, list);
  SCM e = dummy;
  while (scm_is_pair (scm_cdr (e)))
    if (*unsmob_listener (scm_cdadr (e)) == l && first)
      {
	scm_set_cdr_x (e, scm_cddr (e));
	first = false;
	break;
      }
    else
      e = scm_cdr (e);
  list = scm_cdr (dummy);
  scm_hashq_set_x (listeners_, ev_class, list);

  if (first)
    warning ("Attempting to remove nonexisting listener.");
  else if (!scm_is_pair (list))
    {
      /* Unregister with all dispatchers. */
      for (SCM disp = dispatchers_; scm_is_pair (disp); disp = scm_cdr (disp))
	{
	  Dispatcher *d = unsmob_dispatcher (scm_caar (disp));
	  d->remove_listener (GET_LISTENER (dispatch), ev_class);
	}
      listen_classes_ = scm_delq_x (ev_class, listen_classes_);
    }
}

/* Register as a listener to another dispatcher. */
void
Dispatcher::register_as_listener (Dispatcher *disp)
{
  int priority = ++disp->priority_count_;

  // Don't register twice to the same dispatcher.
  if (scm_assq (disp->self_scm (), dispatchers_) != SCM_BOOL_F)
    {
      warning ("Already listening to dispatcher, ignoring request");
      return;
    }

  dispatchers_ = scm_acons (disp->self_scm (), scm_int2num (priority), dispatchers_);

  Listener list = GET_LISTENER (dispatch);
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

  Listener listener = GET_LISTENER (dispatch);
  for (SCM cl = listen_classes_; scm_is_pair (cl); cl = scm_cdr (cl))
    {
      disp->remove_listener (listener, scm_car (cl));
    }
}
