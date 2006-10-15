/*
  dispatcher.hh -- declare Dispatcher

  source file of the GNU LilyPond music typesetter

  (c) 2005 Erik Sandberg  <mandolaerik@gmail.com>
*/

#ifndef DISPATCHER_HH
#define DISPATCHER_HH

#include "listener.hh"
#include "stream-event.hh"

class Dispatcher
{
  /* Hash table. Each event-class maps to a list of listeners. */
  SCM listeners_;
  /* alist of dispatchers that we listen to. Each entry is a
     (dist . priority) pair. */
  SCM dispatchers_;
  SCM listen_classes_;
  DECLARE_LISTENER (dispatch);
  /* priority counter. Listeners with low priority receive events
     first. */
  int priority_count_;
  void internal_add_listener (Listener, SCM event_class, int priority);
public:
  Dispatcher ();
  void broadcast (Stream_event *ev);
  void add_listener (Listener, SCM event_class);
  void remove_listener (Listener, SCM event_class);
  void register_as_listener (Dispatcher *dist);
  void unregister_as_listener (Dispatcher *dist);
protected:
  DECLARE_SMOBS (Dispatcher);
};

DECLARE_UNSMOB (Dispatcher, dispatcher);

#endif // DISPATCHER_HH
