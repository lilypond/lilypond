/*
  stream-event.hh -- declare Stream_event

  source file of the GNU LilyPond music typesetter

  (c) 2005-2006 Erik Sandberg <mandolaerik@gmail.com>
*/

#ifndef STREAM_EVENT_HH
#define STREAM_EVENT_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "prob.hh"

class Stream_event : public Prob
{
public:
  Stream_event ();
  Input *origin () const;
  void set_spot (Input *i);

  DECLARE_SCHEME_CALLBACK (undump, (SCM));
  DECLARE_SCHEME_CALLBACK (dump, (SCM));

  // todo: remove unneeded constructors
  Stream_event (SCM event_class, SCM mutable_props);
  Stream_event (SCM property_alist);
  Stream_event (SCM class_name, Input *);
  Stream_event (Stream_event *ev);
};

Stream_event *unsmob_stream_event (SCM);
DECLARE_TYPE_P (Stream_event);

#endif /* STREAM_EVENT_HH */
