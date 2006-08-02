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
  VIRTUAL_COPY_CONSTRUCTOR (Stream_event, Stream_event);
  // todo: remove unneeded constructors
  Stream_event (SCM event_class, SCM mutable_props);
  Stream_event (SCM property_alist);
  Stream_event (SCM class_name, Input *);
  Stream_event (Stream_event *ev);

  Input *origin () const;
  void set_spot (Input *i);
  bool internal_in_event_class (SCM class_name);

  DECLARE_SCHEME_CALLBACK (undump, (SCM));
  DECLARE_SCHEME_CALLBACK (dump, (SCM));

};

#define in_event_class(class_name) internal_in_event_class (ly_symbol2scm (class_name))

Stream_event *unsmob_stream_event (SCM);
DECLARE_TYPE_P (Stream_event);

#endif /* STREAM_EVENT_HH */
