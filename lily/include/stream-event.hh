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

class Stream_event
{
  void init ();
  SCM property_alist_;
  Input *origin_;

public:
  Stream_event ();
  Input *origin () const;
  void set_spot (Input *i);

  DECLARE_SCHEME_CALLBACK (undump, (SCM));
  DECLARE_SCHEME_CALLBACK (dump, (SCM));

  // todo: remove unneeded constructors
  Stream_event (SCM property_alist);
  Stream_event (SCM class_name, Input *);
  Stream_event (Stream_event *ev);

  SCM internal_get_property (SCM) const;
  void internal_set_property (SCM prop, SCM val);

protected:
  DECLARE_SMOBS (Stream_event,);
};

DECLARE_UNSMOB (Stream_event, stream_event);
DECLARE_TYPE_P (Stream_event);

#endif /* STREAM_EVENT_HH */
