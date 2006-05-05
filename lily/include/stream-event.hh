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

  DECLARE_SCHEME_CALLBACK (undump, (SCM));
  DECLARE_SCHEME_CALLBACK (dump, (SCM));

  // todo: make Input mandatory.
  Stream_event (SCM property_alist);
  Stream_event (Context *c, SCM class_name);
  Stream_event (Context *c, Input *);
  Stream_event (Stream_event *ev);

  SCM internal_get_property (SCM) const;
  void internal_set_property (SCM prop, SCM val);

protected:
  DECLARE_SMOBS (Stream_event,);
};

DECLARE_UNSMOB (Stream_event, stream_event);
DECLARE_TYPE_P (Stream_event);

#define SEND_EVENT_TO_CONTEXT(ctx, cl, ...)				\
  {									\
    Stream_event *_e_ = new Stream_event (ctx, ly_symbol2scm (cl));	\
    __VA_ARGS__;							\
    ctx->event_source ()->distribute (_e_);				\
    scm_gc_unprotect_object (_e_->self_scm ());				\
  }
  
#define EVENT_PROPERTY(prop, val)	\
  (_e_->set_property (prop, val))

#endif /* STREAM_EVENT_HH */
