/*
  stream-event.cc -- implement Stream_event

  source file of the GNU LilyPond music typesetter

  (c) 2005-2006 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "stream-event.hh"

#include "ly-smobs.icc"
#include "context.hh"
#include "input.hh"
#include "input-smob.hh"

Stream_event::Stream_event ()
  : Prob (ly_symbol2scm ("Stream_event"), SCM_EOL)
{
}

Stream_event::Stream_event (SCM event_class, SCM mutable_props)
  : Prob (ly_symbol2scm ("Stream_event"),
	  scm_list_1 (scm_cons (ly_symbol2scm ("class"), event_class)))
{
  mutable_property_alist_ = mutable_props;
}

Stream_event::Stream_event (SCM property_alist)
  : Prob (ly_symbol2scm ("Stream_event"), SCM_EOL)
{
  mutable_property_alist_ = property_alist;
}

Stream_event::Stream_event (SCM class_name, Input *origin)
  : Prob (ly_symbol2scm ("Stream_event"),
	  scm_list_1 (scm_cons (ly_symbol2scm ("class"), class_name)))
{
  if (origin)
    set_spot (origin);
}

Stream_event::Stream_event (Stream_event *ev)
  : Prob (ly_symbol2scm ("Stream_event"), SCM_EOL)
{
  mutable_property_alist_ = scm_copy_tree (ev->mutable_property_alist_);
  immutable_property_alist_ = ev->immutable_property_alist_;
}

Input *
Stream_event::origin () const
{
  Input *i = unsmob_input (get_property ("origin"));
  return i ? i : &dummy_input_global;
}

void Stream_event::set_spot (Input *i)
{
  set_property ("origin", make_input (*i));
}

IMPLEMENT_TYPE_P (Stream_event, "ly:stream-event?");

MAKE_SCHEME_CALLBACK (Stream_event, undump, 1);
MAKE_SCHEME_CALLBACK (Stream_event, dump, 1);

SCM
Stream_event::dump (SCM self)
{
  Stream_event *ev = unsmob_stream_event (self);
  // Reversed alists look prettier.
  return scm_cons (scm_reverse (ev->immutable_property_alist_),
		   scm_reverse (ev->mutable_property_alist_));
}

SCM
Stream_event::undump (SCM data)
{
  Stream_event *obj = new Stream_event ();
  obj->immutable_property_alist_ = scm_reverse (scm_car (data));
  obj->mutable_property_alist_ = scm_reverse (scm_cdr (data));
  return obj->unprotect ();
}

Stream_event *
unsmob_stream_event (SCM m)
{
  return dynamic_cast<Stream_event*> (unsmob_prob (m));
}
