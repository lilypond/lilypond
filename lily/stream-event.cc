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

Stream_event::~Stream_event ()
{
}

void
Stream_event::init ()
{
  self_scm_ = SCM_EOL;
  property_alist_ = SCM_EOL;

  smobify_self ();
}

Stream_event::Stream_event ()
{
  init ();
}

Stream_event::Stream_event (SCM property_alist)
{
  init ();
  property_alist_ = property_alist;
}

/*
   Hm. Perhaps Stream_event should be a prob, with class_name as an
   immutable property?
 */
Stream_event::Stream_event (SCM class_name, Input *origin)
{
  init ();
  set_property ("class", class_name);
  if (origin)
    set_spot (origin);
}

Stream_event::Stream_event (Stream_event *ev)
{
  init ();
  property_alist_ = scm_copy_tree (ev->property_alist_);
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

SCM
Stream_event::mark_smob (SCM sm)
{
  Stream_event *me = (Stream_event *) SCM_CELL_WORD_1 (sm);
  return me->property_alist_;
}

int
Stream_event::print_smob (SCM s, SCM port, scm_print_state *)
{
  scm_puts ("#<Stream_event ", port);
  scm_write (dump (s), port);
  scm_puts (" >", port);
  return 1;
}

IMPLEMENT_SMOBS (Stream_event);
IMPLEMENT_DEFAULT_EQUAL_P (Stream_event);
IMPLEMENT_TYPE_P (Stream_event, "ly:stream-event?");

MAKE_SCHEME_CALLBACK (Stream_event, undump, 1);
MAKE_SCHEME_CALLBACK (Stream_event, dump, 1);

SCM
Stream_event::dump (SCM self)
{
  Stream_event *ev = unsmob_stream_event (self);
  // Reversed alists look prettier.
  return scm_reverse (ev->property_alist_);
}

SCM
Stream_event::undump (SCM data)
{
  Stream_event *obj = new Stream_event ();
  obj->property_alist_ = scm_reverse (data);
  return obj->unprotect ();
}

SCM
Stream_event::internal_get_property (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, property_alist_);
  if (s != SCM_BOOL_F)
    return scm_cdr (s);
  return SCM_EOL;
}

void 
Stream_event::internal_set_property (SCM prop, SCM val)
{
  property_alist_ = scm_assq_set_x (property_alist_, prop, val);
}
