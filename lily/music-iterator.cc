/*
  music-iterator.cc -- implement Music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/


#include <cstdio>
using namespace std;

#include "warn.hh"
#include "music.hh"
#include "context.hh"
#include "event-iterator.hh"
#include "input.hh"
#include "international.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "simple-music-iterator.hh"

#include "ly-smobs.icc"

Music_iterator::Music_iterator ()
{
  music_ = 0;
  smobify_self ();
}

Music_iterator::Music_iterator (Music_iterator const &)
{
  assert (false);
}

Music_iterator::~Music_iterator ()
{
}

Context *
Music_iterator::get_outlet () const
{
  return handle_.get_outlet ();
}

void
Music_iterator::set_context (Context *trans)
{
  handle_.set_context (trans);
}

void
Music_iterator::construct_children ()
{
}

Moment
Music_iterator::pending_moment () const
{
  return 0;
}

void
Music_iterator::process (Moment)
{
}

bool
Music_iterator::ok () const
{
  return false;
}

SCM
Music_iterator::get_static_get_iterator (Music *m)
{
  Music_iterator *p = 0;

  SCM ctor = m->get_property ("iterator-ctor");
  SCM iter = SCM_EOL;
  if (ly_is_procedure (ctor))
    {
      iter = scm_call_0 (ctor);
      p = unsmob_iterator (iter);
    }
  else
    {
      if (dynamic_cast<Music_wrapper *> (m))
	p = new Music_wrapper_iterator;
      else if (m->is_mus_type ("event"))
        p = new Event_iterator;
      else
	p = new Simple_music_iterator;

      iter = p->self_scm ();
      p->unprotect ();
    }

  p->music_ = m;
  assert (m);
  p->music_length_ = m->get_length ();
  p->start_mom_ = m->start_mom ();

  return iter;
}

Moment
Music_iterator::music_get_length () const
{
  return music_length_;
}

Moment
Music_iterator::music_start_mom ()const
{
  return start_mom_;
}

void
Music_iterator::init_context (Music *m, Context *report)
{
  music_ = m;
  assert (m);
  if (! get_outlet ())
    set_context (report);
}

void
Music_iterator::substitute_outlet (Context *f, Context *t)
{
  if (get_outlet () == f)
    set_context (t);
  derived_substitute (f, t);
}

void
Music_iterator::derived_substitute (Context *, Context *)
{
}

SCM
Music_iterator::get_iterator (Music *m) const
{
  SCM ip = get_static_get_iterator (m);
  Music_iterator *p = unsmob_iterator (ip);

  p->init_context (m, get_outlet ());

  p->construct_children ();
  return ip;
}

/* Descend to a bottom context; implicitly create a new one if necessary */
void
Music_iterator::descend_to_bottom_context ()
{
  assert (get_outlet ());
  if (!get_outlet ()->is_bottom_context ())
    set_context (get_outlet ()->get_default_interpreter ());
}

void 
Music_iterator::report_event (Music *m)
{
  descend_to_bottom_context ();

  /*
    FIXME: then don't do it. 
  */
  if (!m->is_mus_type ("event"))
    m->origin ()->programming_error (_ ("Sending non-event to context"));

  m->send_to_context (get_outlet ());
}

IMPLEMENT_CTOR_CALLBACK (Music_iterator);

Music *
Music_iterator::get_music () const
{
  return music_;
}

/****************************************************************/

IMPLEMENT_TYPE_P (Music_iterator, "ly:iterator?");
IMPLEMENT_SMOBS (Music_iterator);
IMPLEMENT_DEFAULT_EQUAL_P (Music_iterator);

SCM
Music_iterator::mark_smob (SCM smob)
{
  Music_iterator *mus = (Music_iterator *)SCM_CELL_WORD_1 (smob);

  mus->derived_mark ();
  /*
    Careful with GC, although we intend the following as pointers
    only, we _must_ mark them.
  */
  if (mus->get_outlet ())
    scm_gc_mark (mus->get_outlet ()->self_scm ());
  if (mus->music_)
    scm_gc_mark (mus->music_->self_scm ());

  return SCM_EOL;
}

int
Music_iterator::print_smob (SCM sm, SCM port, scm_print_state*)
{
  char s[1000];

  Music_iterator *iter = unsmob_iterator (sm);
  sprintf (s, "#<%s>", iter->class_name ());
  scm_puts (s, port);
  return 1;
}

void
Music_iterator::derived_mark ()const
{
}

void
Music_iterator::quit ()
{
  do_quit ();
  handle_.set_context (0);
}

void
Music_iterator::do_quit ()
{
}

bool
Music_iterator::run_always ()const
{
  return false;
}

bool
is_child_context (Context *me, Context *child)
{
  while (child && child != me)
    child = child->get_parent_context ();

  return child == me;
}

/*
  move to context of child iterator if it is deeper down in the
  hierarchy.
*/
void
Music_iterator::descend_to_child (Context *child_report)
{
  Context *me_report = get_outlet ();
  if (is_child_context (me_report, child_report))
    set_context (child_report);
}
