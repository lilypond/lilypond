/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music-iterator.hh"

#include "warn.hh"
#include "music.hh"
#include "context.hh"
#include "event-iterator.hh"
#include "input.hh"
#include "international.hh"
#include "ly-smob-list.hh"
#include "music-wrapper.hh"
#include "music-wrapper-iterator.hh"
#include "simple-music-iterator.hh"

#include <cstdio>

Music_iterator::Music_iterator ()
{
  smobify_self ();
}

Music_iterator::~Music_iterator () = default;

Moment
Music_iterator::pending_moment () const
{
  return Moment::infinity ();
}

void
Music_iterator::process (Moment)
{
}

SCM
Music_iterator::create_iterator (Music_iterator *parent, Music *m)
{
  Music_iterator *p = 0;

  SCM ctor = get_property (m, "iterator-ctor");
  SCM iter = SCM_EOL;
  if (ly_is_procedure (ctor))
    {
      iter = ly_call (ctor);
      p = unsmob<Music_iterator> (iter);
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

  p->parent_ = parent;
  p->music_ = m;
  assert (m);
  p->music_length_ = m->get_length ();
  p->start_mom_ = m->start_mom ();

  p->create_children ();

  return iter;
}

void
Music_iterator::init_context (Context *report)
{
  if (!get_own_context ())
    {
      set_own_context (report);
      create_contexts ();
    }
  else
    {
      programming_error ("context already initialized; skipping");
    }
}

void
Music_iterator::substitute_context (Context *f, Context *t)
{
  if (f != t)
    {
      if (get_own_context () == f)
        set_own_context (t);
    }
}

/* Descend to a bottom context; implicitly create a new one if necessary */
void
Music_iterator::descend_to_bottom_context ()
{
  assert (get_context ());
  if (!get_context ()->is_bottom_context ())
    set_context (get_context ()->get_default_interpreter ());
}

/* Concretely: If the current context is Global, descend to Score. */
void
Music_iterator::descend_to_user_accessible_context ()
{
  auto c = get_context ();
  assert (c);
  if (!c->is_accessible_to_user ())
    {
      c = c->get_user_accessible_interpreter ();
      if (c)
        set_context (c);
      else
        programming_error ("cannot find an accessible context");
    }
}

void
Music_iterator::report_event (Music *m)
{
  descend_to_bottom_context ();

  /*
    FIXME: then don't do it.
  */
  if (!m->is_mus_type ("event"))
    m->programming_error ("Sending non-event to context");

  m->send_to_context (get_context ());
}

IMPLEMENT_CTOR_CALLBACK (Music_iterator);

Music *
Music_iterator::get_music () const
{
  return music_;
}

Input *
Music_iterator::origin () const
{
  if (Music *m = get_music ())
    return m->origin ();
  return nullptr;
}

/****************************************************************/

const char *const Music_iterator::type_p_name_ = "ly:iterator?";

SCM
Music_iterator::mark_smob () const
{
  derived_mark ();
  /*
    Careful with GC, although we intend the following as pointers
    only, we _must_ mark them.
  */
  if (get_own_context ())
    scm_gc_mark (get_own_context ()->self_scm ());
  if (music_)
    scm_gc_mark (music_->self_scm ());

  return SCM_EOL;
}

int
Music_iterator::print_smob (SCM port, scm_print_state *) const
{
  char s[1000];

  sprintf (s, "#<%s>", class_name ());
  scm_puts (s, port);
  return 1;
}

void
Music_iterator::derived_mark () const
{
}

void
Music_iterator::quit ()
{
  do_quit ();
  set_own_context (nullptr);
}

void
Music_iterator::do_quit ()
{
}

bool
Music_iterator::run_always () const
{
  return false;
}

bool
is_child_context (Context *me, Context *child)
{
  while (child && child != me)
    child = child->get_parent ();

  return child == me;
}

/*
  move to context of child iterator if it is deeper down in the
  hierarchy.
*/
void
Music_iterator::descend_to_child (Context *child_report)
{
  Context *me_report = get_context ();
  if (is_child_context (me_report, child_report))
    set_context (child_report);
}

Music_iterator *
Music_iterator::find_above_by_music_type (SCM type_sym) const
{
  for (auto scope = const_cast<Music_iterator *> (this); scope;
       scope = scope->parent_)
    {
      if (auto m = scope->get_music ())
        {
          if (m->internal_is_music_type (type_sym))
            {
              return scope;
            }
        }
    }

  return nullptr;
}

Music_iterator *
Music_iterator::internal_where_defined (SCM sym, SCM *value_out) const
{
  // TODO: Abstract Context property features into a reusable base class?
  for (auto scope = const_cast<Music_iterator *> (this); scope;
       scope = scope->parent_)
    {
      if (auto m = scope->get_music ())
        {
          SCM val = get_property (m, sym);
          if (!scm_is_null (val))
            {
              if (value_out)
                *value_out = val;
              return scope;
            }
        }
    }

  if (value_out)
    *value_out = SCM_EOL;
  return nullptr;
}

Music_iterator *
Music_iterator::where_tagged (SCM tag_sym) const
{
  Music_iterator *scope = const_cast<Music_iterator *> (this);

  if (scm_is_symbol (tag_sym))
    {
      for (Music_iterator *candidate = scope; candidate; /**/)
        {
          SCM tags = SCM_EOL;
          candidate = where_defined (candidate, "tags", &tags);
          if (candidate)
            {
              if (scm_is_true (scm_member (tag_sym, tags)))
                {
                  scope = candidate;
                  break;
                }
              else
                candidate = candidate->get_parent ();
            }
        }
    }

  return scope;
}
