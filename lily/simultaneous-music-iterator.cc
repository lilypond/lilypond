/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "simultaneous-music-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

using std::string;

Simultaneous_music_iterator::Simultaneous_music_iterator ()
{
  create_separate_contexts_ = false;
  children_list_ = SCM_EOL;
}

void
Simultaneous_music_iterator::derived_mark () const
{
  scm_gc_mark (children_list_);
}

void
Simultaneous_music_iterator::derived_substitute (Context *f, Context *t)
{
  for (SCM s = children_list_; scm_is_pair (s); s = scm_cdr (s))
    unsmob<Music_iterator> (scm_car (s))->substitute_outlet (f, t);
}

void
Simultaneous_music_iterator::construct_children ()
{
  int j = 0;

  SCM i = get_property (get_music (), "elements");

  children_list_ = SCM_EOL;
  SCM *tail = &children_list_;
  for (; scm_is_pair (i); i = scm_cdr (i), j++)
    {
      Music *mus = unsmob<Music> (scm_car (i));

      SCM scm_iter = get_static_get_iterator (mus);
      Music_iterator *mi = unsmob<Music_iterator> (scm_iter);

      Context *c = get_outlet ();
      if (j && create_separate_contexts_)
        {
          // create a new context of the same kind with the number as ID
          SCM name = ly_symbol2scm (c->context_name ().c_str ());
          string id = std::to_string (j);
          if (Context *other = c->find_create_context (CENTER, name, id,
                                                       SCM_EOL))
            {
              c = other;
            }
          else
            {
              warning (_f ("cannot find or create context: %s",
                           Context::diagnostic_id (name, id).c_str ()));
            }
        }

      mi->init_context (mus, c);
      mi->construct_children ();

      if (mi->ok ())
        {
          *tail = scm_cons (scm_iter, *tail);
          tail = SCM_CDRLOC (*tail);
        }
      else
        mi->quit ();
    }
}

// If we have some iterators with definite next moment and no of them
// remain after processing, we take the iterators with indefinite next
// moment along.  That makes sure that no Lyric_combine_music_iterator
// will outstay its welcome (issue 2010).

void
Simultaneous_music_iterator::process (Moment until)
{
  SCM *proc = &children_list_;
  bool finite = !pending_moment ().main_part_.is_infinity ();
  while (scm_is_pair (*proc))
    {
      Music_iterator *i = unsmob<Music_iterator> (scm_car (*proc));
      if (i->run_always () || i->pending_moment () == until)
        i->process (until);
      if (!i->ok ())
        {
          i->quit ();
          *proc = scm_cdr (*proc);
        }
      else
        {
          proc = SCM_CDRLOC (*proc);
        }
    }
  // If there were definite-ended iterators and all of them died, take
  // the rest of the iterators along with them.  They have
  // likely lost their reference iterators.  Basing this on the actual
  // music contexts is not reliable since something like
  // \new Voice = blah {
  //    << \context Voice = blah { c4 d }
  //       \addlyrics { oh no }
  //    >> e f
  // }
  // cannot wait for the death of context blah before ending the
  // simultaneous iterator.
  if (finite && pending_moment ().main_part_.is_infinity ())
    {
      for (SCM p = children_list_; scm_is_pair (p); p = scm_cdr (p))
        unsmob<Music_iterator> (scm_car (p))->quit ();
      children_list_ = SCM_EOL;
    }
}

Moment
Simultaneous_music_iterator::pending_moment () const
{
  Moment next (Rational::infinity ());

  for (SCM s = children_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Music_iterator *it = unsmob<Music_iterator> (scm_car (s));
      next = std::min (next, it->pending_moment ());
    }

  return next;
}

bool
Simultaneous_music_iterator::ok () const
{
  for (SCM s = children_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Music_iterator *it = unsmob<Music_iterator> (scm_car (s));
      if (it->ok ())
        return true;
    }
  return false;
}

bool
Simultaneous_music_iterator::run_always () const
{
  for (SCM s = children_list_; scm_is_pair (s); s = scm_cdr (s))
    {
      Music_iterator *it = unsmob<Music_iterator> (scm_car (s));
      if (it->run_always ())
        return true;
    }
  return false;
}

void
Simultaneous_music_iterator::do_quit ()
{
  for (SCM s = children_list_; scm_is_pair (s); s = scm_cdr (s))
    unsmob<Music_iterator> (scm_car (s))->quit ();
}

IMPLEMENT_CTOR_CALLBACK (Simultaneous_music_iterator);
