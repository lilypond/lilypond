/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys

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

#include "context.hh"
#include "dispatcher.hh"
#include "lily-guile.hh"
#include "music.hh"
#include "music-iterator.hh"
#include "music-sequence.hh"
#include "warn.hh"
#include "lily-imports.hh"

class Part_combine_iterator final : public Music_iterator
{
public:
  Part_combine_iterator ();

  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  void derived_substitute (Context *f, Context *t) override;
  void derived_mark () const override;

  void construct_children () override;
  Moment pending_moment () const override;
  void do_quit () override;
  void process (Moment) override;

private:
  static const size_t NUM_PARTS = 2;
  Music_iterator *iterators_[NUM_PARTS];

  Stream_event *mmrest_event_;

  bool is_active_outlet (const Context *c) const;
  void kill_mmrest (Context *c);
};

const size_t Part_combine_iterator::NUM_PARTS;

void
Part_combine_iterator::do_quit ()
{
  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i])
      iterators_[i]->quit ();
}

Part_combine_iterator::Part_combine_iterator ()
{
  mmrest_event_ = 0;

  for (size_t i = 0; i < NUM_PARTS; i++)
    iterators_[i] = 0;
}

void
Part_combine_iterator::derived_mark () const
{
  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i])
      scm_gc_mark (iterators_[i]->self_scm ());

  if (mmrest_event_)
    scm_gc_mark (mmrest_event_->self_scm ());
}

void
Part_combine_iterator::derived_substitute (Context *f,
                                           Context *t)
{
  // (Explain why just iterators_[0].)
  if (iterators_[0])
    iterators_[0]->substitute_outlet (f, t);
}

Moment
Part_combine_iterator::pending_moment () const
{
  Moment p (Rational::infinity ());

  for (size_t i = 0; i < NUM_PARTS; i++)
    p = std::min (p, iterators_[i]->pending_moment ());

  return p;
}

bool Part_combine_iterator::is_active_outlet (const Context *c) const
{
  for (size_t i = 0; i < NUM_PARTS; i++)
    if (iterators_[i] && (iterators_[i]->get_outlet () == c))
      return true;

  return false;
}

void
Part_combine_iterator::kill_mmrest (Context *c)
{

  if (!mmrest_event_)
    {
      mmrest_event_ = new Stream_event
      (Lily::ly_make_event_class (ly_symbol2scm ("multi-measure-rest-event")));
      set_property (mmrest_event_, "duration", SCM_EOL);
      mmrest_event_->unprotect ();
    }

  c->event_source ()->broadcast (mmrest_event_);
}

void
Part_combine_iterator::construct_children ()
{
  Music_iterator::construct_children ();

  SCM lst = get_property (get_music (), "elements");
  iterators_[0] = unsmob<Music_iterator> (get_iterator (unsmob<Music> (scm_car (lst))));
  iterators_[1] = unsmob<Music_iterator> (get_iterator (unsmob<Music> (scm_cadr (lst))));
}

void
Part_combine_iterator::process (Moment m)
{
  Context *prev_active_outlets[NUM_PARTS];
  bool any_outlet_changed = false;
  for (size_t i = 0; i < NUM_PARTS; i++)
    {
      prev_active_outlets[i] = iterators_[i]->get_outlet ();

      if (iterators_[i]->ok ())
        iterators_[i]->process (m);

      if (prev_active_outlets[i] != iterators_[i]->get_outlet ())
        any_outlet_changed = true;
    }

  if (any_outlet_changed)
    {
      // Kill multi-measure rests in outlets that were previously active and
      // are no longer active.
      for (size_t i = 0; i < NUM_PARTS; i++)
        {
          Context *c = prev_active_outlets[i];
          if (c && !is_active_outlet (c))
            kill_mmrest (c);
        }
    }
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_iterator);
