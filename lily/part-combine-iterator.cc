/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys

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
#include "music-sequence.hh"
#include "simultaneous-music-iterator.hh"
#include "warn.hh"
#include "lily-imports.hh"

class Part_combine_iterator final : public Simultaneous_music_iterator
{
public:
  Part_combine_iterator () = default;

  DECLARE_SCHEME_CALLBACK (constructor, ());

protected:
  void derived_mark () const override;

  void process (Moment) override;

private:
  bool is_active_context (const Context *c) const;
  void kill_mmrest (Context *c);

private:
  Stream_event *mmrest_event_ = nullptr;
};

void
Part_combine_iterator::derived_mark () const
{
  Simultaneous_music_iterator::derived_mark ();

  if (mmrest_event_)
    scm_gc_mark (mmrest_event_->self_scm ());
}

bool
Part_combine_iterator::is_active_context (const Context *c) const
{
  for (auto *child : get_children ())
    {
      if (child->get_context () == c)
        return true;
    }

  return false;
}

void
Part_combine_iterator::kill_mmrest (Context *c)
{
  if (!mmrest_event_)
    {
      mmrest_event_ = new Stream_event (
        Lily::ly_make_event_class (ly_symbol2scm ("multi-measure-rest-event")));
      set_property (mmrest_event_, "duration", SCM_EOL);
      mmrest_event_->unprotect ();
    }

  c->event_source ()->broadcast (mmrest_event_);
}

void
Part_combine_iterator::process (Moment m)
{
  // Catalog the contexts that the part iterators were previously sending
  // events to.
  ly_smob_list<Context> prev_active_contexts;
  auto tail = prev_active_contexts.begin ();
  for (auto *child : get_children ())
    {
      if (auto *c = child->get_context ())
        {
          tail = prev_active_contexts.insert_before (tail, c);
          ++tail;
        }
    }

  // Run the part iterators.  They may change contexts.
  Simultaneous_music_iterator::process (m);

  // Kill multi-measure rests in contexts that were previously active and
  // are no longer active.
  for (auto *c : prev_active_contexts)
    {
      if (!is_active_context (c))
        kill_mmrest (c);
    }
}

IMPLEMENT_CTOR_CALLBACK (Part_combine_iterator);
