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

#include "simple-music-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

class Change_iterator final : public Simple_music_iterator
{
public:
  /* constructor is public */
  void process (Moment) override;
  DECLARE_SCHEME_CALLBACK (constructor, ());
  OVERRIDE_CLASS_NAME (Change_iterator);

private:
  static void change (Music_iterator *it, Context *source, Context *target);
  Music_iterator *find_scope (SCM tag);
};

void
Change_iterator::change (Music_iterator *it, Context *source, Context *target)
{
  if (it->get_context () == source)
    {
      // The iterator's immediate context is the one to be changed.  This can't
      // be done by pruning and grafting contexts; the iterator must be changed
      // to refer to the new context.
      it->substitute_context (source, target);
    }
  else
    {
      // The iterator's context might be a descendant of the one to be changed.
      // Find the branch to prune, if any, and announce the change.
      for (auto *branch = it->get_context ();
           auto *const parent = branch->get_parent (); branch = parent)
        {
          if (parent == target)
            {
              break; // already in its proper place
            }

          if (parent == source)
            {
              send_stream_event (branch, "ChangeParent", it->origin (),
                                 ly_symbol2scm ("context"), to_scm (target));
              break;
            }
        }
    }
}

void
Change_iterator::process (Moment m)
{
  SCM to_type = get_property (get_music (), "change-to-type");
  auto to_id = ly_scm2string (get_property (get_music (), "change-to-id"));

  // Find the context to change from.
  auto *const last = get_context ()->find_context (UP, to_type, "");
  if (last)
    {
      // Find the context to change to.
      auto *const dest = find_context_near (get_context (), to_type, to_id);
      if (dest)
        {
          auto *scope
            = where_tagged (get_property (get_music (), "change-tag"));
          scope->preorder_walk ([last, dest] (Music_iterator *iter) {
            Change_iterator::change (iter, last, dest);
          });
        }
      else
        {
          warning (_f ("cannot find context to change to: %s",
                       Context::diagnostic_id (to_type, to_id).c_str ()));
        }
    }
  else
    {
      warning (_f ("cannot find context to change from: %s",
                   Context::diagnostic_id (to_type, "").c_str ()));
    }

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Change_iterator);
