/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "performer-group.hh"

#include "context.hh"
#include "audio-element.hh"
#include "warn.hh"

ADD_TRANSLATOR_GROUP (Performer_group,
                      /* doc */
                      R"(

                )",

                      /* create */
                      R"(

                )",

                      /* read */
                      R"(

                )",

                      /* write */
                      R"(

                )");

void
Performer_group::announce_element (Audio_element_info info)
{
  announce_infos_.push_back (info);
  Translator_group *t = context ()->get_parent ()->implementation ();

  if (Performer_group *eg = dynamic_cast<Performer_group *> (t))
    eg->announce_element (info);
}

void
Performer_group::acknowledge_audio_elements ()
{
  for (vsize j = 0; j < announce_infos_.size (); j++)
    {
      Audio_element_info info = announce_infos_[j];

      for (SCM p = get_simple_trans_list (); scm_is_pair (p); p = scm_cdr (p))
        {
          Translator *t = unsmob<Translator> (scm_car (p));
          Performer *eng = dynamic_cast<Performer *> (t);
          if (eng && eng != info.origin_trans_)
            eng->acknowledge_audio_element (info);
        }
    }
}

void
Performer_group::do_announces ()
{
  for (SCM s = context ()->children_contexts (); scm_is_pair (s);
       s = scm_cdr (s))
    {
      Context *c = unsmob<Context> (scm_car (s));
      Performer_group *group
        = dynamic_cast<Performer_group *> (c->implementation ());
      if (group)
        group->do_announces ();
    }

  while (1)
    {
      if (!announce_infos_.size ())
        break;

      acknowledge_audio_elements ();
      announce_infos_.clear ();
    }
}
