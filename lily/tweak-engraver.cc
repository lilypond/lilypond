/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "engraver.hh"

#include "grob.hh"
#include "stream-event.hh"
#include "translator.icc"

class Tweak_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Tweak_engraver);

protected:
  void acknowledge_grob (Grob_info) override;
};

Tweak_engraver::Tweak_engraver (Context *c)
  : Engraver (c)
{
}

void
Tweak_engraver::acknowledge_grob (Grob_info info)
{
  Stream_event *ev = info.event_cause ();
  bool direct = ev;
  SCM grobname = SCM_UNDEFINED;
  if (!direct)
    ev = info.ultimate_event_cause ();
  if (ev)
    {
      // Each tweak conses an address and a value.
      // The address has one of the following forms:
      // symbol -> direct tweak
      // (grob . symbol) -> targeted tweak
      // (#t . symbol-path) -> direct nested tweak
      // (grob . symbol-path) -> targeted nested tweak
      for (SCM s = get_property (ev, "tweaks"); scm_is_pair (s);
           s = scm_cdr (s))
        {
          if (scm_is_pair (scm_caar (s)))
            {
              if (scm_is_symbol (scm_caaar (s)))
                {
                  if (SCM_UNBNDP (grobname))
                    grobname
                      = scm_from_utf8_symbol (info.grob ()->name ().c_str ());
                  if (scm_is_eq (scm_caaar (s), grobname))
                    {
                      if (scm_is_symbol (scm_cdaar (s)))
                        set_property (info.grob (), scm_cdaar (s),
                                      scm_cdar (s));
                      else
                        set_nested_property (info.grob (), scm_cdaar (s),
                                             scm_cdar (s));
                    }
                }
              else if (direct)
                set_nested_property (info.grob (), scm_cdaar (s), scm_cdar (s));
            }
          else if (direct)
            set_property (info.grob (), scm_caar (s), scm_cdar (s));
        }
    }
}

void
Tweak_engraver::boot ()
{
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Tweak_engraver,
                /* doc */
                R"(
Read the @code{tweaks} property from the originating event, and set properties.
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
