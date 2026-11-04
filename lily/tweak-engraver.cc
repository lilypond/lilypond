/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "context.hh"
#include "grob.hh"
#include "stream-event.hh"
#include "translator.icc"
#include <tuple>

class Tweak_engraver final : public Engraver
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
                    grobname = ly_symbol2scm (info.grob ()->name ());
                  if (scm_is_eq (scm_caaar (s), grobname))
                    {
                      if (scm_is_symbol (scm_cdaar (s)))
                        {
                          SCM sym = scm_cdaar (s);
                          SCM val = scm_cdar (s);
                          std::tie (sym, val) =
                            type_check_assignment (sym, val,
                                                   ly_symbol2scm ("backend-type?"));
                          if (scm_is_symbol (sym))
                            set_property (info.grob (), sym, val);
                        }
                      else
                        {
                          SCM path = scm_cdaar (s);
                          SCM sym = scm_car (path);
                          SCM val = scm_cdar (s);
                          SCM testval = nested_create_alist (scm_cdr (path), val);
                          std::tie (sym, testval) =
                            type_check_assignment (sym, testval,
                                                   ly_symbol2scm ("backend-type?"));
                          if (scm_is_symbol (sym))
                            set_nested_property (info.grob (), path, val);
                        }
                    }
                }
              else if (direct)
                {
                  SCM path = scm_cdaar (s);
                  SCM sym = scm_car (path);
                  SCM val = scm_cdar (s);
                  SCM testval = nested_create_alist (scm_cdr (path), val);
                  std::tie (sym, testval) =
                    type_check_assignment (sym, testval,
                                           ly_symbol2scm ("backend-type?"));
                  if (scm_is_symbol (sym))
                    set_nested_property (info.grob (), path, val);
                }
            }
          else if (direct)
            {
              SCM sym = scm_caar (s);
              SCM val = scm_cdar (s);
              std::tie (sym, val) =
                type_check_assignment (sym, val, ly_symbol2scm ("backend-type?"));
              if (scm_is_symbol (sym))
                set_property (info.grob (), sym, val);
            }
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
