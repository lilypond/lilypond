/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "engraver.hh"
#include "lily-imports.hh"
#include "translator-group.hh"

#include "translator.icc"

#include <string>
#include <utility>

enum class BarType
{
  // from low to high priority
  NONE = 0,
  DEFAULT,
  OTHER
};

/*
  Objective:

  -- set and reset repeatCommands, so Unfolded_repeat_iterator knows
  where to set variables.

  -- collect information passed by Unfolded_repeat_iterator for
  Bar_engraver: writes whichBar property. (TODO: check for
  interactions with timing engraver.)
*/
class Repeat_acknowledge_engraver : public Engraver
{
public:

  TRANSLATOR_DECLARATIONS (Repeat_acknowledge_engraver);
protected:
  void listen_fine (Stream_event *);
  void listen_section (Stream_event *);
  void listen_segno_mark (Stream_event *);
  void listen_volta_span (Stream_event *);
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
  void initialize () override;

private:
  bool first_time_ = true;
  bool heard_fine_ = false;
  bool heard_section_ = false;
  bool heard_segno_mark_ = false;
  bool heard_volta_span_ = false;
};

void
Repeat_acknowledge_engraver::initialize ()
{
  set_property (context (), "repeatCommands", SCM_EOL);
}

Repeat_acknowledge_engraver::Repeat_acknowledge_engraver (Context *c)
  : Engraver (c)
{
}

void
Repeat_acknowledge_engraver::start_translation_timestep ()
{
  auto *tr = where_defined (context (), "repeatCommands");
  if (!tr)
    tr = context ();

  set_property (tr, "repeatCommands", SCM_EOL);

  heard_fine_ = false;
  heard_section_ = false;
  heard_segno_mark_ = false;
  heard_volta_span_ = false;
}

void
Repeat_acknowledge_engraver::listen_fine (Stream_event *)
{
  heard_fine_ = true;
}

void
Repeat_acknowledge_engraver::listen_section (Stream_event *)
{
  heard_section_ = true;
}

void
Repeat_acknowledge_engraver::listen_segno_mark (Stream_event *)
{
  heard_segno_mark_ = true;
}

void
Repeat_acknowledge_engraver::listen_volta_span (Stream_event *)
{
  heard_volta_span_ = true;
}

void
Repeat_acknowledge_engraver::process_music ()
{
  /*
    At the start of a piece, we don't print any repeat bars.
  */
  if (first_time_)
    return;

  SCM cs = get_property (this, "repeatCommands");

  bool start = false;
  bool end = false;
  bool volta_found = heard_volta_span_;
  while (scm_is_pair (cs))
    {
      SCM command = scm_car (cs);
      if (scm_is_eq (command, ly_symbol2scm ("start-repeat")))
        start = true;
      else if (scm_is_eq (command, ly_symbol2scm ("end-repeat")))
        end = true;
      else if (scm_is_pair (command)
               && scm_is_eq (scm_car (command), ly_symbol2scm ("volta")))
        volta_found = true;
      cs = scm_cdr (cs);
    }

  // TODO: Implement a mark style of segno with (probably) the Mark_engraver
  // creating the grob.  In that case, the Repeat_acknowledge_engraver would
  // not add a segno to the bar line, but would still use underlyingBarType.
  // Until then, we add the segno to the bar line whenever we hear the event.
  const bool segno = heard_segno_mark_;

  auto forced_bar_type = BarType::NONE;
  SCM wb = get_property (this, "whichBar");
  if (scm_is_string (wb))
    {
      // Note that we don't distinguish between a default bar set by the
      // Default_bar_line_engraver and a default bar set by the user.
      forced_bar_type = ly_is_equal (wb, get_property (this, "defaultBarType"))
                        ? BarType::DEFAULT
                        : BarType::OTHER;
    }

  /*
    We only set the barline if we wouldn't overwrite a previously set
    barline.
  */
  if (forced_bar_type <= BarType::DEFAULT)
    {
      bool has_repeat_bar = false;
      std::string rb;

      // TODO: Move this jenga tower into a Scheme callback if further
      // customizability is desired.  The number of dimensions makes it a
      // hassle to maintain a built-in context property for every combination.
      // Don't pass the state as parameters: set context properties before
      // calling.  (Well, some of these already came from repeatCommands, for
      // what that's worth.)
      if (segno)
        {
          if (start)
            {
              if (end) // { segno, start, end }
                {
                  SCM s = get_property (this, "doubleRepeatSegnoType");
                  rb = robust_scm2string (s, ":|.S.|:");
                  has_repeat_bar = true;
                }
              else // { segno, start }
                {
                  if (heard_fine_)
                    {
                      SCM s = get_property (this, "fineStartRepeatSegnoType");
                      rb = robust_scm2string (s, "|.S.|:");
                      has_repeat_bar = true;
                    }
                  else
                    {
                      SCM s = get_property (this, "startRepeatSegnoType");
                      rb = robust_scm2string (s, "S.|:");
                      has_repeat_bar = true;
                    }
                }
            }
          else if (end) // { segno, end }
            {
              SCM s = get_property (this, "endRepeatSegnoType");
              rb = robust_scm2string (s, ":|.S");
              has_repeat_bar = true;
            }
          else // { segno }
            {
              if (heard_fine_)
                {
                  SCM s = get_property (this, "fineSegnoType");
                  rb = robust_scm2string (s, "|.S");
                  has_repeat_bar = true;
                }
              else
                {
                  SCM s = get_property (this, "segnoType");
                  rb = robust_scm2string (s, "S");
                  has_repeat_bar = true;
                }
            }
        }
      else if (start)
        {
          if (end) // { start, end }
            {
              SCM s = get_property (this, "doubleRepeatType");
              rb = robust_scm2string (s, ":..:");
              has_repeat_bar = true;
            }
          else // { start }
            {
              SCM s = get_property (this, "startRepeatType");
              rb = robust_scm2string (s, ".|:");
              has_repeat_bar = true;
            }
        }
      else if (end) // { end }
        {
          SCM s = get_property (this, "endRepeatType");
          rb = robust_scm2string (s, ":|.");
          has_repeat_bar = true;
        }

      bool has_underlying_bar = false;
      std::string ub;
      if (heard_fine_)
        {
          ub = robust_scm2string (get_property (this, "fineBarType"), "|.");
          has_underlying_bar = true;
        }
      else if (heard_section_)
        {
          ub = robust_scm2string (get_property (this, "sectionBarType"), "||");
          has_underlying_bar = true;
        }
      else if ((heard_segno_mark_ || has_repeat_bar)
               && (forced_bar_type < BarType::DEFAULT))
        {
          // At points of repetition or departure where there wouldn't
          // otherwise be a bar line, print a thin double bar line (Behind
          // Bars, p.240).
          SCM s = get_property (this, "underlyingRepeatType");
          ub = robust_scm2string (s, "||");
          has_underlying_bar = true;
        }

      if (has_repeat_bar)
        {
          if (has_underlying_bar)
            {
              // The repeat bar has priority, but we append the underlying bar
              // as an annotation so that it can be made to appear at the end
              // of the previous line if the line is later broken here.  (This
              // can support other cases too, but that is the motivating case.)
              constexpr auto annotation_char = '-';
              auto annotated_rb = rb + annotation_char + ub;

              // Try to keep out of the way when a user overrides the default
              // repeat bar types.  For example, if the user set
              // startRepeatType to ".|:-||", appending the default value of
              // underlyingRepeatType would make it ".|:-||-||", which isn't in
              // the built-in set of bar lines.  Rather than setting whichBar
              // to a useless value, discard the annotation.
              if (!scm_is_null (ly_assoc_get (ly_string2scm (annotated_rb),
                                              Lily::bar_glyph_alist,
                                              SCM_EOL)))
                {
                  rb = std::move (annotated_rb);
                }
            }
        }
      else if (has_underlying_bar) // visible because there is no repeat bar
        {
          rb = ub;
          has_repeat_bar = true;
        }
      else if (volta_found && (forced_bar_type == BarType::NONE))
        {
          // Volta brackets align on bar lines, so create an empty bar line
          // where there isn't already a bar line.
          //
          // TODO: This is possibly out of order: adding a bar line allows a
          // line break, which might be unwanted.  Consider enhancing the
          // Volta_engraver and bracket to align to something else
          // (Paper_column?) when there is no bar line.
          has_repeat_bar = true;
        }

      if (has_repeat_bar)
        set_property (context (), "whichBar", ly_string2scm (rb));
    }
}

void
Repeat_acknowledge_engraver::stop_translation_timestep ()
{
  first_time_ = false;
}

void
Repeat_acknowledge_engraver::boot ()
{
  ADD_LISTENER (Repeat_acknowledge_engraver, fine);
  ADD_LISTENER (Repeat_acknowledge_engraver, section);
  ADD_LISTENER (Repeat_acknowledge_engraver, segno_mark);
  ADD_LISTENER (Repeat_acknowledge_engraver, volta_span);
}

ADD_TRANSLATOR (Repeat_acknowledge_engraver,
                /* doc */
                "Acknowledge repeated music, and convert the contents of"
                " @code{repeatCommands} into an appropriate setting for"
                " @code{whichBar}.",

                /* create */
                "",

                /* read */
                "defaultBarType "
                "doubleRepeatSegnoType "
                "doubleRepeatType "
                "endRepeatSegnoType "
                "endRepeatType "
                "fineBarType "
                "fineSegnoType "
                "fineStartRepeatSegnoType "
                "repeatCommands "
                "sectionBarType "
                "segnoType "
                "startRepeatSegnoType "
                "startRepeatType "
                "underlyingRepeatType "
                "whichBar",

                /* write */
                "whichBar"
               );
