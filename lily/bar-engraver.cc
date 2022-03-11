/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "engraver.hh"

#include "context.hh"
#include "grob-properties.hh"
#include "item.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "score-engraver.hh"
#include "spanner.hh"
#include "warn.hh"

#include "translator.icc"

#include <string>
#include <vector>

class Bar_engraver final : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Bar_engraver);

private:
  void acknowledge_end_spanner (Grob_info_t<Spanner>);
  SCM calc_bar_type () const;
  SCM calc_measure_bar_type () const;
  void initialize () override;
  void listen_ad_hoc_jump (Stream_event *);
  void listen_coda_mark (Stream_event *);
  void listen_dal_segno (Stream_event *);
  void listen_fine (Stream_event *);
  void listen_section (Stream_event *);
  void listen_segno_mark (Stream_event *);
  void listen_volta_span (Stream_event *);
  void process_acknowledged ();
  void process_music ();
  void start_translation_timestep ();
  void stop_translation_timestep ();

  struct Observations
  {
    bool fine = false;
    bool repeat_end = false;
    bool repeat_start = false;
    bool section = false;
    bool segno = false;
    bool underlying_repeat = false;
    bool volta_span = false;
  };

private:
  Observations observations_;
  Item *bar_ = nullptr;
  std::vector<Spanner *> spanners_;
  bool first_time_ = true;
};

Bar_engraver::Bar_engraver (Context *c)
  : Engraver (c)
{
}

SCM
Bar_engraver::calc_measure_bar_type () const
{
  // Gould writes that "[a] thin double barline ... marks the written end of
  // the music when this is not the end of the piece" (Behind Bars, p.240).
  // Although it would be fairly easy to implement that as a default, we avoid
  // it on the grounds that the input is possibly not a finished work, and it
  // is easy for the user to add a \section command at the end when it is.

  // TODO: barAlways seems to be a hack to allow a line break anywhere.
  // Improve.
  if (from_scm<bool> (get_property (this, "measureStartNow"))
      || from_scm<bool> (get_property (this, "barAlways")))
    {
      // TODO: Why do we need automaticBars?  Instead, set measureBarType to a
      // value that prevents creation of a bar line.
      if (from_scm<bool> (get_property (this, "automaticBars")))
        {
          return get_property (this, "measureBarType");
        }
    }

  return SCM_EOL;
}

SCM
Bar_engraver::calc_bar_type () const
{
  const bool segno = observations_.segno
                     && scm_is_eq (get_property (this, "segnoStyle"),
                                   ly_symbol2scm ("bar-line"));

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
      if (observations_.repeat_start)
        {
          if (observations_.repeat_end) // { segno, start, end }
            {
              SCM s = get_property (this, "doubleRepeatSegnoBarType");
              rb = robust_scm2string (s, ":|.S.|:");
              has_repeat_bar = true;
            }
          else // { segno, start }
            {
              if (observations_.fine)
                {
                  SCM s = get_property (this, "fineStartRepeatSegnoBarType");
                  rb = robust_scm2string (s, "|.S.|:");
                  has_repeat_bar = true;
                }
              else
                {
                  SCM s = get_property (this, "startRepeatSegnoBarType");
                  rb = robust_scm2string (s, "S.|:");
                  has_repeat_bar = true;
                }
            }
        }
      else if (observations_.repeat_end) // { segno, end }
        {
          SCM s = get_property (this, "endRepeatSegnoBarType");
          rb = robust_scm2string (s, ":|.S");
          has_repeat_bar = true;
        }
      else // { segno }
        {
          if (observations_.fine)
            {
              SCM s = get_property (this, "fineSegnoBarType");
              rb = robust_scm2string (s, "|.S");
              has_repeat_bar = true;
            }
          else
            {
              SCM s = get_property (this, "segnoBarType");
              rb = robust_scm2string (s, "S");
              has_repeat_bar = true;
            }
        }
    }
  else if (observations_.repeat_start)
    {
      if (observations_.repeat_end) // { start, end }
        {
          SCM s = get_property (this, "doubleRepeatBarType");
          rb = robust_scm2string (s, ":..:");
          has_repeat_bar = true;
        }
      else // { start }
        {
          SCM s = get_property (this, "startRepeatBarType");
          rb = robust_scm2string (s, ".|:");
          has_repeat_bar = true;
        }
    }
  else if (observations_.repeat_end) // { end }
    {
      SCM s = get_property (this, "endRepeatBarType");
      rb = robust_scm2string (s, ":|.");
      has_repeat_bar = true;
    }

  SCM mb_scm = SCM_EOL; // computed lazily; see below
  bool has_underlying_bar = false;
  std::string ub;
  if (observations_.fine)
    {
      ub = robust_scm2string (get_property (this, "fineBarType"), "|.");
      has_underlying_bar = true;
    }
  else if (observations_.section)
    {
      ub = robust_scm2string (get_property (this, "sectionBarType"), "||");
      has_underlying_bar = true;
    }
  else if ((mb_scm = calc_measure_bar_type ()), scm_is_string (mb_scm))
    {
      ub = ly_scm2string (mb_scm);
      has_underlying_bar = true;
    }
  else if (observations_.underlying_repeat)
    {
      // At points of repetition or departure where there wouldn't
      // otherwise be a bar line, print a thin double bar line (Behind
      // Bars, p.240).
      SCM s = get_property (this, "underlyingRepeatBarType");
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
          // startRepeatBarType to ".|:-||", appending the default value of
          // underlyingRepeatBarType would make it ".|:-||-||", which isn't
          // in the built-in set of bar lines.  Rather than setting
          // whichBar to a useless value, discard the annotation.
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
  else if (observations_.volta_span)
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

  return has_repeat_bar ? ly_string2scm (rb) : SCM_EOL;
}

void
Bar_engraver::listen_ad_hoc_jump (Stream_event *)
{
  observations_.underlying_repeat = true;
}

void
Bar_engraver::listen_coda_mark (Stream_event *)
{
  observations_.underlying_repeat = true;
}

void
Bar_engraver::listen_dal_segno (Stream_event *)
{
  observations_.underlying_repeat = true;
}

void
Bar_engraver::listen_fine (Stream_event *)
{
  observations_.fine = true;
}

void
Bar_engraver::listen_section (Stream_event *)
{
  observations_.section = true;
}

void
Bar_engraver::listen_segno_mark (Stream_event *ev)
{
  // Ignore a default segno at the beginning of a piece, just like
  // Mark_tracking_translator.
  if (first_time_)
    {
      SCM label = get_property (ev, "label");
      if (!scm_is_integer (label)) // \segnoMark \default
        return;
    }

  observations_.segno = true;
}

void
Bar_engraver::listen_volta_span (Stream_event *)
{
  observations_.volta_span = true;
}

void
Bar_engraver::initialize ()
{
  Engraver::initialize ();
  set_property (context (), "currentBarLine", SCM_EOL);
}

void
Bar_engraver::start_translation_timestep ()
{
  // We reset currentBarLine here rather than in stop_translation_timestep ()
  // so that other engravers can use it during stop_translation_timestep ().
  if (bar_)
    {
      bar_ = nullptr;
      set_property (context (), "currentBarLine", SCM_EOL);
    }
}

// At the start of a piece, we don't print any repeat bars.
void
Bar_engraver::process_music ()
{
  // If whichBar is set, use it.  It was probably set with \bar, but it might
  // have been set with the deprecated \set Timing.whichBar or a Scheme
  // equivalent.
  SCM wb = get_property (this, "whichBar");
  if (!scm_is_string (wb))
    {
      if (!first_time_)
        {
          SCM repeat_commands = get_property (this, "repeatCommands");
          for (SCM command : as_ly_scm_list (repeat_commands))
            {
              if (scm_is_pair (command)) // (command option...)
                command = scm_car (command);

              if (scm_is_eq (command, ly_symbol2scm ("end-repeat")))
                observations_.repeat_end = true;
              else if (scm_is_eq (command, ly_symbol2scm ("start-repeat")))
                observations_.repeat_start = true;
              else if (scm_is_eq (command, ly_symbol2scm ("volta")))
                observations_.volta_span = true;
            }
        }
      else
        {
          observations_.repeat_end = false;
          observations_.repeat_start = false;
          observations_.underlying_repeat = false;
          observations_.volta_span = false;
        }

      if (observations_.repeat_start
          || observations_.repeat_end
          || observations_.segno)
        {
          observations_.underlying_repeat = true;
        }

      wb = calc_bar_type ();
    }

  if (scm_is_string (wb))
    {
      bar_ = make_item ("BarLine", SCM_EOL);
      if (!ly_is_equal (wb, get_property (bar_, "glyph")))
        set_property (bar_, "glyph", wb);

      set_property (context (), "currentBarLine", to_scm (bar_));
    }
}

void
Bar_engraver::process_acknowledged ()
{
  if (bar_)
    {
      for (const auto &sp : spanners_)
        sp->set_bound (RIGHT, bar_);
    }

  spanners_.clear ();
}

/*
  lines may only be broken if there is a barline in all staves
*/
void
Bar_engraver::stop_translation_timestep ()
{
  if (!bar_)
    set_property (find_score_context (), "forbidBreak", SCM_BOOL_T);

  first_time_ = false;
  observations_ = {};
}

void
Bar_engraver::acknowledge_end_spanner (Grob_info_t<Spanner> gi)
{
  if (bar_) // otherwise avoid a little work
    {
      auto *const sp = gi.grob ();
      if (from_scm<bool> (get_property (sp, "to-barline")))
        spanners_.push_back (sp);
    }
}

void
Bar_engraver::boot ()
{
  ADD_END_ACKNOWLEDGER (Bar_engraver, spanner);
  ADD_LISTENER (Bar_engraver, ad_hoc_jump);
  ADD_LISTENER (Bar_engraver, coda_mark);
  ADD_LISTENER (Bar_engraver, dal_segno);
  ADD_LISTENER (Bar_engraver, fine);
  ADD_LISTENER (Bar_engraver, section);
  ADD_LISTENER (Bar_engraver, segno_mark);
  ADD_LISTENER (Bar_engraver, volta_span);
}

ADD_TRANSLATOR (Bar_engraver,
                /* doc */
                R"(
Create barlines.  This engraver is controlled through the @code{whichBar}
property.  If it has no bar line to create, it will forbid a linebreak at this
point.  This engraver is required to trigger the creation of clefs at the start
of systems.
                )",

                /* create */
                R"(
BarLine
                )",

                /* read */
                R"(
doubleRepeatBarType
doubleRepeatSegnoBarType
endRepeatBarType
endRepeatSegnoBarType
fineBarType
fineSegnoBarType
fineStartRepeatSegnoBarType
measureBarType
repeatCommands
sectionBarType
segnoBarType
segnoStyle
startRepeatBarType
startRepeatSegnoBarType
underlyingRepeatBarType
whichBar
                )",

                /* write */
                R"(
currentBarLine
forbidBreak
                )");
