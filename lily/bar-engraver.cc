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
#include "ly-smob-list.hh"
#include "score-engraver.hh"
#include "simple-event-listener.hh"
#include "spanner.hh"
#include "warn.hh"

#include "translator.icc"

#include <array>
#include <string>
#include <vector>

enum class BarType
{
  // from low to high priority
  NONE = 0,
  EMPTY,
  UNDERLYING_REPEAT,
  UNDERLYING_CAESURA,
  MEASURE,
  CAESURA,
  SECTION,
  FINE,
  REPEAT
};

class Bar_engraver final : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Bar_engraver);

private:
  void acknowledge_end_spanner (Grob_info_t<Spanner>);
  ly_scm_list calc_bar_type () const;
  void initialize () override;
  void listen_caesura (Stream_event *);
  void listen_segno_mark (Stream_event *);
  void process_acknowledged ();
  void pre_process_music ();
  void process_music ();
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void derived_mark () const override;

  struct Observations
  {
    bool repeat_end = false;
    bool repeat_start = false;
    bool segno = false;
  };

private:
  Stream_event *caesura_ev_ = nullptr;
  Boolean_event_listener fine_listener_;
  Boolean_event_listener section_listener_;
  Boolean_event_listener underlying_repeat_listener_;
  Boolean_event_listener volta_span_listener_;

  Observations observations_;
  SCM glyph_ = SCM_EOL;
  SCM glyph_left_ = SCM_EOL;
  SCM glyph_right_ = SCM_EOL;
  Item *bar_ = nullptr;
  std::vector<Spanner *> spanners_;
  bool first_time_ = true;
  bool has_any_glyph_ = false;
};

Bar_engraver::Bar_engraver (Context *c)
  : Engraver (c)
{
}

void
Bar_engraver::derived_mark () const
{
  scm_gc_mark (glyph_);
  scm_gc_mark (glyph_left_);
  scm_gc_mark (glyph_right_);
}

// Returns zero or more BarLine.glyph values from highest to lowest priority.
ly_scm_list
Bar_engraver::calc_bar_type () const
{
  // Read caesuraType and pass it through caesuraTypeTransform.
  // TODO: Caesura_engraver and Divisio_engraver also do this stuff.
  // Refactor to reduce repetition and ensure consistency.
  auto get_caesura_type = [] (Context *ctx, Stream_event *ev) {
    SCM caesura_type = get_property (ctx, "caesuraType");

    // Form a symbol list describing the user-provided articulations.
    SCM user_artic_types = SCM_EOL;
    SCM articulations_sym = ly_symbol2scm ("articulations");
    ly_smob_list<Stream_event> arts (get_property (ev, articulations_sym));
    for (auto *art : arts)
      {
        SCM a_type = get_property (art, "articulation-type");
        if (scm_is_symbol (a_type))
          user_artic_types = scm_cons (a_type, user_artic_types);
      }

    user_artic_types = scm_reverse_x (user_artic_types, SCM_EOL);

    // Add the user's articulations to the caesuraType value.
    caesura_type = scm_acons (ly_symbol2scm ("articulations"), user_artic_types,
                              caesura_type);
    // Pass caesuraType through the transform function, if it is set.
    SCM transform = get_property (ctx, "caesuraTypeTransform");
    if (ly_is_procedure (transform))
      {
        caesura_type = ly_call (transform, to_scm (ctx), caesura_type,
                                SCM_EOL /*observations*/);
      }

    return caesura_type;
  };

  SCM caesura_type
    = caesura_ev_ ? get_caesura_type (context (), caesura_ev_) : SCM_EOL;

  const bool segno = observations_.segno
                     && scm_is_eq (get_property (this, "segnoStyle"),
                                   ly_symbol2scm ("bar-line"));

  ly_scm_list glyphs;
  auto glyphs_tail = glyphs.begin ();

  // This order could be user-configurable, but most of the permutations are
  // probably not useful enough to be worth explaining, testing, and
  // maintaining.  Varying the position of a caesura/phrase bar might be a good
  // reason to do it, but that is easy enough to do with two layers (as seen).
  constexpr std::array<BarType, 8> types_by_priority {
    BarType::REPEAT,
    BarType::FINE,
    BarType::SECTION,
    BarType::CAESURA,
    BarType::MEASURE,
    BarType::UNDERLYING_CAESURA,
    BarType::UNDERLYING_REPEAT,
    BarType::EMPTY};

  for (const auto layer : types_by_priority)
    {
      bool has_underlying_bar = false;
      std::string ub;

      // Read the named bar-type context property into `ub`.
      auto read_bar = [this, &has_underlying_bar, &ub] (SCM context_prop_sym) {
        SCM s = get_property (this, context_prop_sym);
        if (scm_is_string (s))
          {
            ub = ly_scm2string (s);
            has_underlying_bar = true;
          }
      };

      // Get the requested bar subproperty ('bar-line or 'underlying-bar-line)
      // from the caesura properties.
      auto read_caesura_bar
        = [this, &caesura_type, &has_underlying_bar, &ub] (SCM subprop_sym) {
            SCM s = scm_assq_ref (caesura_type, subprop_sym);
            if (scm_is_string (s))
              {
                ub = ly_scm2string (s);
                has_underlying_bar = true;
              }
          };

      switch (layer)
        {
        case BarType::REPEAT:
          // TODO: Move this jenga tower into a Scheme callback if further
          // customizability is desired.  The number of dimensions makes it a
          // hassle to maintain a built-in context property for every
          // combination.  Don't pass the state as parameters: set context
          // properties before calling.  (Well, some of these already came from
          // repeatCommands, for what that's worth.)
          if (segno)
            {
              if (observations_.repeat_start)
                {
                  if (observations_.repeat_end)
                    read_bar (ly_symbol2scm ("doubleRepeatSegnoBarType"));
                  else if (fine_listener_.heard ())
                    read_bar (ly_symbol2scm ("fineStartRepeatSegnoBarType"));
                  else
                    read_bar (ly_symbol2scm ("startRepeatSegnoBarType"));
                }
              else if (observations_.repeat_end)
                {
                  read_bar (ly_symbol2scm ("endRepeatSegnoBarType"));
                }
              else // no repeat
                {
                  if (fine_listener_.heard ())
                    read_bar (ly_symbol2scm ("fineSegnoBarType"));
                  else
                    read_bar (ly_symbol2scm ("segnoBarType"));
                }
            }
          else // no segno
            {
              if (observations_.repeat_start)
                {
                  if (observations_.repeat_end)
                    read_bar (ly_symbol2scm ("doubleRepeatBarType"));
                  else
                    read_bar (ly_symbol2scm ("startRepeatBarType"));
                }
              else if (observations_.repeat_end)
                {
                  read_bar (ly_symbol2scm ("endRepeatBarType"));
                }
            }
          break;

        case BarType::FINE:
          if (fine_listener_.heard ())
            read_bar (ly_symbol2scm ("fineBarType"));
          break;

        case BarType::SECTION:
          // Gould writes that "[a] thin double barline ... marks the written
          // end of the music when this is not the end of the piece" (Behind
          // Bars, p.240).  Although it would be fairly easy to implement that
          // as a default, we avoid it on the grounds that the input is
          // possibly not a finished work, and it is easy for the user to add a
          // \section command at the end when it is.
          if (section_listener_.heard ())
            read_bar (ly_symbol2scm ("sectionBarType"));
          break;

        case BarType::CAESURA:
          if (caesura_ev_)
            read_caesura_bar (ly_symbol2scm ("bar-line"));
          break;

        case BarType::MEASURE:
          if (from_scm<bool> (get_property (this, "measureStartNow")))
            read_bar (ly_symbol2scm ("measureBarType"));
          break;

        case BarType::UNDERLYING_CAESURA:
          if (caesura_ev_)
            read_caesura_bar (ly_symbol2scm ("underlying-bar-line"));
          break;

        case BarType::UNDERLYING_REPEAT:
          if (underlying_repeat_listener_.heard ())
            read_bar (ly_symbol2scm ("underlyingRepeatBarType"));
          break;

        case BarType::EMPTY:
          if (volta_span_listener_.heard ())
            {
              // Volta brackets align on bar lines, so create an empty bar
              // line where there isn't already a bar line.
              //
              // TODO: This is possibly out of order: adding a bar line
              // allows a line break, which might be unwanted.  Consider
              // enhancing the Volta_engraver and bracket to align to
              // something else (Paper_column?) when there is no bar line.
              ub.clear ();
              has_underlying_bar = true;
            }

        default:
          break;
        }

      if (has_underlying_bar)
        {
          glyphs_tail = glyphs.insert_before (glyphs_tail, ly_string2scm (ub));
          ++glyphs_tail;
        }
    }

  return glyphs;
}

void
Bar_engraver::listen_caesura (Stream_event *ev)
{
  assign_event_once (caesura_ev_, ev);
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
Bar_engraver::pre_process_music ()
{
  SCM glyphs = SCM_EOL;

  // If whichBar is set, use it.  It was probably set with \bar, but it might
  // have been set with the deprecated \set Timing.whichBar or a Scheme
  // equivalent.
  SCM wb = get_property (this, "whichBar");
  if (scm_is_string (wb))
    {
      glyphs = ly_list (wb);
    }
  else // consider automatic bars
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
                volta_span_listener_.set_heard ();
            }
        }
      else
        {
          observations_.repeat_end = false;
          observations_.repeat_start = false;
          underlying_repeat_listener_.reset ();
          volta_span_listener_.reset ();
        }

      if (observations_.repeat_start || observations_.repeat_end
          || observations_.segno)
        {
          underlying_repeat_listener_.set_heard ();
        }

      glyphs = calc_bar_type ().begin_scm ();
    }

  auto &calc_name = Lily::bar_line_calc_glyph_name_for_direction;
  glyph_ = calc_name (glyphs, to_scm (CENTER));
  glyph_left_ = calc_name (glyphs, to_scm (LEFT));
  glyph_right_ = calc_name (glyphs, to_scm (RIGHT));
  has_any_glyph_ = scm_is_string (glyph_) || scm_is_string (glyph_left_)
                   || scm_is_string (glyph_right_);

  // This needs to be in pre-process-music so other engravers can notice a break
  // won't be allowed (unless forced) at process-music stage.  That allows some
  // of them to efficiently skip processing that is only needed at potential
  // break points.
  if (!has_any_glyph_
      && from_scm<bool> (get_property (this, "forbidBreakBetweenBarLines")))
    {
      set_property (find_score_context (), "forbidBreak", SCM_BOOL_T);
    }
}

void
Bar_engraver::process_music ()
{
  if (has_any_glyph_)
    {
      // TODO: We have events in most cases (not for manual repeats), so it
      // would be nice to provide a cause here.
      bar_ = make_item ("BarLine", SCM_EOL);

      SCM glyph_sym = ly_symbol2scm ("glyph");
      if (!ly_is_equal (glyph_, get_property (bar_, glyph_sym)))
        set_property (bar_, glyph_sym, glyph_);

      SCM glyph_left_sym = ly_symbol2scm ("glyph-left");
      if (!ly_is_equal (glyph_left_, get_property (bar_, glyph_left_sym)))
        set_property (bar_, glyph_left_sym, glyph_left_);

      SCM glyph_right_sym = ly_symbol2scm ("glyph-right");
      if (!ly_is_equal (glyph_right_, get_property (bar_, glyph_right_sym)))
        set_property (bar_, glyph_right_sym, glyph_right_);

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
  glyph_ = SCM_EOL;
  glyph_left_ = SCM_EOL;
  glyph_right_ = SCM_EOL;
  first_time_ = false;
  has_any_glyph_ = false;
  observations_ = {};

  caesura_ev_ = nullptr;
  fine_listener_.reset ();
  section_listener_.reset ();
  underlying_repeat_listener_.reset ();
  volta_span_listener_.reset ();
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
  ADD_END_ACKNOWLEDGER (spanner);

  ADD_LISTENER (caesura);
  ADD_LISTENER (segno_mark);

  ADD_DELEGATE_LISTENER (fine);
  ADD_DELEGATE_LISTENER (section);
  ADD_DELEGATE_LISTENER_FOR (underlying_repeat_listener_, listen, ad_hoc_jump);
  ADD_DELEGATE_LISTENER_FOR (underlying_repeat_listener_, listen, coda_mark);
  ADD_DELEGATE_LISTENER_FOR (underlying_repeat_listener_, listen, dal_segno);
  ADD_DELEGATE_LISTENER (volta_span);
}

ADD_TRANSLATOR (Bar_engraver,
                /* doc */
                R"(
Create bar lines for various commands, including @code{\\bar}.

If @code{forbidBreakBetweenBarLines} is true, allow line breaks at bar lines
only.
                )",

                /* create */
                R"(
BarLine
                )",

                /* read */
                R"(
caesuraType
caesuraTypeTransform
doubleRepeatBarType
doubleRepeatSegnoBarType
endRepeatBarType
endRepeatSegnoBarType
fineBarType
fineSegnoBarType
fineStartRepeatSegnoBarType
forbidBreakBetweenBarLines
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
