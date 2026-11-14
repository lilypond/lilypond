/*
  scheme-performer.cc -- implement Scheme_performer

  This file is part of LilyPond, the GNU music typesetter.

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

#include "scheme-performer.hh"

#include "stream-event.hh"

Preinit_Scheme_performer::Preinit_Scheme_performer ()
{
  initialize_function_ = SCM_EOL;
  finalize_function_ = SCM_EOL;
  per_instance_listeners_ = SCM_EOL;
  for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
    precomputable_methods_[i] = SCM_UNDEFINED;
}

Scheme_performer::~Scheme_performer ()
{
}

// Extracts the value if callable, if not return SCM_UNDEFINED.
static SCM
callable (SCM symbol, SCM defn)
{
  SCM val = ly_assoc_get (symbol, defn, SCM_BOOL_F);
  return ly_is_procedure (val) ? val : SCM_UNDEFINED;
}

Scheme_performer::Scheme_performer (SCM definition, Context *c)
  : Performer (c)
{
  precomputable_methods_[START_TRANSLATION_TIMESTEP]
    = callable (ly_symbol2scm ("start-translation-timestep"), definition);
  precomputable_methods_[STOP_TRANSLATION_TIMESTEP]
    = callable (ly_symbol2scm ("stop-translation-timestep"), definition);
  precomputable_methods_[PRE_PROCESS_MUSIC]
    = callable (ly_symbol2scm ("pre-process-music"), definition);
  precomputable_methods_[PROCESS_MUSIC]
    = callable (ly_symbol2scm ("process-music"), definition);
  precomputable_methods_[PROCESS_ACKNOWLEDGED]
    = callable (ly_symbol2scm ("process-acknowledged"), definition);
  initialize_function_ = callable (ly_symbol2scm ("initialize"), definition);
  finalize_function_ = callable (ly_symbol2scm ("finalize"), definition);

  must_be_last_ = from_scm<bool> (
    ly_assoc_get (ly_symbol2scm ("must-be-last"), definition, SCM_BOOL_F));

  SCM listeners = SCM_EOL;
  SCM p = ly_assoc_get (ly_symbol2scm ("listeners"), definition, SCM_EOL);
  for (; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM event_class = scm_caar (p);
      SCM proc = scm_cdar (p);

      if (!(ly_is_procedure (proc) && ly_is_symbol (event_class)))
        continue;

      listeners = scm_acons (event_class, proc, listeners);
    }
  per_instance_listeners_ = listeners;

  // Note: "acknowledgers" and "end-acknowledgers" are deliberately ignored
  // for now.  They become meaningful once Audio_elements are visible to
  // Scheme.

  scm_remember_upto_here_2 (definition, listeners);
}

void
Scheme_performer::fetch_precomputable_methods (SCM ptrs[])
{
  for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
    ptrs[i] = precomputable_methods_[i];
}

SCM
Scheme_performer::get_listener_list () const
{
  return per_instance_listeners_;
}

bool
Scheme_performer::must_be_last () const
{
  return must_be_last_;
}

void
Scheme_performer::initialize ()
{
  if (!SCM_UNBNDP (initialize_function_))
    ly_call (initialize_function_, self_scm ());
}

void
Scheme_performer::finalize ()
{
  if (!SCM_UNBNDP (finalize_function_))
    ly_call (finalize_function_, self_scm ());
}

void
Scheme_performer::derived_mark () const
{
  for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
    scm_gc_mark (precomputable_methods_[i]);

  scm_gc_mark (initialize_function_);
  scm_gc_mark (finalize_function_);
  scm_gc_mark (per_instance_listeners_);
}

void
Scheme_performer::announce_control_change (Stream_event *cause, int control,
                                           int value)
{
  announce<Audio_control_change> (cause, control, value);
}

void
Scheme_performer::announce_text (Stream_event *cause, Audio_text::Type type,
                                 const std::string &text)
{
  announce<Audio_text> (cause, type, text);
}

//
// Scheme interface
//

static Stream_event *
cause_from_scm (SCM cause, int pos)
{
  if (scm_is_false (cause))
    return nullptr;
  return LY_ASSERT_SMOB (Stream_event, cause, pos);
}

static int
data_byte_from_scm (SCM n, int pos, char const *who, char const *what)
{
  LY_ASSERT_TYPE (scm_is_integer, n, pos);
  const auto v = from_scm<int> (n);
  if (v < 0 || v > 127)
    scm_misc_error (who, what, scm_list_1 (n));
  return v;
}

LY_DEFINE (ly_performer_announce_control_change,
           "ly:performer-announce-control-change", 4, 0, 0,
           (SCM performer, SCM cause, SCM control, SCM value),
           R"(
Announce a MIDI control change event from @var{performer}, a translator
created by @code{make-performer}.  @var{cause} is a stream event or @code{#f}.
@var{control} and @var{value} are integers in the range 0--127.  The event is
placed at the current time step in the track and channel of the enclosing
@code{Staff} or @code{Voice}.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Scheme_performer, performer, 1);
  Stream_event *const ev = cause_from_scm (cause, 2);
  const int c
    = data_byte_from_scm (control, 3, "ly:performer-announce-control-change",
                          "control number out of range 0--127: ~S");
  const int v
    = data_byte_from_scm (value, 4, "ly:performer-announce-control-change",
                          "control value out of range 0--127: ~S");

  p->announce_control_change (ev, c, v);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_performer_announce_text, "ly:performer-announce-text", 4, 0, 0,
           (SCM performer, SCM cause, SCM type, SCM text),
           R"(
Announce a MIDI text meta event from @var{performer}, a translator created by
@code{make-performer}.  @var{type} is one of the symbols @code{text},
@code{copyright}, @code{track-name}, @code{instrument-name}, @code{lyric},
@code{marker} or @code{cue-point}.  @var{text} is a string.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Scheme_performer, performer, 1);
  Stream_event *const ev = cause_from_scm (cause, 2);
  LY_ASSERT_TYPE (ly_is_symbol, type, 3);
  LY_ASSERT_TYPE (scm_is_string, text, 4);

  struct Entry
  {
    char const *name;
    Audio_text::Type type;
  };
  static const Entry table[] = {
    {"text", Audio_text::TEXT},
    {"copyright", Audio_text::COPYRIGHT},
    {"track-name", Audio_text::TRACK_NAME},
    {"instrument-name", Audio_text::INSTRUMENT_NAME},
    {"lyric", Audio_text::LYRIC},
    {"marker", Audio_text::MARKER},
    {"cue-point", Audio_text::CUE_POINT},
  };

  for (const auto &e : table)
    {
      if (scm_is_eq (type, ly_symbol2scm (e.name)))
        {
          p->announce_text (ev, e.type, from_scm<std::string> (text));
          return SCM_UNSPECIFIED;
        }
    }

  scm_misc_error ("ly:performer-announce-text", "unknown text type: ~S",
                  scm_list_1 (type));
  return SCM_UNSPECIFIED;
}
