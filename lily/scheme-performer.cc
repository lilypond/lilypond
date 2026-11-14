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

void
Scheme_performer::announce_raw (Stream_event *cause, Audio_raw_event::Kind kind,
                                uint8_t status, std::string data)
{
  announce<Audio_raw_event> (cause, kind, status, std::move (data));
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
Announce a MIDI control change event

This announces a MIDI control change event from @var{performer}, a translator
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
Announce a MIDI text meta event

This announces a MIDI text meta event from @var{performer}, a translator
created by @code{make-performer}.  @var{type} is one of the symbols
@code{text}, @code{copyright}, @code{track-name}, @code{instrument-name},
@code{lyric}, @code{marker} or @code{cue-point}.  @var{text} is a string.
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

// Convert a bytevector or a list of integers 0--255 to a string of bytes.
static std::string
bytes_from_scm (SCM data, int pos, char const *who)
{
  std::string out;

  if (scm_is_bytevector (data))
    {
      const size_t n = scm_c_bytevector_length (data);
      out.reserve (n);
      for (size_t i = 0; i < n; ++i)
        out += static_cast<char> (scm_c_bytevector_ref (data, i));
      return out;
    }

  if (!ly_cheap_is_list (data))
    scm_wrong_type_arg_msg (who, pos, data, "bytevector or list of bytes");

  for (SCM p = data; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM b = scm_car (p);
      if (!scm_is_integer (b))
        scm_wrong_type_arg_msg (who, pos, data, "bytevector or list of bytes");
      if (!scm_is_unsigned_integer (b, 0, 255))
        scm_misc_error (who, "byte out of range 0--255: ~S", scm_list_1 (b));
      out += static_cast<char> (scm_to_int (b));
    }
  return out;
}

static void
check_data_bytes (const std::string &data, char const *who)
{
  for (const char c : data)
    {
      if (static_cast<unsigned char> (c) > 0x7f)
        scm_misc_error (who, "data byte out of range 0--127: ~S",
                        scm_list_1 (to_scm (c & 0xff)));
    }
}

LY_DEFINE (ly_performer_announce_midi_channel,
           "ly:performer-announce-midi-channel", 4, 0, 0,
           (SCM performer, SCM cause, SCM status, SCM data),
           R"(
Announce raw MIDI channel voice message

This announces a raw MIDI channel voice message from @var{performer}, a
translator created by @code{make-performer}.  @var{cause} is a stream event or
@code{#f}.  @var{status} is a status byte in the range
@code{#x80}--@code{#xEF}; only its upper four bits are used, because the
channel is determined by the enclosing @code{Staff} or @code{Voice}.
@var{data} is a bytevector or a list of integers holding the data bytes (each
in the range 0--127): one byte for program change (@code{#xC0}) and channel
pressure (@code{#xD0}), two bytes for all other messages.

The message is placed at the current time step.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Scheme_performer, performer, 1);
  Stream_event *const ev = cause_from_scm (cause, 2);
  LY_ASSERT_TYPE (scm_is_integer, status, 3);

  constexpr auto who = "ly:performer-announce-midi-channel";

  if (!scm_is_unsigned_integer (status, 0x80, 0xef))
    scm_misc_error (who, "status byte out of range #x80--#xEF: ~S",
                    scm_list_1 (status));
  const auto st = static_cast<uint8_t> (scm_to_int (status) & 0xf0);

  std::string bytes = bytes_from_scm (data, 4, who);
  check_data_bytes (bytes, who);

  const size_t expected = (st == 0xc0 || st == 0xd0) ? 1 : 2;
  if (bytes.length () != expected)
    scm_misc_error (who, "status #x~X needs ~S data byte(s), got ~S",
                    scm_list_3 (to_scm (static_cast<int> (st)),
                                to_scm (static_cast<int> (expected)),
                                to_scm (static_cast<int> (bytes.length ()))));

  p->announce_raw (ev, Audio_raw_event::CHANNEL, st, std::move (bytes));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_performer_announce_midi_meta, "ly:performer-announce-midi-meta",
           4, 0, 0, (SCM performer, SCM cause, SCM type, SCM data),
           R"(
Announce raw MIDI meta event

This announces a raw MIDI meta event from @var{performer}, a translator created
by @code{make-performer}.  @var{cause} is a stream event or @code{#f}.
@var{type} is the meta event type in the range 0--127, except @code{#x2F} (end
of track), which LilyPond writes itself.  @var{data} is a bytevector or a list
of integers 0--255 holding the event data.  The length is added automatically.

The event is placed at the current time step, in the track of the enclosing
@code{Staff} or @code{Voice}.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Scheme_performer, performer, 1);
  Stream_event *const ev = cause_from_scm (cause, 2);
  LY_ASSERT_TYPE (scm_is_integer, type, 3);

  constexpr auto who = "ly:performer-announce-midi-meta";

  if (!scm_is_unsigned_integer (type, 0, 0x7f))
    scm_misc_error (who, "meta event type out of range 0--127: ~S",
                    scm_list_1 (type));
  const auto t = static_cast<uint8_t> (scm_to_int (type));
  if (t == 0x2f)
    scm_misc_error (who, "end-of-track events are generated automatically: ~S",
                    scm_list_1 (type));

  p->announce_raw (ev, Audio_raw_event::META, t, bytes_from_scm (data, 4, who));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_performer_announce_midi_sysex, "ly:performer-announce-midi-sysex",
           3, 0, 0, (SCM performer, SCM cause, SCM data),
           R"(
Announce raw MIDI system exclusive message

This announces a raw MIDI system exclusive message from @var{performer}, a
translator created by @code{make-performer}.  @var{cause} is a stream event or
@code{#f}.  @var{data} is a bytevector or a list of integers holding the
message body @emph{without} the leading @code{#xF0}.  Every byte must be in
the range 0--127, except for an optional terminating @code{#xF7}, which is
added if missing.

The event is placed at the current time step, in the track of the enclosing
@code{Staff} or @code{Voice}.
           )")
{
  auto *const p = LY_ASSERT_SMOB (Scheme_performer, performer, 1);
  Stream_event *const ev = cause_from_scm (cause, 2);

  constexpr auto who = "ly:performer-announce-midi-sysex";

  std::string bytes = bytes_from_scm (data, 3, who);
  if (!bytes.empty () && static_cast<unsigned char> (bytes.back ()) == 0xf7)
    bytes.pop_back ();
  check_data_bytes (bytes, who);
  bytes += static_cast<char> (0xf7);

  p->announce_raw (ev, Audio_raw_event::SYSEX, 0, std::move (bytes));
  return SCM_UNSPECIFIED;
}
