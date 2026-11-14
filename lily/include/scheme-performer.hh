/*
  scheme-performer.hh -- declare Scheme_performer

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

#ifndef SCHEME_PERFORMER_HH
#define SCHEME_PERFORMER_HH

#include "audio-item.hh"
#include "performer.hh"

#include <string>

// Holds the SCM members.  As in Preinit_Scheme_engraver, this is a private
// base listed *before* Performer, so the members are initialized to safe
// values before the Translator smob exists and can be marked.
struct Preinit_Scheme_performer
{
  SCM initialize_function_;
  SCM finalize_function_;
  SCM precomputable_methods_[TRANSLATOR_METHOD_PRECOMPUTE_COUNT];

  // Alist of listened-symbol . scheme-function
  SCM per_instance_listeners_;
  Preinit_Scheme_performer ();
};

// A translator defined by a Scheme alist (see make-performer) that is a real
// Performer, so it can announce Audio_elements.  Created for definitions that
// have (is-midi . #t) and not (is-layout . #t).
class Scheme_performer final : Preinit_Scheme_performer, public Performer
{
public:
  TRANSLATOR_FAMILY_DECLARATIONS (Scheme_performer)
  Scheme_performer (SCM definition, Context *c);

  // Public wrappers: Performer::announce () is protected.  The Audio_element
  // is owned by the Performance; Scheme never sees the pointer, and the
  // payload is copied into C++ values, so there is nothing to mark.
  // `cause` may be null.  Channel and track are assigned by Staff_performer.
  void announce_control_change (Stream_event *cause, int control, int value);
  void announce_text (Stream_event *cause, Audio_text::Type type,
                      const std::string &text);
  void announce_raw (Stream_event *cause, Audio_raw_event::Kind kind,
                     uint8_t status, std::string data);

protected:
  ~Scheme_performer ();

  void initialize () override;
  void finalize () override;
  void derived_mark () const override;
  SCM get_listener_list () const override;
  bool must_be_last () const override;
  bool is_midi () const override { return true; }

private:
  // Performers are not offered grobs, and Performer_group does not use
  // acknowledger tables, so there is nothing to look up.
  SCM get_acknowledger (SCM, Direction) override { return SCM_UNDEFINED; }

  bool must_be_last_;
};

#endif /* SCHEME_PERFORMER_HH */
