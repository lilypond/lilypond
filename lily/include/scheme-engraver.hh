/*
  scheme-engraver.hh -- declare Scheme_engraver

  source file of the GNU LilyPond music typesetter

  Copyright (c) 2009--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#ifndef SCHEME_ENGRAVER_HH
#define SCHEME_ENGRAVER_HH

#include "engraver.hh"

struct Preinit_Scheme_engraver
{
  SCM initialize_function_;
  SCM finalize_function_;
  SCM precomputable_methods_[TRANSLATOR_METHOD_PRECOMPUTE_COUNT];

  // hashq table of interface-symbol -> scheme-function
  Drul_array<SCM> interface_acknowledger_hash_ {SCM_EOL, SCM_EOL};

  // Alist of listened-symbol . scheme-function
  SCM per_instance_listeners_;
  Preinit_Scheme_engraver ();
};

class Scheme_engraver : Preinit_Scheme_engraver, public Engraver
{
public:
  TRANSLATOR_FAMILY_DECLARATIONS (Scheme_engraver)
  Scheme_engraver (SCM definition, Context *c);

protected:
  ~Scheme_engraver ();

  void initialize () override;
  void finalize () override;
  void derived_mark () const override;
  SCM get_listener_list () const override;
  bool must_be_last () const override;
  bool is_midi () const override { return is_midi_; };
  bool is_layout () const override { return is_layout_; };

private:
  SCM get_acknowledger (SCM sym, Direction start_end) override
  {
    return generic_get_acknowledger (sym,
                                     interface_acknowledger_hash_[start_end]);
  }

  SCM init_acknowledgers (SCM alist);

  bool must_be_last_;
  bool is_midi_;
  bool is_layout_;
};

#endif /* SCHEME_ENGRAVER_HH */
