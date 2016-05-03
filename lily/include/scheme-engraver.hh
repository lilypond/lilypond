/*
  scheme-engraver.hh -- declare Scheme_engraver

  source file of the GNU LilyPond music typesetter

  Copyright (c) 2009--2015 Han-Wen Nienhuys <hanwen@lilypond.org>

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

class Scheme_engraver : public Engraver
{
  void init_from_scheme (SCM definition);
public:
  TRANSLATOR_FAMILY_DECLARATIONS (Scheme_engraver);
  Scheme_engraver (SCM definition);

protected:
  ~Scheme_engraver ();

  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
  void process_acknowledged ();

  virtual void initialize ();
  virtual void finalize ();
  virtual void derived_mark () const;
  virtual SCM get_listener_list () const;
  virtual bool must_be_last () const;

private:
  virtual SCM get_acknowledger (SCM sym)
  {
    return generic_get_acknowledger (sym, interface_acknowledger_hash_);
  }
  virtual SCM get_end_acknowledger (SCM sym)
  {
    return generic_get_acknowledger (sym, interface_end_acknowledger_hash_);
  }

  void init_acknowledgers (SCM alist, SCM *hash);
  // For now no description.  In future, something derived from the
  // definition might make sense.
  SCM translator_description () const { return SCM_EOL; }

  bool must_be_last_;

  SCM acknowledge_grob_function_;
  SCM stop_translation_timestep_function_;
  SCM start_translation_timestep_function_;
  SCM process_music_function_;
  SCM process_acknowledged_function_;
  SCM initialize_function_;
  SCM finalize_function_;

  // hashq table of interface-symbol -> scheme-function
  SCM interface_acknowledger_hash_;
  SCM interface_end_acknowledger_hash_;

  // Alist of listened-symbol . scheme-function
  SCM per_instance_listeners_;
};

#endif /* SCHEME_ENGRAVER_HH */
