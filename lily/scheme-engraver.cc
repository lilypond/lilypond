/*
  scheme-engraver.cc -- implement Scheme_engraver

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

#include "scheme-engraver.hh"

#include "grob.hh"

#include "translator.icc"

#include "scm-hash.hh"

Preinit_Scheme_engraver::Preinit_Scheme_engraver ()
{
  initialize_function_ = SCM_EOL;
  finalize_function_ = SCM_EOL;

  per_instance_listeners_ = SCM_EOL;
  for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
    precomputable_methods_[i] = SCM_UNDEFINED;
}

Scheme_engraver::~Scheme_engraver ()
{
}

// Extracts the value if callable, if not return SCM_UNDEFINED;
static SCM
callable (SCM symbol, SCM defn)
{
  SCM val = ly_assoc_get (symbol, defn, SCM_BOOL_F);
  return ly_is_procedure (val) ? val : SCM_UNDEFINED;
}

bool
Scheme_engraver::must_be_last () const
{
  return must_be_last_;
}

void
Scheme_engraver::fetch_precomputable_methods (SCM ptrs[])
{
  for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
    ptrs[i] = precomputable_methods_[i];
}

Scheme_engraver::Scheme_engraver (SCM definition, Context *c)
  : Engraver (c)
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

  is_midi_ = from_scm<bool> (
    ly_assoc_get (ly_symbol2scm ("is-midi"), definition, SCM_BOOL_F));

  is_layout_
    = from_scm<bool> (ly_assoc_get (ly_symbol2scm ("is-layout"), definition,
                                    is_midi_ ? SCM_BOOL_F : SCM_BOOL_T));

  SCM p = ly_assoc_get (ly_symbol2scm ("listeners"), definition, SCM_EOL);
  SCM listeners = SCM_EOL;

  must_be_last_ = from_scm<bool> (
    ly_assoc_get (ly_symbol2scm ("must-be-last"), definition, SCM_BOOL_F));

  for (; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM event_class = scm_caar (p);
      SCM proc = scm_cdar (p);

      if (!(ly_is_procedure (proc) && ly_is_symbol (event_class)))
        continue;

      // We should check the arity of the function?

      // Record for later lookup.
      listeners = scm_acons (event_class, proc, listeners);
    }

  SCM hash1 = init_acknowledgers (
    ly_assoc_get (ly_symbol2scm ("acknowledgers"), definition, SCM_EOL));
  SCM hash2 = init_acknowledgers (
    ly_assoc_get (ly_symbol2scm ("end-acknowledgers"), definition, SCM_EOL));

  per_instance_listeners_ = listeners;
  interface_acknowledger_hash_ = {hash1, hash2};

  // It's not defined whether Scheme_engraver::derived_mark is already
  // active while the construction is underway, so we make sure we
  // keep a version of everything on the stack that is not still
  // covered by `definition'.

  scm_remember_upto_here_2 (definition, listeners);
  scm_remember_upto_here_2 (hash1, hash2);

  // TODO: hook up description, props read/written, grobs created
  // etc. to provide automatic documentation.
}

SCM
Scheme_engraver::init_acknowledgers (SCM alist)
{
  SCM hash = Scheme_hash_table::make_smob ();
  for (SCM p = alist; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM iface = scm_caar (p);
      SCM proc = scm_cdar (p);

      if (!(ly_is_procedure (proc) && ly_is_symbol (iface)))
        continue;

      unsmob<Scheme_hash_table> (hash)->set (iface, proc);
    }
  return hash;
}

SCM
Scheme_engraver::get_listener_list () const
{
  return per_instance_listeners_;
}

void
Scheme_engraver::initialize ()
{
  if (!SCM_UNBNDP (initialize_function_))
    ly_call (initialize_function_, self_scm ());
}

void
Scheme_engraver::finalize ()
{
  if (!SCM_UNBNDP (finalize_function_))
    ly_call (finalize_function_, self_scm ());
}

void
Scheme_engraver::derived_mark () const
{
  for (int i = 0; i < TRANSLATOR_METHOD_PRECOMPUTE_COUNT; i++)
    scm_gc_mark (precomputable_methods_[i]);

  scm_gc_mark (initialize_function_);
  scm_gc_mark (finalize_function_);
  scm_gc_mark (per_instance_listeners_);
  scm_gc_mark (interface_acknowledger_hash_[START]);
  scm_gc_mark (interface_acknowledger_hash_[STOP]);
}
