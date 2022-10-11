/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef TRANSLATOR_GROUP_HH
#define TRANSLATOR_GROUP_HH

#include "callback.hh"
#include "translator.hh"

#include <vector>

class Translator_group : public Smob<Translator_group>
{
public:
  SCM mark_smob () const;
  int print_smob (SCM, scm_print_state *) const;
  static const char *const type_p_name_;
  virtual ~Translator_group ();

private:
  void precompute_method_bindings ();
  std::vector<Method_instance>
    precomputed_method_bindings_[TRANSLATOR_METHOD_PRECOMPUTE_COUNT];

  SCM protected_events_;

  void create_child_translator (SCM);

public:
  VIRTUAL_CLASS_NAME (Translator_group);

  virtual void connect_to_context (Context *c);
  virtual void disconnect_from_context ();
  virtual SCM get_simple_trans_list ();
  virtual void initialize ();
  virtual void finalize ();

  void protect_event (SCM ev);

  void stop_translation_timestep ();
  void start_translation_timestep ();

  Translator_group ();

  void precomputed_translator_foreach (Translator_precompute_index);

  Context *context () const { return context_; }

protected:
  SCM simple_trans_list_;
  Context *context_;

  friend class Context_def;
  virtual void derived_mark () const;
};

SCM names_to_translators (SCM namelist, Context *tg);
void recurse_over_translators (Context *c, SCM tr_method, SCM tr_group_method,
                               Direction);
void precomputed_recurse_over_translators (Context *c,
                                           Translator_precompute_index idx,
                                           Direction dir);
Translator_group *get_translator_group (SCM sym);

#define foobar
#define ADD_TRANSLATOR_GROUP(classname, desc, grobs, read, write) foobar

#endif // TRANSLATOR_GROUP_HH
