/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef CONTEXT_DEF_HH
#define CONTEXT_DEF_HH

#include "acceptance-set.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "input.hh"
#include "virtual-methods.hh"

#include <set>
#include <vector>

/*
  The definition of an interpretation context as given in the
  input. The lists are stored in order of definition.
*/
class Context_def : public Smob<Context_def>
{
public:
  SCM mark_smob () const;
  int print_smob (SCM, scm_print_state *) const;
  static const char *const type_p_name_;
  virtual ~Context_def ();

private:
  /*
    these lists store the definition, in opposite order of entry
  */
  SCM translator_mods_;
  Acceptance_set acceptance_;
  SCM property_ops_;
  SCM description_;
  SCM context_name_;
  SCM context_aliases_;
  SCM translator_group_type_;
  SCM input_location_;

public:
  Input *origin () const;
  void add_context_mod (SCM);
  SCM get_context_name () const { return context_name_; }
  SCM get_property_ops () const { return property_ops_; }
  const Acceptance_set &get_acceptance () const { return acceptance_; }
  SCM get_context_aliases () const { return context_aliases_; }
  SCM get_translator_names (SCM) const;
  SCM get_translator_group_type () const { return translator_group_type_; }
  void set_acceptor (SCM accepts, bool add);
  SCM lookup (SCM sym) const;
  bool is_alias (SCM sym) const;

  VIRTUAL_CLASS_NAME (Context_def);
  virtual Context_def *clone () const { return new Context_def (*this); }

  std::vector<Context_def *>
  path_to_acceptable_context (SCM type_string, Output_def *, SCM) const;

  static std::vector<Context_def *>
  path_to_bottom_context (Output_def *, SCM first_child_type_sym);

  SCM to_alist () const;
  static SCM make_scm ();

  void apply_default_property_operations (Context *);

private:
  Context_def ();
  Context_def (Context_def const &);

  std::vector<Context_def *> internal_path_to_acceptable_context (
    SCM type_string, bool instantiable, Output_def *, SCM,
    std::set<const Context_def *> *seen) const;

  static bool internal_path_to_bottom_context (Output_def *,
                                               std::vector<Context_def *> *path,
                                               SCM next_type_sym);
};

#endif /* CONTEXT_DEF_HH */
