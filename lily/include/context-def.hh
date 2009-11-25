/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "std-vector.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "input.hh"
#include "virtual-methods.hh"
#include <set>


/*
  The definition of a interpretation context as given in the
  input. The lists are stored in order of definition.
*/
struct Context_def
{
private:
  /*
    these lists store the definition, in opposite order of entry
  */
  SCM translator_mods_;
  SCM accept_mods_;
  SCM property_ops_;
  SCM description_;
  SCM context_name_;
  SCM context_aliases_;
  SCM translator_group_type_;
  SCM default_child_;
  SCM input_location_;
public:
  Input *origin () const;
  void add_context_mod (SCM);
  SCM get_default_child (SCM user_mods) const;
  SCM get_context_name () const { return context_name_; }
  SCM get_accepted (SCM user_mods) const;
  SCM get_property_ops () const { return property_ops_; }
  SCM get_translator_names (SCM) const;
  SCM get_translator_group_type () const { return translator_group_type_; }
  void set_acceptor (SCM accepts, bool add);

  VIRTUAL_COPY_CONSTRUCTOR(Context_def, Context_def);

  vector<Context_def*> path_to_acceptable_context (SCM type_string,
						   Output_def *,
						   SCM) const;
  vector<Context_def*> internal_path_to_acceptable_context (SCM type_string,
							    Output_def *,
							    SCM,
							    set<const Context_def *> *seen) const;
  Context *instantiate (SCM extra_ops);

  SCM to_alist () const;
  static SCM make_scm ();

  void apply_default_property_operations (Context *);

private:
  DECLARE_SMOBS (Context_def);
  Context_def ();
  Context_def (Context_def const &);
};

DECLARE_UNSMOB (Context_def, context_def);

#endif /* CONTEXT_DEF_HH */

