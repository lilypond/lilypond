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

/* TODO: should junk this class an replace by
   a single list of context modifications?  */

#include "context-def.hh"

#include "context.hh"
#include "context-mod.hh"
#include "international.hh"
#include "ly-scm-list.hh"
#include "output-def.hh"
#include "scm-hash.hh"
#include "translator.hh"
#include "warn.hh"

using std::set;
using std::string;
using std::vector;

static bool
is_instantiable (Context_def *c)
{
  // This looks trivial, but it has semantic value.
  //
  // We can instantiate any context with a definition, but if there were any
  // properties of the definition that could prevent instantiatation, we would
  // also want to check them here.
  return c;
}

Context_def::Context_def ()
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  description_ = SCM_EOL;
  input_location_ = SCM_EOL;

  smobify_self ();

  input_location_ = Input ().smobbed_copy ();
  context_name_ = ly_symbol2scm ("");
}

Input *
Context_def::origin () const
{
  return unsmob<Input> (input_location_);
}

Context_def::Context_def (Context_def const &s)
  : Smob<Context_def> ()
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  description_ = SCM_EOL;
  input_location_ = SCM_EOL;
  smobify_self ();

  description_ = s.description_;
  input_location_ = s.origin ()->smobbed_copy ();
  acceptance_.assign_copy (s.acceptance_);
  property_ops_ = s.property_ops_;
  translator_mods_ = s.translator_mods_;
  context_aliases_ = s.context_aliases_;
  translator_group_type_ = s.translator_group_type_;
  context_name_ = s.context_name_;
}

Context_def::~Context_def ()
{
}

const char *const Context_def::type_p_name_ = "ly:context-def?";

int
Context_def::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Context_def ", port);
  scm_display (context_name_, port);
  scm_puts (" ", port);
  string loc = origin ()->location_string ();
  scm_puts (loc.c_str (), port);
  scm_puts (">", port);
  return 1;
}

SCM
Context_def::mark_smob () const
{
  ASSERT_LIVE_IS_ALLOWED (self_scm ());

  scm_gc_mark (description_);
  scm_gc_mark (context_aliases_);
  acceptance_.gc_mark ();
  scm_gc_mark (translator_mods_);
  scm_gc_mark (property_ops_);
  scm_gc_mark (translator_group_type_);
  scm_gc_mark (input_location_);

  return context_name_;
}

void
Context_def::add_context_mod (SCM mod)
{
  SCM tag = scm_car (mod);
  if (scm_is_eq (tag, ly_symbol2scm ("description")))
    {
      description_ = scm_cadr (mod);
      return;
    }

  /*
    other modifiers take symbols as argument.
  */
  SCM sym = scm_cadr (mod);
  if (scm_is_string (sym))
    sym = scm_string_to_symbol (sym);

  if (scm_is_eq (tag, ly_symbol2scm ("default-child")))
    acceptance_.accept_default (sym);
  else if (scm_is_eq (tag, ly_symbol2scm ("consists"))
           || scm_is_eq (tag, ly_symbol2scm ("remove")))
    {
      translator_mods_ = scm_cons (ly_list (tag, sym), translator_mods_);
    }
  else if (scm_is_eq (tag, ly_symbol2scm ("accepts")))
    acceptance_.accept (sym);
  else if (scm_is_eq (tag, ly_symbol2scm ("denies")))
    acceptance_.deny (sym);
  else if (scm_is_eq (tag, ly_symbol2scm ("pop"))
           || scm_is_eq (tag, ly_symbol2scm ("push"))
           || scm_is_eq (tag, ly_symbol2scm ("assign"))
           || scm_is_eq (tag, ly_symbol2scm ("unset"))
           || scm_is_eq (tag, ly_symbol2scm ("apply")))
    property_ops_ = scm_cons (mod, property_ops_);
  else if (scm_is_eq (tag, ly_symbol2scm ("alias")))
    context_aliases_ = scm_cons (sym, context_aliases_);
  else if (scm_is_eq (tag, ly_symbol2scm ("translator-type")))
    translator_group_type_ = sym;
  else if (scm_is_eq (tag, ly_symbol2scm ("context-name")))
    context_name_ = sym;
  else
    programming_error ("unknown context mod tag");
}

/*
  Given a name of a context that we want to create, finds a list of context
  definitions such that:
   - the first element in the list defines a context that is a valid child of
     the context defined by this Context_def
   - each subsequent element in the list defines a context that is a valid child
     of the context defined by the preceding element in the list
   - the last element in the list defines a context with the given name

  The ACCEPTED parameter is the list of contexts that the caller accepts.  (The
  caller is a Context instantiated from this Context_def, but its acceptance
  list may have been modified from the defined default.)
*/
vector<Context_def *>
Context_def::path_to_acceptable_context (SCM type_sym, Output_def *odef,
                                         SCM accepted) const
{
  set<const Context_def *> seen;
  Context_def *t = unsmob<Context_def> (find_context_def (odef, type_sym));
  return internal_path_to_acceptable_context (type_sym, is_instantiable (t),
                                              odef, accepted, &seen);
}

/*
The SEEN parameter is a set which keeps track of visited contexts, allowing
contexts of the same type to be nested.

When the leaf is instantiable (the usual), we ignore aliases and thereby use
the requested context or nothing.  Example: If the caller requests a Staff, we
do not substitute a RhythmicStaff.

When the leaf is not instantiable, since there would otherwise be nothing worth
doing, we allow substituting an instantiable context that aliases the requested
context.  Example: The caller requests a Timing and the current context would
accept a Score, for which Timing is an alias, so substitute a Score.
*/
vector<Context_def *>
Context_def::internal_path_to_acceptable_context (
  SCM type_sym, bool instantiable, Output_def *odef, SCM accepted,
  set<const Context_def *> *seen) const
{
  assert (scm_is_symbol (type_sym));

  vector<Context_def *> accepteds;
  for (SCM s = accepted; scm_is_pair (s); s = scm_cdr (s))
    {
      Context_def *t
        = unsmob<Context_def> (find_context_def (odef, scm_car (s)));
      if (is_instantiable (t))
        accepteds.push_back (t);
    }

  vector<Context_def *> best_result;
  for (vsize i = 0; i < accepteds.size (); i++)
    {
      bool valid = instantiable
                     ? ly_is_equal (accepteds[i]->get_context_name (), type_sym)
                     : accepteds[i]->is_alias (type_sym);
      if (valid)
        {
          best_result.push_back (accepteds[i]);
          return best_result;
        }
    }

  seen->insert (this);
  vsize best_depth = INT_MAX;
  for (vsize i = 0; (best_depth > 1) && (i < accepteds.size ()); i++)
    {
      Context_def *g = accepteds[i];

      if (!seen->count (g))
        {
          SCM acc = g->acceptance_.get_list ();
          vector<Context_def *> result
            = g->internal_path_to_acceptable_context (type_sym, instantiable,
                                                      odef, acc, seen);
          if (result.size () && result.size () < best_depth)
            {
              best_depth = result.size ();
              result.insert (result.begin (), g);
              best_result = result;
            }
        }
    }
  seen->erase (this);

  return best_result;
}

vector<Context_def *>
Context_def::path_to_bottom_context (Output_def *odef, SCM first_child_type_sym)
{
  vector<Context_def *> path;
  if (!internal_path_to_bottom_context (odef, &path, first_child_type_sym))
    path.clear ();
  return path;
}

bool
Context_def::internal_path_to_bottom_context (Output_def *odef,
                                              vector<Context_def *> *path,
                                              SCM next_type_sym)
{
  if (!scm_is_symbol (next_type_sym))
    return true; // the caller is the bottom

  Context_def *t = unsmob<Context_def> (find_context_def (odef, next_type_sym));
  if (!is_instantiable (t))
    {
      warning (_f ("cannot create default child context: %s",
                   Context::diagnostic_id (next_type_sym, "").c_str ()));
      return false;
    }

  if (std::find (path->begin (), path->end (), t) != path->end ())
    {
      warning (_f ("default child context begins a cycle: %s",
                   Context::diagnostic_id (next_type_sym, "").c_str ()));
      return false;
    }

  path->push_back (t);
  return internal_path_to_bottom_context (odef, path,
                                          t->acceptance_.get_default ());
}

SCM
Context_def::get_translator_names (SCM user_mod) const
{
  SCM mods = scm_reverse_x (scm_list_copy (translator_mods_), user_mod);
  // Incrementally build the set of translators for this context.  A \consists
  // command adds to the set.  Duplicates are avoided by storing the index of
  // the last \consists command for a given translator in the added_index hash
  // table.  The reason for avoiding duplicates without a warning is that this
  // can be useful if several independent commands add the same translator.
  // After the first iteration, the list is filtered for indices where the value
  // in added_index matches, to let the order keep the last \consists.  A
  // \remove command removes from the set, implemented as removing the
  // corresponding value in added_index.  (This could also be implemented with
  // just scm_memq and scm_delq, but that would be a quadratic algorithm.)
  Scheme_hash_table *added_index
    = unsmob<Scheme_hash_table> (Scheme_hash_table::make_smob ());
  auto iter_mods = [&] (auto func) {
    vsize i = 0;
    for (SCM p : as_ly_scm_list (mods))
      {
        SCM tag = scm_car (p);
        SCM arg = scm_cadr (p);
        if (scm_is_string (arg))
          arg = scm_string_to_symbol (arg);
        func (i, tag, arg);
        i++;
      }
  };
  iter_mods ([&] (vsize i, SCM tag, SCM arg) {
    if (scm_is_eq (tag, ly_symbol2scm ("consists")))
      added_index->set (arg, to_scm (i));
    else if (scm_is_eq (tag, ly_symbol2scm ("remove")))
      added_index->remove (arg);
  });
  SCM ret = SCM_EOL;
  iter_mods ([&] (vsize i, SCM tag, SCM arg) {
    if (scm_is_eq (tag, ly_symbol2scm ("consists")))
      {
        SCM index = added_index->get (arg);
        if (!SCM_UNBNDP (index) && from_scm<vsize> (index) == i)
          ret = scm_cons (arg, ret);
      }
  });
  // Note that the list is returned in reverse order.
  return ret;
}

SCM
Context_def::make_scm ()
{
  Context_def *t = new Context_def;
  return t->unprotect ();
}

void
Context_def::apply_default_property_operations (Context *tg)
{
  apply_property_operations (tg, scm_reverse (property_ops_));
}

SCM
Context_def::to_alist () const
{
  SCM ell = SCM_EOL;

  ell = scm_cons (
    scm_cons (ly_symbol2scm ("consists"), get_translator_names (SCM_EOL)), ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("description"), description_), ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("aliases"), context_aliases_), ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("accepts"), acceptance_.get_list ()),
                  ell);
  if (acceptance_.has_default ())
    {
      ell = scm_acons (ly_symbol2scm ("default-child"),
                       acceptance_.get_default (), ell);
    }
  ell
    = scm_cons (scm_cons (ly_symbol2scm ("property-ops"), property_ops_), ell);
  ell
    = scm_cons (scm_cons (ly_symbol2scm ("context-name"), context_name_), ell);

  if (scm_is_symbol (translator_group_type_))
    ell = scm_cons (
      scm_cons (ly_symbol2scm ("group-type"), translator_group_type_), ell);
  return ell;
}

SCM
Context_def::lookup (SCM sym) const
{
  if (scm_is_eq (ly_symbol2scm ("default-child"), sym))
    return acceptance_.get_default ();
  else if (scm_is_eq (ly_symbol2scm ("consists"), sym))
    return get_translator_names (SCM_EOL);
  else if (scm_is_eq (ly_symbol2scm ("description"), sym))
    return description_;
  else if (scm_is_eq (ly_symbol2scm ("aliases"), sym))
    return context_aliases_;
  else if (scm_is_eq (ly_symbol2scm ("accepts"), sym))
    return acceptance_.get_list ();
  else if (scm_is_eq (ly_symbol2scm ("property-ops"), sym))
    return property_ops_;
  else if (scm_is_eq (ly_symbol2scm ("context-name"), sym))
    return context_name_;
  else if (scm_is_eq (ly_symbol2scm ("group-type"), sym))
    return translator_group_type_;
  return SCM_UNDEFINED;
}

bool
Context_def::is_alias (SCM sym) const
{
  if (scm_is_eq (sym, ly_symbol2scm ("Bottom")))
    return !acceptance_.has_default ();

  if (scm_is_eq (sym, get_context_name ()))
    return true;

  return scm_is_true (scm_c_memq (sym, context_aliases_));
}

LY_DEFINE (ly_context_def_lookup, "ly:context-def-lookup", 2, 1, 0,
           (SCM def, SCM sym, SCM val),
           R"(
Return the value of @var{sym} in context definition @var{def} (e.g.,
@code{\Voice}).  If no value is found, return @var{val} or @code{'()} if
@var{val} is undefined.  @var{sym} can be any of @samp{default-child},
@samp{consists}, @samp{description}, @samp{aliases}, @samp{accepts},
@samp{property-ops}, @samp{context-name}, @samp{group-type}.
           )")
{
  auto *const cd = LY_ASSERT_SMOB (Context_def, def, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  SCM res = cd->lookup (sym);

  scm_remember_upto_here_1 (def);

  if (SCM_UNBNDP (res))
    res = SCM_EOL;

  if (scm_is_null (res) && !SCM_UNBNDP (val))
    return val;

  return res;
}

LY_DEFINE (ly_context_def_modify, "ly:context-def-modify", 2, 0, 0,
           (SCM def, SCM mod),
           R"(
Return the result of applying the context-mod @var{mod} to the context
definition @var{def}.  Does not change @var{def}.
           )")
{
  auto *const orig_cd = LY_ASSERT_SMOB (Context_def, def, 1);
  auto *const cm = LY_ASSERT_SMOB (Context_mod, mod, 2);

  auto *cd = orig_cd->clone ();

  for (SCM s = cm->get_mods (); scm_is_pair (s); s = scm_cdr (s))
    cd->add_context_mod (scm_car (s));

  return cd->unprotect ();
}
