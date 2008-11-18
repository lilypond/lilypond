/*
  context-def.cc -- implement Context_def

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

/* TODO: should junk this class an replace by
   a single list of context modifications?  */

#include "context-def.hh"

#include "context.hh"
#include "international.hh"
#include "output-def.hh"
#include "translator.hh"
#include "warn.hh"

Context_def::Context_def ()
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accept_mods_ = SCM_EOL;
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  default_child_ = SCM_EOL;
  description_ = SCM_EOL;
  input_location_ = SCM_EOL;

  smobify_self ();

  input_location_ = make_input (Input ());
  context_name_ = ly_symbol2scm ("");
}

Input *
Context_def::origin () const
{
  return unsmob_input (input_location_);
}

Context_def::Context_def (Context_def const &s)
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accept_mods_ = SCM_EOL;
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  description_ = SCM_EOL;
  default_child_ = SCM_EOL;
  input_location_ = SCM_EOL;
  smobify_self ();

  description_ = s.description_;
  input_location_ = make_input (*s.origin ()); 
  default_child_ = s.default_child_;
  accept_mods_ = s.accept_mods_;
  property_ops_ = s.property_ops_;
  translator_mods_ = s.translator_mods_;
  context_aliases_ = s.context_aliases_;
  translator_group_type_ = s.translator_group_type_;
  context_name_ = s.context_name_;
}

Context_def::~Context_def ()
{
}

#include "ly-smobs.icc"
IMPLEMENT_SMOBS (Context_def);
IMPLEMENT_DEFAULT_EQUAL_P (Context_def);

int
Context_def::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Context_def *me = (Context_def *) SCM_CELL_WORD_1 (smob);

  scm_puts ("#<Context_def ", port);
  scm_display (me->context_name_, port);
  scm_puts (">", port);
  return 1;
}

SCM
Context_def::mark_smob (SCM smob)
{
  ASSERT_LIVE_IS_ALLOWED ();
  
  Context_def *me = (Context_def *) SCM_CELL_WORD_1 (smob);

  scm_gc_mark (me->description_);
  scm_gc_mark (me->context_aliases_);
  scm_gc_mark (me->accept_mods_);
  scm_gc_mark (me->translator_mods_);
  scm_gc_mark (me->property_ops_);
  scm_gc_mark (me->translator_group_type_);
  scm_gc_mark (me->default_child_);
  scm_gc_mark (me->input_location_);

  return me->context_name_;
}

void
Context_def::add_context_mod (SCM mod)
{
  SCM tag = scm_car (mod);
  if (ly_symbol2scm ("description") == tag)
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

  if (ly_symbol2scm ("default-child") == tag)
    default_child_ = sym;
  else if (ly_symbol2scm ("consists") == tag
	   || ly_symbol2scm ("consists-end") == tag
	   || ly_symbol2scm ("remove") == tag)
    {
      if (!get_translator (sym))
	error (_f ("program has no such type: `%s'",
		   ly_symbol2string (sym).c_str ()));
      else
	translator_mods_ = scm_cons (scm_list_2 (tag, sym), translator_mods_);
    }
  else if (ly_symbol2scm ("accepts") == tag
	   || ly_symbol2scm ("denies") == tag)
    accept_mods_ = scm_cons (scm_list_2 (tag, sym), accept_mods_);
  else if (ly_symbol2scm ("pop") == tag
	   || ly_symbol2scm ("push") == tag
	   || ly_symbol2scm ("assign") == tag
	   || ly_symbol2scm ("unset") == tag)
    property_ops_ = scm_cons (mod, property_ops_);
  else if (ly_symbol2scm ("alias") == tag)
    context_aliases_ = scm_cons (sym, context_aliases_);
  else if (ly_symbol2scm ("translator-type") == tag)
    translator_group_type_ = sym;
  else if (ly_symbol2scm ("context-name") == tag)
    context_name_ = sym;
  else
    programming_error ("unknown context mod tag");
}

SCM
Context_def::get_accepted (SCM user_mod) const
{
  SCM mods = scm_reverse_x (scm_list_copy (accept_mods_), user_mod);
  SCM acc = SCM_EOL;
  for (SCM s = mods; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM tag = scm_caar (s);
      SCM sym = scm_cadar (s);
      if (tag == ly_symbol2scm ("accepts"))
	acc = scm_cons (sym, acc);
      else if (tag == ly_symbol2scm ("denies"))
	acc = scm_delete_x (sym, acc);
    }

  SCM def = get_default_child (user_mod);
  if (scm_is_symbol (def))
    {
      if (scm_memq (def, acc))
	acc = scm_delete_x (def, acc);
      acc = scm_cons (def, acc);
    }

  return acc;
}

SCM
Context_def::get_default_child (SCM user_mod) const
{
  SCM name = default_child_;
  for (SCM s = user_mod; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (scm_car (entry) == ly_symbol2scm ("default-child"))
	{
	  name = scm_cadr (entry);
	  break;
	}
    }

  return name;
}

/*
  Given a name of a context that we want to create, finds a list of context
  definitions such that:
   - the first element in the list defines a context that is a valid child of
     the context defined by this Context_def
   - each subsequent element in the list defines a context that is a valid child
     of the the context defined by the preceding element in the list
   - the last element in the list defines a context with the given name

  The ADDITIONAL_ACCEPTS parameter is a list of additional contexts that this
  specific context def (but not any of the child context defs) should accept.
*/
vector<Context_def *>
Context_def::path_to_acceptable_context (SCM type_sym,
					 Output_def *odef,
					 SCM additional_accepts) const
{
  set<const Context_def *> seen;
  return internal_path_to_acceptable_context (type_sym, odef, additional_accepts, &seen);
}

/*
The SEEN parameter is a set which keeps track of visited contexts, allowing
contexts of the same type to be nested.
*/
vector<Context_def*>
Context_def::internal_path_to_acceptable_context (SCM type_sym,
						  Output_def *odef,
						  SCM additional_accepts,
						  set<const Context_def *> *seen) const
{
  assert (scm_is_symbol (type_sym));

  SCM accepted = get_accepted (additional_accepts);

  vector<Context_def*> accepteds;
  for (SCM s = accepted; scm_is_pair (s); s = scm_cdr (s))
    if (Context_def *t = unsmob_context_def (find_context_def (odef,
							       scm_car (s))))
      accepteds.push_back (t);

  vector<Context_def*> best_result;
  for (vsize i = 0; i < accepteds.size (); i++)
    {
      /* do not check aliases, because \context Staff should not
	 create RhythmicStaff. */
      if (ly_is_equal (accepteds[i]->get_context_name (), type_sym))
	{
	  best_result.push_back (accepteds[i]);
	  return best_result;
	}
    }

  seen->insert (this);
  vsize best_depth = INT_MAX;
  for (vsize i = 0; i < accepteds.size (); i++)
    {
      Context_def *g = accepteds[i];

      if (!seen->count (g))
	{
	  vector<Context_def*> result
	    = g->internal_path_to_acceptable_context (type_sym, odef, SCM_EOL, seen);
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

SCM
Context_def::get_translator_names (SCM user_mod) const
{
  SCM l1 = SCM_EOL;

  SCM mods = scm_reverse_x (scm_list_copy (translator_mods_), user_mod);

  for (SCM s = mods; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM tag = scm_caar (s);
      SCM arg = scm_cadar (s);

      if (scm_is_string (arg))
	arg = scm_string_to_symbol (arg);

      if (ly_symbol2scm ("consists") == tag)
	l1 = scm_cons (arg, l1);
      else if (ly_symbol2scm ("remove") == tag)
	l1 = scm_delete_x (arg, l1);
    }

  return l1;
}

Context *
Context_def::instantiate (SCM ops)
{
  Context *context = new Context ();

  context->definition_ = self_scm ();
  context->definition_mods_ = ops;
  context->aliases_ = context_aliases_;
  context->accepts_list_ = get_accepted (ops);

  return context;
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
  apply_property_operations (tg, property_ops_);
}

SCM
Context_def::to_alist () const
{
  SCM ell = SCM_EOL;

  ell = scm_cons (scm_cons (ly_symbol2scm ("consists"),
			    get_translator_names (SCM_EOL)), ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("description"), description_), ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("aliases"), context_aliases_), ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("accepts"), get_accepted (SCM_EOL)),
		  ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("property-ops"), property_ops_),
		  ell);
  ell = scm_cons (scm_cons (ly_symbol2scm ("context-name"), context_name_),
		  ell);

  if (scm_is_symbol (translator_group_type_))
    ell = scm_cons (scm_cons (ly_symbol2scm ("group-type"),
			      translator_group_type_), ell);
  return ell;
}

