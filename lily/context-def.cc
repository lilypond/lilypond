/*   
  translator-def.cc --  implement Context_def

  source file of the GNU LilyPond music typesetter

  (c) 2000--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

/* TODO: should junk this class an replace by
   a single list of context modifications?  */

#include "context-def.hh"
#include "engraver.hh"
#include "lily-proto.hh"
#include "output-def.hh"
#include "performer.hh"
#include "score-context.hh"
#include "translator-group.hh"
#include "warn.hh"

Context_def::Context_def ()
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accept_mods_ = SCM_EOL;
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  description_ = SCM_EOL;

  smobify_self ();

  context_name_ = ly_symbol2scm ("");
}

Context_def::Context_def (Context_def const & s)
  : Input (s)
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accept_mods_ = SCM_EOL;   
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  description_ = SCM_EOL;
  
  smobify_self ();
  description_ = s.description_;

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
  Context_def* me = (Context_def*) SCM_CELL_WORD_1 (smob);

  scm_puts ("#<Context_def ", port);
  scm_display (me->context_name_, port);
  scm_puts (">", port);
  return 1;
}

SCM
Context_def::mark_smob (SCM smob)
{
  Context_def* me = (Context_def*) SCM_CELL_WORD_1 (smob);

  scm_gc_mark (me->description_);
  scm_gc_mark (me->context_aliases_);
  scm_gc_mark (me->accept_mods_);
  scm_gc_mark (me->translator_mods_);
  scm_gc_mark (me->property_ops_);  
  scm_gc_mark (me->translator_group_type_);

  return me->context_name_;
}

void
Context_def::add_context_mod (SCM mod)
{
  SCM tag = ly_car (mod);
  if (ly_symbol2scm ("description") == tag)
    {
      description_ = ly_cadr (mod);
      return;
    }

  SCM sym = ly_cadr (mod);
  if (ly_c_string_p (sym))
    sym = scm_string_to_symbol (sym);
  
  if (ly_symbol2scm ("consists") == tag
      || ly_symbol2scm ("consists-end") == tag
      || ly_symbol2scm ("remove") == tag)
    {
      if (!get_translator (sym))
	error (_f ("Program has no such type: `%s'",
		   ly_symbol2string (sym).to_str0 ()));
      else
	translator_mods_ = scm_cons (scm_list_2 (tag, sym), translator_mods_);
    }
  else if (ly_symbol2scm ("accepts") == tag
	   || ly_symbol2scm ("denies") == tag)
    accept_mods_ = scm_cons (scm_list_2 (tag, sym), accept_mods_); 
  else if (ly_symbol2scm ("poppush") == tag
	   || ly_symbol2scm ("pop") == tag
	   || ly_symbol2scm ("push") == tag
	   || ly_symbol2scm ("assign") == tag
	   || ly_symbol2scm ("unset") == tag)
    property_ops_ = scm_cons (mod, property_ops_);
  else if (ly_symbol2scm ("alias") == tag)
    context_aliases_ = scm_cons (sym, context_aliases_);
  else if (ly_symbol2scm ("translator-type")  == tag)
    translator_group_type_ = sym;
  else if (ly_symbol2scm ("context-name")  == tag)
    context_name_ = sym;
  else
    programming_error ("Unknown context mod tag.");
}

SCM
Context_def::get_context_name () const
{
  return context_name_;
}

SCM
Context_def::get_accepted (SCM user_mod) const
{
  SCM mods = scm_reverse_x (scm_list_copy (accept_mods_), user_mod);
  SCM acc = SCM_EOL;
  for (SCM s = mods; ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM tag = ly_caar (s);
      SCM sym = ly_cadar (s);
      if (tag == ly_symbol2scm ("accepts"))
	acc = scm_cons (sym, acc);
      else if (tag == ly_symbol2scm ("denies"))
	acc = scm_delete_x (sym, acc);
    }
  return acc;
}

Link_array<Context_def>
Context_def::path_to_acceptable_context (SCM type_sym, Output_def *odef) const
{
  assert (ly_c_symbol_p (type_sym));
  
  SCM accepted = get_accepted (SCM_EOL);

  Link_array<Context_def> accepteds;
  for (SCM s = accepted; ly_c_pair_p (s); s = ly_cdr (s))
    if (Context_def *t = unsmob_context_def (find_context_def (odef,
							       ly_car (s))))
      accepteds.push (t);

  Link_array<Context_def> best_result;
  for (int i=0; i < accepteds.size (); i++)
    {
      /* do not check aliases, because \context Staff should not
	 create RhythmicStaff. */
      if (ly_c_equal_p (accepteds[i]->get_context_name (), type_sym))
	{
	  best_result.push (accepteds[i]);
	  return best_result;
	}
    }

  int best_depth= INT_MAX;
  for (int i=0; i < accepteds.size (); i++)
    {
      Context_def * g = accepteds[i];

      Link_array<Context_def> result
	= g->path_to_acceptable_context (type_sym, odef);
      if (result.size () && result.size () < best_depth)
	{
	  result.insert (g,0);
	  best_result = result;

	  /* this following line was added in 1.9.3, but hsould've been
	     there all along... Let's hope it doesn't cause nightmares.  */
	  best_depth = result.size ();
	}
    }

  return best_result;
}

SCM
Context_def::get_translator_names (SCM user_mod) const
{
  SCM l1 = SCM_EOL;

  SCM mods = scm_reverse_x (scm_list_copy (translator_mods_), user_mod);
  
  for (SCM s = mods; ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM tag = ly_caar (s);
      SCM arg = ly_cadar (s);

      if (ly_c_string_p (arg))
	arg = scm_string_to_symbol (arg);
      
      if (ly_symbol2scm ("consists") == tag)
	l1 = scm_cons (arg, l1);
      else if (ly_symbol2scm ("remove") == tag)
	{
	  l1 = scm_delete_x (arg, l1);
	}
    }

  return l1;
}

SCM
filter_performers (SCM ell)
{
  for (SCM *tail = &ell; ly_c_pair_p (*tail); tail = SCM_CDRLOC (*tail))
    {
      if (dynamic_cast<Performer*> (unsmob_translator (ly_car (*tail))))
	{
	  *tail = ly_cdr (*tail);
	  if (!ly_c_pair_p (*tail))
	    break ;
	}
    }
  return ell;
}

SCM
filter_engravers (SCM ell)
{
  SCM *tail = &ell;  
  for (; ly_c_pair_p (*tail) ; tail = SCM_CDRLOC (*tail))
    {
      if (dynamic_cast<Engraver*> (unsmob_translator (ly_car (*tail))))
	{
	  *tail = ly_cdr (*tail);
	  if (!ly_c_pair_p (*tail))
	    break ;
	}
    }
  return ell;
}


Context *
Context_def::instantiate (SCM ops)
{
  Context *tg =  0;

  if (context_name_ == ly_symbol2scm ("Score"))
    tg = new Score_context ();
  else
    tg = new Context ();

  tg->definition_ = self_scm ();

  SCM trans_names = get_translator_names (ops); 

  Translator *g = get_translator (translator_group_type_);
  g = g->clone ();

  SCM trans_list = SCM_EOL;
  
  for (SCM s = trans_names; ly_c_pair_p (s) ; s = ly_cdr (s))
    {
      Translator *t = get_translator (ly_car (s));
      if (!t)
	warning (_f ("can't find: `%s'", s));
      else
	{
	  Translator *tr = t->clone ();
	  SCM str = tr->self_scm ();

	  if (tr->must_be_last ())
	    {
	      SCM cons = scm_cons (str, SCM_EOL);
	      trans_list = ly_c_pair_p (trans_list)
		? scm_set_cdr_x (scm_last_pair (trans_list), cons)
		: cons;
	    }
	  else
	    {
	      trans_list = scm_cons (str, trans_list);
	    }

	  tr->daddy_context_ = tg;
	  scm_gc_unprotect_object (str);
	}
    }
  
  g->simple_trans_list_ =  trans_list;

  tg->implementation_ = g->self_scm ();
  if (dynamic_cast<Engraver*> (g))
    g->simple_trans_list_ = filter_performers (g->simple_trans_list_);
  else if (dynamic_cast<Performer*> (g))
    g->simple_trans_list_ = filter_engravers (g->simple_trans_list_);
	
  g->daddy_context_ = tg;
  tg->aliases_ = context_aliases_ ;
  
  scm_gc_unprotect_object (g->self_scm ());
  
  tg->accepts_list_ = get_accepted  (ops);
  
  return tg;
}

SCM
Context_def::clone_scm () const
{
  Context_def * t = new Context_def (*this);
  SCM x = t->self_scm ();
  scm_gc_unprotect_object (x);
  return x;
}

SCM
Context_def::make_scm ()
{
  Context_def* t = new Context_def;
  SCM x = t->self_scm ();
  scm_gc_unprotect_object (x);
  return x;
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

  if (ly_c_symbol_p (translator_group_type_))
    ell = scm_cons (scm_cons (ly_symbol2scm ("group-type"),
			      translator_group_type_), ell);    
  return ell;  
}

