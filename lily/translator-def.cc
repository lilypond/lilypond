/*   
  translator-def.cc --  implement Translator_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lily-proto.hh"
#include "translator-def.hh"
#include "translator-group.hh"
#include "warn.hh"
#include "music-output-def.hh"
#include "ly-smobs.icc"

int
Translator_def::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Translator_def* me = (Translator_def*) SCM_CELL_WORD_1 (smob);

  scm_puts ("#<Translator_def ", port);
  scm_display (me->context_name_, port);
  scm_puts (">", port);
  return 1;
}


SCM
Translator_def::mark_smob (SCM smob)
{
  Translator_def* me = (Translator_def*) SCM_CELL_WORD_1 (smob);

  scm_gc_mark (me->description_);
  scm_gc_mark (me->context_aliases_);
  scm_gc_mark (me->accept_mods_);
  scm_gc_mark (me->translator_mods_);
  scm_gc_mark (me->property_ops_);  
  scm_gc_mark (me->translator_group_type_);
  return me->context_name_;
}


Translator_def::Translator_def ()
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accept_mods_ = SCM_EOL;
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  description_ = SCM_EOL;

  smobify_self();
}

Translator_def::~Translator_def ()
{
}

Translator_def::Translator_def (Translator_def const & s)
  : Input (s)
{
  context_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accept_mods_ = SCM_EOL;   
  translator_mods_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  context_name_ = SCM_EOL;
  description_ = SCM_EOL;
  
  smobify_self();
  description_ = s.description_;


  accept_mods_ = s.accept_mods_;
  property_ops_ = s.property_ops_;
  translator_mods_ = s.translator_mods_;
  context_aliases_ = s.context_aliases_;
  translator_group_type_ = s.translator_group_type_;
  context_name_ = s.context_name_;
}


void
Translator_def::add_context_mod (SCM mod)
{
  SCM tag  = gh_car (mod);
  if (ly_symbol2scm ("description")  == tag)
    {
      description_ = gh_cadr (mod);
      return ;
    }

  SCM sym = gh_cadr (mod);
  if (gh_string_p (sym))
    sym = scm_string_to_symbol (sym);
  
  if (ly_symbol2scm ("consists") == tag
      || ly_symbol2scm ("consists-end") == tag
      || ly_symbol2scm ("remove") == tag)
    {
      if (!get_translator (sym))
	error (_f ("Program has no such type: `%s'", ly_symbol2string (sym).to_str0 ()));
      else
	translator_mods_ = gh_cons (scm_list_2 (tag, sym), translator_mods_ );
    }
  else if (ly_symbol2scm ("accepts") == tag
	   || ly_symbol2scm ("denies") == tag)
    {
      accept_mods_ = gh_cons (scm_list_2 (tag, sym), accept_mods_); 
    }
  else if (ly_symbol2scm ("poppush") == tag
	   || ly_symbol2scm ("pop") == tag
	   || ly_symbol2scm ("push") == tag
	   || ly_symbol2scm ("assign") == tag
	   || ly_symbol2scm ("unset") == tag)
    {
      property_ops_ = gh_cons (mod, property_ops_);
    }
  else if (ly_symbol2scm ("alias") == tag)
    {
      context_aliases_ = gh_cons (sym, context_aliases_);
    }
  else if (ly_symbol2scm ("translator-type")  == tag)
    {
      translator_group_type_ = sym;
    }
  else if (ly_symbol2scm ("context-name")  == tag)
    {
      context_name_ = sym;
    }
  else
    {
      programming_error ("Unknown context mod tag.");
    }
}

SCM
Translator_def::get_translator_names () const
{
  SCM l1 = SCM_EOL;
  SCM l2 = SCM_EOL;

  SCM mods = scm_reverse (translator_mods_);
  for (SCM s = mods; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM tag = gh_caar (s);
      SCM arg = gh_cadar (s);

      if (ly_symbol2scm ("consists") == tag)
	l1 = gh_cons (arg, l1);
      else if (ly_symbol2scm ("consists-end") == tag)
	l2 = gh_cons (arg, l2);
      else if (ly_symbol2scm ("remove") == tag)
	{
	  l1 = scm_delete_x (arg, l1);
	  l2 = scm_delete_x (arg, l2);
	}
    }

  return scm_append_x (scm_list_2 (l1, l2));
}

SCM
Translator_def::get_context_name () const
{
  return context_name_;
}

SCM
Translator_def::get_accepted () const
{
  SCM correct_order = scm_reverse (accept_mods_);
  SCM acc = SCM_EOL;
  for (SCM s = correct_order; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM tag = gh_caar (s);
      SCM sym = gh_cadar (s);
      if (tag == ly_symbol2scm ("accepts"))
	acc = gh_cons (sym, acc);
      else if (tag == ly_symbol2scm ("denies"))
	acc = scm_delete_x (sym, acc);
    }
  return acc;
}

	   
Link_array<Translator_def>
Translator_def::path_to_acceptable_translator (SCM type_sym, Music_output_def* odef) const
{
  assert (gh_symbol_p (type_sym));
  
  SCM accepted = get_accepted ();

  Link_array<Translator_def> accepteds;
  for (SCM s = accepted; gh_pair_p (s); s = ly_cdr (s))
    {
      Translator_def *t = unsmob_translator_def (odef->find_translator (ly_car (s)));
      if (!t)
	continue;
      accepteds.push (t);
    }

  Link_array<Translator_def> best_result;
  for (int i=0; i < accepteds.size (); i++)
    {
      /*
	don't check aliases, because \context Staff should not create RhythmicStaff.
      */
      if (gh_equal_p (accepteds[i]->get_context_name (), type_sym))
	{
	  best_result.push (accepteds[i]);
	  return best_result;
	}
    }
      
  int best_depth= INT_MAX;
  for (int i=0; i < accepteds.size (); i++)
    {
      Translator_def * g = accepteds[i];

      Link_array<Translator_def> result
	= g->path_to_acceptable_translator (type_sym, odef);
      if (result.size () && result.size () < best_depth)
	{
	  result.insert (g,0);
	  best_result = result;

	  /*
	    this following line was added in 1.9.3, but hsould've been
	    there all along... Let's hope it doesn't cause nightmares.
	   */
	  best_depth = result.size();
	}
    }

  return best_result;
}

IMPLEMENT_SMOBS (Translator_def);
IMPLEMENT_DEFAULT_EQUAL_P (Translator_def);


static SCM
names_to_translators (SCM namelist, Translator_group*tg)
{
  SCM l = SCM_EOL;
  for (SCM s = namelist; gh_pair_p (s) ; s = ly_cdr (s))
    {
      Translator * t = get_translator (ly_car (s));
      if (!t)
	warning (_f ("can't find: `%s'", s));
      else
	{
	  Translator * tr = t->clone ();
	  SCM str = tr->self_scm ();
	  l = gh_cons (str, l);

	  tr->daddy_trans_ = tg;
	  tr->output_def_  = tg->output_def_;

	  scm_gc_unprotect_object (str);
	}
    }
  return l; 
}


Translator_group *
Translator_def::instantiate (Music_output_def* md)
{
  Translator * g = get_translator (translator_group_type_);
  g = g->clone (); 

  Translator_group *tg = dynamic_cast<Translator_group*> (g);
  tg->output_def_ = md;
  tg->definition_ = self_scm ();

  SCM trans_names = get_translator_names (); 
  tg->simple_trans_list_ = names_to_translators (trans_names, tg);
  
  return tg;
}

void
Translator_def::apply_default_property_operations (Translator_group*tg)
{
  apply_property_operations (tg, property_ops_);
}


SCM
Translator_def::clone_scm () const
{
  Translator_def * t = new Translator_def (*this);
  scm_gc_unprotect_object (t->self_scm());
  return t->self_scm();
}

SCM
Translator_def::make_scm ()
{
  Translator_def* t = new Translator_def;
  scm_gc_unprotect_object (t->self_scm());
  return t->self_scm();
}


/*
  Default child context as a SCM string, or something else if there is
  none.
*/
SCM
Translator_def::default_child_context_name ()
{
  SCM d = get_accepted ();
  return gh_pair_p (d) ? ly_car (scm_last_pair (d)) : SCM_EOL;
}

SCM
Translator_def::to_alist () const
{
  SCM l = SCM_EOL;

  l = gh_cons (gh_cons (ly_symbol2scm ("consists"),  get_translator_names ()), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("description"),  description_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("aliases"),  context_aliases_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("accepts"),  get_accepted ()), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("property-ops"),  property_ops_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("context-name"),  context_name_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("group-type"),  translator_group_type_), l);    

  return l;  
}

bool
Translator_def::is_alias (SCM sym) const
{
  bool b  = sym == context_name_;

  for (SCM a = context_aliases_; !b && gh_pair_p (a); a = ly_cdr (a))
    b = b || sym == ly_car (a);

  return b;
}
