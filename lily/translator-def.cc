/*   
  translator-def.cc --  implement Translator_def
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  scm_display (me->type_name_, port);
  scm_puts (">", port);
  return 1;
}


SCM
Translator_def::mark_smob (SCM smob)
{
  Translator_def* me = (Translator_def*) SCM_CELL_WORD_1 (smob);

  scm_gc_mark (me->type_aliases_);
  scm_gc_mark (me->consists_name_list_);
  scm_gc_mark (me->accepts_name_list_);
  scm_gc_mark (me->end_consists_name_list_);
  scm_gc_mark (me->property_ops_);  
  scm_gc_mark (me->translator_group_type_);
  return me->type_name_;
}


Translator_def::Translator_def ()
{
  type_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accepts_name_list_ = SCM_EOL;   
  consists_name_list_ = SCM_EOL;
  end_consists_name_list_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  type_name_ = SCM_EOL;

  smobify_self();
  
}

Translator_def::~Translator_def ()
{
}

Translator_def::Translator_def (Translator_def const & s)
  : Input (s)
{
  type_aliases_ = SCM_EOL;
  translator_group_type_ = SCM_EOL;
  accepts_name_list_ = SCM_EOL;   
  consists_name_list_ = SCM_EOL;
  end_consists_name_list_ = SCM_EOL;
  property_ops_ = SCM_EOL;
  type_name_ = SCM_EOL;

  
  smobify_self();
  consists_name_list_ = scm_list_copy (s.consists_name_list_);
  end_consists_name_list_ = scm_list_copy (s.end_consists_name_list_);
  accepts_name_list_ = scm_list_copy (s.accepts_name_list_);
  property_ops_ = scm_list_copy (s.property_ops_);
  type_aliases_ = scm_list_copy (s.type_aliases_);
  translator_group_type_ = s.translator_group_type_;
  type_name_ = s.type_name_;
}



void
Translator_def::set_acceptor (SCM name, bool add)
{
  if (add)
    this->accepts_name_list_ = gh_cons (name, this->accepts_name_list_);
  else
    this->accepts_name_list_ = scm_delete_x (name, this->accepts_name_list_);
}


SCM
Translator_def::modify_definition (SCM list, SCM str, bool add)
{
  String s = ly_scm2string (str);
  if (!get_translator (s))
    error (_ ("Program has no such type"));

  if (add)
    {
      if (scm_memq (str, list) != SCM_BOOL_F)
	{
	  warning (_f ("Already contains: `%s'", s));
	  warning (_f ("Not adding translator: `%s'", s));
	}
      else
	list= gh_cons (str, list);
    }
  else
    {
      list = scm_delete_x (str, list);
    }
  return list;
}



void
Translator_def::remove_element (SCM s)
{
  this->end_consists_name_list_ = modify_definition (this->end_consists_name_list_, s, false);
  this->consists_name_list_ = modify_definition (this->consists_name_list_, s, false);
}

void
Translator_def::add_element (SCM s)
{
  this->consists_name_list_ = modify_definition (this->consists_name_list_, s, true);
}

void
Translator_def::add_last_element (SCM s)
{
  this->end_consists_name_list_ = modify_definition (this->end_consists_name_list_, s, true);
}
void
Translator_def::add_push_property (SCM props, SCM syms,  SCM vals)
{
  this->property_ops_ = gh_cons (scm_list_n (ly_symbol2scm ("push"), props, syms, vals, SCM_UNDEFINED),
				 this->property_ops_);
}

void
Translator_def::add_pop_property (SCM props, SCM syms)
{
  this->property_ops_ = gh_cons (scm_list_n (ly_symbol2scm ("push"), props, syms, SCM_UNDEFINED),
				 this->property_ops_);
}

/*
  Do it. SYM is single symbol. VAL is SCM_UNDEFINED in case of a pop
*/
void
Translator_def::apply_pushpop_property (Translator_group* me,SCM sym, SCM eprop, SCM val)
{
  dynamic_cast<Translator_group*> (me)
    ->execute_single_pushpop_property (sym, eprop, val);
}



Link_array<Translator_def>
Translator_def::path_to_acceptable_translator (SCM type_string, Music_output_def* odef) const
{
  assert (gh_string_p (type_string));
  
  Link_array<Translator_def> accepteds;
  for (SCM s = accepts_name_list_; gh_pair_p (s); s = ly_cdr (s))
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
      if (gh_equal_p (accepteds[i]->type_name_, type_string))
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
	= g->path_to_acceptable_translator (type_string, odef);
      if (result.size () && result.size () < best_depth)
	{
	  result.insert (g,0);
	  best_result = result;
	}
    }

  return best_result;
}

IMPLEMENT_SMOBS (Translator_def);
IMPLEMENT_DEFAULT_EQUAL_P (Translator_def);


static SCM
trans_list (SCM namelist, Translator_group*tg)
{
  SCM l = SCM_EOL;
  for (SCM s = namelist; gh_pair_p (s) ; s = ly_cdr (s))
    {
      Translator * t = get_translator (ly_scm2string (ly_car (s)));
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
  Translator * g = get_translator (ly_scm2string (translator_group_type_));
  g = g->clone (); 

  Translator_group *tg = dynamic_cast<Translator_group*> (g);
  tg->output_def_ = md;
  tg->definition_ = self_scm ();
  tg->type_string_ = ly_scm2string (type_name_);

  /*
    TODO: ugh. we're reversing CONSISTS_NAME_LIST_ here
   */
  SCM l1 = trans_list (consists_name_list_, tg);
  SCM l2 =trans_list (end_consists_name_list_,tg);
  l1 = scm_reverse_x (l1, l2);
  
  tg->simple_trans_list_ = l1;
  
  return tg;
}


void
Translator_def::apply_property_operations (Translator_group*tg)
{
  SCM correct_order = scm_reverse (property_ops_); // pity of the mem.
  for (SCM s = correct_order; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM entry = ly_car (s);
      SCM type = ly_car (entry);
      entry = ly_cdr (entry); 
      
      if (type == ly_symbol2scm ("push"))
	{
	  SCM val = ly_cddr (entry);
	  val = gh_pair_p (val) ? ly_car (val) : SCM_UNDEFINED;

	  apply_pushpop_property (tg, ly_car (entry), ly_cadr (entry), val);
	}
      else if (type == ly_symbol2scm ("assign"))
	{
	  tg->internal_set_property (ly_car (entry), ly_cadr (entry));
	}
    }
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

void
Translator_def::add_property_assign (SCM nm, SCM val)
{
  this->property_ops_ = gh_cons (scm_list_n (ly_symbol2scm ("assign"), scm_string_to_symbol (nm), val, SCM_UNDEFINED),
				 this->property_ops_);
}

/*
  Default child context as a SCM string, or something else if there is
  none.
*/
SCM
Translator_def::default_child_context_name ()
{
  SCM d = accepts_name_list_;
  return gh_pair_p (d) ? ly_car (scm_last_pair (d)) : SCM_EOL;
}

SCM
Translator_def::to_alist () const
{
  SCM l = SCM_EOL;
  
  l = gh_cons (gh_cons (ly_symbol2scm ("consists"),  consists_name_list_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("end-consists"),  end_consists_name_list_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("accepts"),  accepts_name_list_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("property-ops"),  property_ops_), l);
  l = gh_cons (gh_cons (ly_symbol2scm ("type-name"),  type_name_), l); // junkme.
  l = gh_cons (gh_cons (ly_symbol2scm ("group-type"),  translator_group_type_), l);    

  return l;  
}
