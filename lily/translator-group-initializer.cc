	/*   
  translator-group-initializer.cc --  implement Translator_group_initializer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "translator-group.hh"
#include "warn.hh"


void
Translator_group_initializer::set_acceptor (Translator *me,SCM name, bool add)
{
  if (add)
    me->accepts_name_list_ = gh_append2 (me->accepts_name_list_, gh_cons (name, SCM_EOL));
  else
    me->accepts_name_list_ = scm_delete_x (name, me->accepts_name_list_);
}


SCM
Translator_group_initializer::modify_definition (SCM list, SCM str, bool add)
{
  String s = ly_scm2string (str);
  if (!get_translator_l (s))
    error (_ ("Program has no such type"));

  if (add)
    {
      if (scm_memq (str, list) != SCM_BOOL_F)
	{
	  warning (_f("Already contains: `%s'", s));
	  warning (_f("Not adding translator: `%s'", s));
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
Translator_group_initializer::remove_element (Translator *me,SCM s)
{
  me->end_consists_name_list_ = modify_definition (me->end_consists_name_list_, s, false);
  me->consists_name_list_ = modify_definition (me->consists_name_list_, s, false);
}

void
Translator_group_initializer::add_element (Translator *me,SCM s)
{
  me->consists_name_list_ = modify_definition (me->consists_name_list_, s, true);
}

void
Translator_group_initializer::add_last_element (Translator *me,SCM s)
{
  me->end_consists_name_list_ = modify_definition (me->end_consists_name_list_, s, true);
}
void
Translator_group_initializer::add_push_property (Translator * me,
						 SCM props, SCM syms,  SCM vals)
{
  me->property_pushes_ = gh_cons (gh_list (props, syms, vals, SCM_UNDEFINED),
				  me->property_pushes_);
}

void
Translator_group_initializer::add_pop_property (Translator * me,
						 SCM props, SCM syms)
{
  me->property_pushes_ = gh_cons (gh_list (props, syms, SCM_UNDEFINED),
				  me->property_pushes_);
}

/*
  Do it. SYMS maybe a symbol or a list of symbols. VAL is
  SCM_UNDEFINED in case of a pop
*/
void
Translator_group_initializer::apply_pushpop_property (Translator *trans, SCM syms, SCM eprop, SCM val)
{
  if (gh_symbol_p (syms))
    dynamic_cast<Translator_group*>(trans)->execute_single_pushpop_property (syms, eprop, val);
  else for (SCM s = syms; gh_pair_p (s); s = gh_cdr (s))
    dynamic_cast<Translator_group*>(trans)->execute_single_pushpop_property (gh_car (s), eprop, val);
}
