#include "translator-group.hh"
#include "context-def.hh"
#include "warn.hh"
#include "music-output-def.hh"
#include "scm-hash.hh"
#include "main.hh"

bool
Translator_group::is_removable () const
{
  return trans_group_list_ == SCM_EOL && ! iterator_count_;
}

Translator_group *
Translator_group::find_existing_translator (SCM n, String id)
{
  if ((is_alias (n) && (id_string_ == id || id.is_empty ())) || n == ly_symbol2scm ("Current"))
    return this;

  Translator_group* r = 0;
  for (SCM p = trans_group_list_; !r && gh_pair_p (p); p = ly_cdr (p))
    {
      Translator *  t = unsmob_translator (ly_car (p));
      
      r = dynamic_cast<Translator_group*> (t)->find_existing_translator (n, id);
    }

  return r;
}


Translator_group*
Translator_group::find_create_translator (SCM n, String id, SCM operations)
{
  Translator_group * existing = find_existing_translator (n,id);
  if (existing)
    return existing;

  if (n == ly_symbol2scm ("Bottom"))
    {
      Translator_group* tg = get_default_interpreter ();
      tg->id_string_ = id;
      return tg;
    }

  /*
    TODO: use accepts_list_.
   */
  Link_array<Context_def> path
    = unsmob_context_def (definition_)->path_to_acceptable_translator (n, get_output_def ());

  if (path.size ())
    {
      Translator_group * current = this;

      // start at 1.  The first one (index 0) will be us.
      for (int i=0; i < path.size (); i++)
	{
	  SCM ops = (i == path.size () -1) ? operations : SCM_EOL;

	  Translator_group * new_group
	    = path[i]->instantiate (ops);

	  if (i == path.size () -1)
	    {
	      new_group->id_string_ = id;
	    }

	  current->add_fresh_group_translator (new_group);
	  apply_property_operations (new_group, ops);
	  
	  current = new_group;
	}

      return current;
    }

  Translator_group *ret = 0;
  if (daddy_trans_)
    ret = daddy_trans_->find_create_translator (n, id, operations);
  else
    {
      warning (_f ("Cannot find or create `%s' called `%s'",
		   ly_symbol2string (n).to_str0 (), id));
      ret =0;
    }
  return ret;
}

/*
  Default child context as a SCM string, or something else if there is
  none.
*/
SCM
default_child_context_name (Translator_group const *tg)
{
  return gh_pair_p (tg->accepts_list_)
    ? ly_car (scm_last_pair (tg->accepts_list_))
    : SCM_EOL;
}


bool
Translator_group::is_bottom_context () const
{
  return !gh_symbol_p (default_child_context_name (this));
}

Translator_group*
Translator_group::get_default_interpreter ()
{
  if (!is_bottom_context ())
    {
      SCM nm = default_child_context_name (this);
      SCM st = get_output_def ()->find_translator (nm);

      Context_def *t = unsmob_context_def (st);
      if (!t)
	{
	  warning (_f ("can't find or create: `%s'", ly_symbol2string (nm).to_str0 ()));
	  t = unsmob_context_def (this->definition_);
	}
      Translator_group *tg = t->instantiate (SCM_EOL);
      add_fresh_group_translator (tg);
      if (!tg->is_bottom_context ())
	return tg->get_default_interpreter ();
      else
	return tg;
    }
  return this;
}

/*
  PROPERTIES
 */
Translator_group*
Translator_group::where_defined (SCM sym) const
{
  if (properties_dict ()->contains (sym))
    {
      return (Translator_group*)this;
    }

  return (daddy_trans_) ? daddy_trans_->where_defined (sym) : 0;
}

/*
  return SCM_EOL when not found.
*/
SCM
Translator_group::internal_get_property (SCM sym) const
{
  SCM val =SCM_EOL;
  if (properties_dict ()->try_retrieve (sym, &val))
    return val;

  if (daddy_trans_)
    return daddy_trans_->internal_get_property (sym);
  
  return val;
}

bool
Translator_group::is_alias (SCM sym) const
{
  if (sym == ly_symbol2scm ("Bottom")
      && !gh_pair_p (accepts_list_))
    return true;
  return unsmob_context_def (definition_)->is_alias (sym);
}

void
Translator_group::internal_set_property (SCM sym, SCM val)
{
#ifndef NDEBUG
  if (internal_type_checking_global_b)
    assert (type_check_assignment (sym, val, ly_symbol2scm ("translation-type?")));
#endif
  
  properties_dict ()->set (sym, val);
}

/*
  TODO: look up to check whether we have inherited var? 
 */
void
Translator_group::unset_property (SCM sym)
{
  properties_dict ()->remove (sym);
}

