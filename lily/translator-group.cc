/*
  Translator_group.cc -- implement Translator_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-output-def.hh"
#include "translator-group.hh"
#include "translator.hh"
#include "warn.hh"
#include "moment.hh"
#include "scm-hash.hh"
#include "translator-def.hh"
#include "main.hh"
#include "music.hh"

Translator_group::Translator_group (Translator_group const&s)
  : Translator (s)
{
  iterator_count_ =0;
  
  Scheme_hash_table * tab =  new Scheme_hash_table (*s.properties_dict ());
  properties_scm_ = tab->self_scm ();
  scm_gc_unprotect_object (tab->self_scm ());
}

Scheme_hash_table*
Translator_group::properties_dict () const
{
  return Scheme_hash_table::unsmob (properties_scm_);
}

Translator_group::~Translator_group ()
{
  
  //assert (removable_b ());
}


Translator_group::Translator_group ()
{
  iterator_count_  = 0;
  Scheme_hash_table *tab = new Scheme_hash_table ;
  properties_scm_ = tab->self_scm ();

  scm_gc_unprotect_object (tab->self_scm ());
}

void
Translator_group::check_removal ()
{
  SCM next = SCM_EOL; 
  for (SCM p = trans_group_list_; gh_pair_p (p); p = next)
    {
      next = ly_cdr (p);

      Translator_group *trg =  dynamic_cast<Translator_group*> (unsmob_translator (ly_car (p)));

      trg->check_removal ();
      if (trg->removable_b ())
	terminate_translator (trg);
    }
}

SCM
Translator_group::add_translator (SCM list, Translator *t)
{
  /*
    Must append, since list ordering must be preserved.
   */
  list = gh_append2 (list, gh_cons (t->self_scm (), SCM_EOL));
  t->daddy_trans_ = this;
  t->output_def_ = output_def_;

  return list;
}


void
Translator_group::add_used_group_translator (Translator *t)
{
  trans_group_list_ = add_translator (trans_group_list_,t);
}


void
Translator_group::add_fresh_group_translator (Translator*t)
{
  Translator_group*tg = dynamic_cast<Translator_group*> (t);
  assert (tg);

  trans_group_list_ = add_translator (trans_group_list_,t); 
  unsmob_translator_def (tg->definition_)->apply_property_operations (tg);
  t->initialize ();
  
}


bool
Translator_group::removable_b () const
{
  return trans_group_list_ == SCM_EOL && ! iterator_count_;
}

Translator_group *
Translator_group::find_existing_translator (String n, String id)
{
  if ((is_alias_b (n) && (id_string_ == id || id.empty_b ())) || n == "Current")
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
Translator_group::find_create_translator (String n, String id)
{
  Translator_group * existing = find_existing_translator (n,id);
  if (existing)
    return existing;

  Link_array<Translator_def> path
    = unsmob_translator_def (definition_)->path_to_acceptable_translator (scm_makfrom0str ((char*)n.to_str0 ()), get_output_def ());

  if (path.size ())
    {
      Translator_group * current = this;

      // start at 1.  The first one (index 0) will be us.
      for (int i=0; i < path.size (); i++)
	{
	  Translator_group * new_group = path[i]->instantiate (output_def_);

	  if (i == path.size () -1)
	    new_group->id_string_ = id;	  
	  current->add_fresh_group_translator (new_group);
	  current = new_group;
	}

      return current;
    }

  Translator_group *ret = 0;
  if (daddy_trans_)
    ret = daddy_trans_->find_create_translator (n,id);
  else
    {
      warning (_f ("can't find or create `%s' called `%s'", n, id));
      ret =0;
    }
  return ret;
}

bool
Translator_group::try_music (Music* m)
{
  bool hebbes_b = try_music_on_nongroup_children (m);
  
  if (!hebbes_b && daddy_trans_)
    hebbes_b = daddy_trans_->try_music (m);
  return hebbes_b ;
}

int
Translator_group::get_depth () const
{
  return (daddy_trans_) ? daddy_trans_->get_depth ()  + 1 : 0;
}

Translator_group*
Translator_group::get_ancestor (int level)
{
  if (!level || !daddy_trans_)
    return this;

  return daddy_trans_->get_ancestor (level-1);
}

void
Translator_group::terminate_translator (Translator*r)
{
  r->removal_processing ();
  /*
    Return value ignored. GC does the rest.
   */
  remove_translator (r);
}


/**
   Remove a translator from the hierarchy.
 */
Translator *
Translator_group::remove_translator (Translator*trans)
{
  assert (trans);

  trans_group_list_ = scm_delq_x (trans->self_scm (), trans_group_list_);
  trans->daddy_trans_ = 0;
  return trans;
}

bool
Translator_group::is_bottom_translator_b () const
{
  return !gh_string_p (unsmob_translator_def (definition_)->default_child_context_name ());

}

Translator_group*
Translator_group::get_default_interpreter ()
{
  if (!is_bottom_translator_b ())
    {
      SCM nm = unsmob_translator_def (definition_)->default_child_context_name ();
      SCM st = get_output_def ()->find_translator (nm);

      Translator_def *t = unsmob_translator_def (st);
      if (!t)
	{
	  warning (_f ("can't find or create: `%s'", ly_scm2string (nm).to_str0 ()));
	  t = unsmob_translator_def (this->definition_);
	}
      Translator_group *tg = t->instantiate (output_def_);
      add_fresh_group_translator (tg);

      if (!tg->is_bottom_translator_b ())
	return tg->get_default_interpreter ();
      else
	return tg;
    }
  return this;
}

static void
static_each (SCM list, Method_pointer method)
{
  for (SCM p = list; gh_pair_p (p); p = ly_cdr (p))
    (unsmob_translator (ly_car (p))->*method) ();
  
}

void
Translator_group::each (Method_pointer method) 
{
  static_each (simple_trans_list_, method);
  static_each (trans_group_list_, method);
}


/*
  PROPERTIES
 */
Translator_group*
Translator_group::where_defined (SCM sym) const
{
  if (properties_dict ()->elem_b (sym))
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


/*
  Push or pop (depending on value of VAL) a single entry (ELTPROP . VAL)
  entry from a translator property list by name of PROP
*/
void
Translator_group::execute_single_pushpop_property (SCM prop, SCM eltprop, SCM val)
{
  if (gh_symbol_p (prop))
    {
      if (val != SCM_UNDEFINED)
	{
	  SCM prev = internal_get_property (prop);

	  if (gh_pair_p (prev) || prev == SCM_EOL)
	    {
	      bool ok = type_check_assignment (eltprop, val, ly_symbol2scm ("backend-type?"));
	      
	      if (ok)
		{
		  prev = gh_cons (gh_cons (eltprop, val), prev);
		  internal_set_property (prop, prev);
		}
	    }
	  else
	    {
	      // warning here.
	    }
	  
	}
      else
	{
	  SCM prev = internal_get_property (prop);

	  SCM newprops= SCM_EOL ;
	  while (gh_pair_p (prev) && ly_caar (prev) != eltprop)
	    {
	      newprops = gh_cons (ly_car (prev), newprops);
	      prev = ly_cdr (prev);
	    }
	  
	  if (gh_pair_p (prev))
	    {
	      newprops = scm_reverse_x (newprops, ly_cdr (prev));
	      internal_set_property (prop, newprops);
	    }
	}
    }
}





/*
  STUBS
*/
void
Translator_group::stop_translation_timestep ()
{
  each (&Translator::stop_translation_timestep);
}

void
Translator_group::start_translation_timestep ()
{
  each (&Translator::start_translation_timestep);
}

void
Translator_group::do_announces ()
{
  each (&Translator::do_announces);
}

void
Translator_group::initialize ()
{
  SCM tab = scm_make_vector (gh_int2scm (19), SCM_BOOL_F);
  set_property ("acceptHashTable", tab);
  each (&Translator::initialize);
}

void
Translator_group::finalize ()
{
  each (&Translator::removal_processing);
}



bool translator_accepts_any_of (Translator*tr, SCM ifaces)
{
  SCM ack_ifs = scm_assoc (ly_symbol2scm ("events-accepted"),
			   tr->translator_description());
  ack_ifs = gh_cdr (ack_ifs);
  for (SCM s = ifaces; ly_pair_p (s); s = ly_cdr (s))
    if (scm_memq (ly_car (s), ack_ifs) != SCM_BOOL_F)
      return true;
  return false;
}

SCM
find_accept_translators (SCM gravlist, SCM ifaces)
{
  SCM l = SCM_EOL;
  for (SCM s = gravlist; ly_pair_p (s);  s = ly_cdr (s))
    {
      Translator* tr = unsmob_translator (ly_car (s));
      if (translator_accepts_any_of (tr, ifaces))
	l = scm_cons (tr->self_scm (), l); 
    }
  l = scm_reverse_x (l, SCM_EOL);

  return l;
}

bool
Translator_group::try_music_on_nongroup_children (Music *m )
{
  SCM tab = get_property ("acceptHashTable");
  SCM name = scm_assq ( ly_symbol2scm ("name"), m->get_property_alist (false));

  if (!gh_pair_p (name))
    return false;

  name = gh_car (name);
  SCM accept_list = scm_hashq_ref (tab, name, SCM_UNDEFINED);
  if (accept_list == SCM_BOOL_F)
    {
      accept_list = find_accept_translators (simple_trans_list_,
					     m->get_mus_property ("types"));
      scm_hashq_set_x (tab, name, accept_list);
    }

  for (SCM p = accept_list; gh_pair_p (p); p = ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      if (t && t->try_music (m))
	return true;
    }
  return false;
}
