/*
  Translator_group.cc -- implement Translator_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-output-def.hh"
#include "translator-group.hh"
#include "translator.hh"
#include "debug.hh"
#include "moment.hh"
#include "scm-hash.hh"
#include "killing-cons.tcc"
#include "translator-def.hh"

Translator_group::Translator_group (Translator_group const&s)
  : Translator(s)
{
  iterator_count_ =0;
  
  Scheme_hash_table * tab =  new Scheme_hash_table (*s.properties_dict ());
  properties_scm_ = tab->self_scm ();
  scm_unprotect_object (tab->self_scm( ));
}

Scheme_hash_table*
Translator_group::properties_dict () const
{
  return Scheme_hash_table::unsmob (properties_scm_);
}

Translator_group::~Translator_group ()
{
  
  //assert (removable_b());
}


Translator_group::Translator_group()
{
  iterator_count_  = 0;
  Scheme_hash_table *tab = new Scheme_hash_table ;
  properties_scm_ = tab->self_scm ();

  scm_unprotect_object (tab->self_scm ());
}

void
Translator_group::check_removal()
{
  SCM next = SCM_EOL; 
  for (SCM p = trans_group_list_; gh_pair_p (p); p = next)
    {
      next = gh_cdr (p);

      Translator_group *trg =  dynamic_cast<Translator_group*> (unsmob_translator (gh_car (p)));

      trg->check_removal ();
      if (trg->removable_b())
	terminate_translator (trg);
    }
}


SCM
Translator_group::add_translator (SCM list, Translator *t)
{
  list = gh_append2 (list, gh_cons (t->self_scm (), SCM_EOL));
  t->daddy_trans_l_ = this;
  t->output_def_l_ = output_def_l_;
  t->add_processing ();

  return list;
}
void
Translator_group::add_group_translator (Translator *t)
{
  trans_group_list_ = add_translator (trans_group_list_,t);
}



bool
Translator_group::removable_b() const
{
  return trans_group_list_ == SCM_EOL && ! iterator_count_;
}

Translator_group *
Translator_group::find_existing_translator_l (String n, String id)
{
  if (is_alias_b (n) && (id_str_ == id || id.empty_b ()))
    return this;

  Translator_group* r = 0;
  for (SCM p = trans_group_list_; !r && gh_pair_p (p); p = gh_cdr (p))
    {
      Translator *  t = unsmob_translator (gh_car (p));
      
      r = dynamic_cast<Translator_group*> (t)->find_existing_translator_l (n, id);
    }

  return r;
}




Translator_group*
Translator_group::find_create_translator_l (String n, String id)
{
  Translator_group * existing = find_existing_translator_l (n,id);
  if (existing)
    return existing;

  Link_array<Translator_def> path
    = unsmob_translator_def (definition_)->path_to_acceptable_translator (ly_str02scm ((char*)n.ch_C()), output_def_l ());

  if (path.size ())
    {
      Translator_group * current = this;

      // start at 1.  The first one (index 0) will be us.
      for (int i=0; i < path.size (); i++)
	{
	  Translator_group * new_group = path[i]->instantiate (output_def_l_);

	  current->add_group_translator (new_group);
	  current = new_group;
	}
      current->id_str_ = id;
      return current;
    }

  Translator_group *ret = 0;
  if (daddy_trans_l_)
    ret = daddy_trans_l_->find_create_translator_l (n,id);
  else
    {
      warning (_f ("can't find or create `%s' called `%s'", n, id));
      ret =0;
    }
  return ret;
}

bool
Translator_group::try_music_on_nongroup_children (Music *m)
{
  bool hebbes_b =false;

  
  for (SCM p = simple_trans_list_; !hebbes_b && gh_pair_p (p); p = gh_cdr (p))
    {
      hebbes_b = unsmob_translator (gh_car (p))->try_music (m);
    }
  return hebbes_b;
}

bool
Translator_group::do_try_music (Music* m)
{
  bool hebbes_b = try_music_on_nongroup_children (m);
  
  if (!hebbes_b && daddy_trans_l_)
    hebbes_b = daddy_trans_l_->try_music (m);
  return hebbes_b ;
}

int
Translator_group::depth_i() const
{
  return (daddy_trans_l_) ? daddy_trans_l_->depth_i()  + 1 : 0;
}

Translator_group*
Translator_group::ancestor_l (int level)
{
  if (!level || !daddy_trans_l_)
    return this;

  return daddy_trans_l_->ancestor_l (level-1);
}

void
Translator_group::terminate_translator (Translator*r_l)
{
  r_l->removal_processing();
  /*
    Return value ignored. GC does the rest.
   */
  remove_translator_p (r_l);
}


/**
   Remove a translator from the hierarchy.
 */
Translator *
Translator_group::remove_translator_p (Translator*trans_l)
{
  assert (trans_l);

  trans_group_list_ = scm_delq_x (trans_l->self_scm (), trans_group_list_);
  trans_l->daddy_trans_l_ = 0;
  return trans_l;
}

bool
Translator_group::is_bottom_translator_b () const
{
  return !gh_string_p (unsmob_translator_def (definition_)->default_child_context_name ());

}

Translator_group*
Translator_group::get_default_interpreter()
{
  if (!is_bottom_translator_b ())
    {
      SCM nm = unsmob_translator_def (definition_)->default_child_context_name ();
      SCM st = output_def_l ()->find_translator_l (nm);

      Translator_def *t = unsmob_translator_def (st);
      if (!t)
	{
	  warning (_f ("can't find or create: `%s'", ly_scm2string (nm).ch_C()));
	  t = unsmob_translator_def (this->definition_);
	}
      Translator_group *tg = t->instantiate (output_def_l_);
      add_group_translator (tg);

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
  for (SCM p = list; gh_pair_p (p); p = gh_cdr(p))
    (unsmob_translator (gh_car (p))->*method) ();
  
}

void
Translator_group::each (Method_pointer method) 
{
  static_each (simple_trans_list_, method);
  static_each (trans_group_list_, method);
}



void
Translator_group::do_add_processing ()
{
  unsmob_translator_def (definition_)->apply_property_operations (this);
  for (SCM s = simple_trans_list_; gh_pair_p (s) ; s = gh_cdr (s))
    {
      Translator * t = unsmob_translator (gh_car (s));
      t->add_processing ();
    }
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

  return (daddy_trans_l_) ? daddy_trans_l_->where_defined (sym) : 0;
}

SCM
Translator_group::get_property (SCM sym) const
{
  SCM val =SCM_UNDEFINED;
  if (properties_dict ()->try_retrieve (sym, &val))
    return val;

  if (daddy_trans_l_)
    return daddy_trans_l_->get_property (sym);
  
  return val;
}

void
Translator_group::set_property (String id, SCM val)
{
  set_property (ly_symbol2scm (id.ch_C()), val);
}

void
Translator_group::set_property (SCM sym, SCM val)
{
  properties_dict ()->set (sym, val);
}

/*
  Push or pop (depending on value of VAL) a single entry (ELTPROP . VAL)
  entry from a translator property list by name of PROP
*/
void
Translator_group::execute_single_pushpop_property (SCM prop, SCM eltprop, SCM val)
{
  if (gh_symbol_p(prop))
    {
      if (val != SCM_UNDEFINED)
	{
	  SCM prev = get_property (prop);

  	  /*
	    we don't tack onto SCM_UNDEFINED, because it creates
	    errors down the line, if we do scm_assoc().
	   */
	  if (gh_pair_p (prev) || prev == SCM_EOL)
	    {
	      bool ok = true;
	      
	      SCM errport = scm_current_error_port ();
	      
	      SCM meta = scm_assoc (ly_symbol2scm ("meta"), prev);
	      SCM props = scm_assoc (ly_symbol2scm ("properties"), gh_cdr (meta));
	      SCM type_p = scm_assoc (eltprop, gh_cdr (props));
	      if (!gh_pair_p (type_p))
		{
		  scm_puts (_("Couldn't find property description for #'").ch_C(),errport);
		  scm_display (eltprop, errport);

		  scm_puts (_(" in element description ").ch_C(),errport);
		  scm_display (prop, errport);

		  scm_puts (_(". Perhaps you made a typing error?\n").ch_C(),errport);		  
		}
	      else
		{
		  type_p = gh_cdr (type_p);
		  if (gh_call1 (type_p, val) == SCM_BOOL_F)
		    {
		      ok = false;
		      scm_puts (_("Failed typecheck for #'").ch_C (),errport);
		      scm_display (eltprop,errport);
		      scm_puts ( _(", value ").ch_C (), errport);
		      scm_write (val, errport);
		      scm_puts (_(" must be of type ").ch_C (), errport);
		      SCM typefunc = scm_eval2 (ly_symbol2scm ("type-name"), SCM_EOL);
		      scm_display (gh_call1 (typefunc, type_p), errport);
		      scm_puts ("\n", errport);		      
		    }
		}

	      if (ok)
		{
		  prev = gh_cons (gh_cons (eltprop, val), prev);
		  set_property (prop, prev);
		}
	    }
	  else
	    {
	      // warning here.
	    }
	  
	}
      else
	{
	  SCM prev = get_property (prop);

	  SCM newprops= SCM_EOL ;
	  while (gh_pair_p (prev) && gh_caar (prev) != eltprop)
	    {
	      newprops = gh_cons (gh_car (prev), newprops);
	      prev = gh_cdr (prev);
	    }
	  
	  if (gh_pair_p (prev))
	    {
	      newprops = scm_reverse_x (newprops, gh_cdr (prev));
	      set_property (prop, newprops);
	    }
	}
    }
}





/*
  STUBS
*/
void
Translator_group::do_pre_move_processing ()
{
  each (&Translator::pre_move_processing);
}

void
Translator_group::do_post_move_processing ()
{
  each (&Translator::post_move_processing);
}

void
Translator_group::do_process_music ()
{
  each (&Translator::process_music);
}

void
Translator_group::do_creation_processing ()
{
  each (&Translator::creation_processing);
}

void
Translator_group::do_removal_processing ()
{
  each (&Translator::removal_processing);
}
