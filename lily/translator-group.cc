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
  assert (removable_b());
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

Link_array<Translator_group>
Translator_group::path_to_acceptable_translator (String type, Music_output_def* odef) const
{
  Link_array<Translator_group> accepted_arr;
  for (SCM s = accepts_name_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      
      Translator *t = odef->find_translator_l (ly_scm2string (gh_car (s)));
      if (!t || !dynamic_cast <Translator_group *> (t))
	continue;
      accepted_arr.push (dynamic_cast <Translator_group *> (t));
    }


 for (int i=0; i < accepted_arr.size (); i++)
    if (accepted_arr[i]->type_str_ == type)
      {
	Link_array<Translator_group> retval;
	retval.push (accepted_arr[i]);
	return retval;
      }

  Link_array<Translator_group> best_result;
  int best_depth= INT_MAX;
  for (int i=0; i < accepted_arr.size (); i++)
    {
      Translator_group * g = accepted_arr[i];

      Link_array<Translator_group> result
	= g->path_to_acceptable_translator (type, odef);
      if (result.size () && result.size () < best_depth)
	{
	  result.insert (g,0);
	  best_result = result;
	}
    }

  return best_result;
}

Translator_group*
Translator_group::find_create_translator_l (String n, String id)
{
  Translator_group * existing = find_existing_translator_l (n,id);
  if (existing)
    return existing;

  Link_array<Translator_group> path
    = path_to_acceptable_translator (n, output_def_l ());

  if (path.size ())
    {
      Translator_group * current = this;

      // start at 1.  The first one (index 0) will be us.
      for (int i=0; i < path.size (); i++)
	{
	  Translator_group * new_group = dynamic_cast<Translator_group*>(path[i]->clone ());

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
  Translator * trans_p =remove_translator_p (r_l);
  /*
    forget trans_p, GC does the rest.
   */
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

#if 0
/*
  should not use, instead: use properties to communicate between engravers.
 */
Translator*
Translator_group::get_simple_translator (String type) const
{
  for (SCM p = simple_trans_list_;  gh_pair_p (p); p =gh_cdr (p))
    {
      if (classname (unsmob_translator (gh_car (p))) == type)
	return unsmob_translator (gh_car (p));
    }
  if (daddy_trans_l_)
    return daddy_trans_l_->get_simple_translator (type);
  return 0;
}
#endif 

bool
Translator_group::is_bottom_translator_b () const
{
  return accepts_name_list_ == SCM_EOL;
}



Translator_group*
Translator_group::get_default_interpreter()
{
  if (gh_pair_p (accepts_name_list_))
    {
      String str = ly_scm2string (gh_car (accepts_name_list_));
      Translator*t = output_def_l ()->find_translator_l (str);
      if (!t)
	{
	  warning (_f ("can't find or create: `%s'", str));
	  t = this;
	}
      Translator_group * g= dynamic_cast <Translator_group*>(t->clone ());
      add_group_translator (g);

      if (!g->is_bottom_translator_b ())
	return g->get_default_interpreter ();
      else
	return g;
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
Translator_group::do_print() const
{
#ifndef NPRINT
#endif
}

static SCM
trans_list (SCM namelist, Music_output_def *mdef)
{
  SCM l = SCM_EOL;
  for (SCM s = namelist; gh_pair_p (s) ; s = gh_cdr (s))
    {
      Translator * t = mdef->find_translator_l (ly_scm2string (gh_car (s)));
      if (!t)
	warning (_f ("can't find: `%s'", s));
      else
	{
	  Translator * tr = t->clone ();
	  SCM str = tr->self_scm ();
	  l = gh_cons (str, l);
	  scm_unprotect_object (str);
	}
    }
  return l; 
}


void
Translator_group::do_add_processing ()
{
  assert (simple_trans_list_== SCM_EOL);

  SCM correct_order = scm_reverse (property_pushes_); // pity of the mem.
  for (SCM s = correct_order; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM entry = gh_car (s);
      SCM val = gh_cddr (entry);
      val = gh_pair_p (val) ? gh_car (val) : SCM_UNDEFINED;
      
      Translator_group_initializer::apply_pushpop_property (this, gh_car (entry),
							    gh_cadr (entry),
							    val);
    }

  SCM l1 = trans_list (consists_name_list_, output_def_l ());
  SCM l2 =trans_list (end_consists_name_list_, output_def_l ());
  l1 = scm_reverse_x (l1, l2);
  
  simple_trans_list_ = l1;
  for (SCM s = l1; gh_pair_p (s) ; s = gh_cdr (s))
    {
      Translator * t = unsmob_translator (gh_car (s));

      t->daddy_trans_l_ = this;
      t->output_def_l_ = output_def_l_;
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
  if (properties_dict ()->elem_b (sym))
    {
      return properties_dict ()->get (sym);
    }

  if (daddy_trans_l_)
    return daddy_trans_l_->get_property (sym);
  
  return SCM_UNDEFINED;
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

	  prev = gh_cons (gh_cons (eltprop, val), prev);
	  set_property (prop, prev);
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
