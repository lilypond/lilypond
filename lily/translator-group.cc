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
  consists_str_arr_ = s.consists_str_arr_;
  consists_end_str_arr_ = s.consists_end_str_arr_;
  accepts_str_arr_ = s.accepts_str_arr_;
  iterator_count_ =0;
  properties_dict_ = new Scheme_hash_table (*s.properties_dict_);
}

Translator_group::~Translator_group ()
{
  assert (removable_b());
  trans_p_list_.junk ();
  delete properties_dict_;
}


Translator_group::Translator_group()
{
  iterator_count_  = 0;
  properties_dict_ = new Scheme_hash_table ;
}

void
Translator_group::check_removal()
{
  Cons<Translator> *next =0;
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = next)
    {
      next = p->next_;
      if (Translator_group *trg =  dynamic_cast <Translator_group *> (p->car_))
	{
	  trg->check_removal ();
	  if (trg->removable_b())
	    terminate_translator (trg);
	}
    }
}

void
Translator_group::add_translator (Translator *trans_p)
{
  trans_p_list_.append (new Killing_cons<Translator> (trans_p,0));

  trans_p->daddy_trans_l_ = this;
  trans_p->output_def_l_ = output_def_l_;
  trans_p->add_processing ();
}

void
Translator_group::set_acceptor (String accepts, bool add)
{
  if (add)
    accepts_str_arr_.push (accepts);
  else
    for (int i=accepts_str_arr_.size (); i--; )
      if (accepts_str_arr_[i] == accepts)
	accepts_str_arr_.del (i);
}

void
Translator_group::add_last_element (String s)
{
  if (!get_translator_l (s))
    error (_ ("Program has no such type"));

  for (int i=consists_end_str_arr_.size (); i--; )
    if (consists_end_str_arr_[i] == s)
      warning (_f ("Already contains: `%s'", s));
      
  consists_end_str_arr_.push (s);
}

void
Translator_group::set_element (String s, bool add)
{
  if (!get_translator_l (s))
    error (_ ("Program has no such type"));

  if (add)
    {
      for (int i=consists_str_arr_.size (); i--; )
	if (consists_str_arr_[i] == s)
	  warning (_f("Already contains: `%s'", s));
      
      consists_str_arr_.push (s);
    }
  else
    {
      for (int i=consists_str_arr_.size (); i--; )
	if (consists_str_arr_[i] == s)
	  consists_str_arr_.del (i);
      for (int i=consists_end_str_arr_.size (); i--; )
	if (consists_end_str_arr_[i] == s)
	  consists_end_str_arr_.del (i);
    }
}
bool
Translator_group::removable_b() const
{
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    {
      if (dynamic_cast <Translator_group *> (p->car_))
	return false;
    }

  return !iterator_count_;
}

Translator_group *
Translator_group::find_existing_translator_l (String n, String id)
{
  if (is_alias_b (n) && (id_str_ == id || id.empty_b ()))
    return this;

  Translator_group* r = 0;
  for (Cons<Translator> *p = trans_p_list_.head_; !r && p; p = p->next_)
    {
      if (Translator_group *trg =  dynamic_cast <Translator_group *> (p->car_))
	r = trg->find_existing_translator_l (n, id);
    }

  return r;
}

Link_array<Translator_group>
Translator_group::path_to_acceptable_translator (String type, Music_output_def* odef) const
{
 Link_array<Translator_group> accepted_arr;
  for (int i=0; i < accepts_str_arr_.size (); i++)
    {
      Translator *t = odef->find_translator_l (accepts_str_arr_[i]);
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

	  current->add_translator (new_group);
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

  for (Cons<Translator> *p = trans_p_list_.head_; !hebbes_b && p; p = p->next_)
    {
      if (!dynamic_cast <Translator_group *> (p->car_))
	{
	  hebbes_b = p->car_->try_music (m);
	}
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

  delete trans_p;
}


/**
   Remove a translator from the hierarchy.
 */
Translator *
Translator_group::remove_translator_p (Translator*trans_l)
{
  assert (trans_l);
  
  for (Cons<Translator> **pp = &trans_p_list_.head_; *pp; pp = &(*pp)->next_)
    if ((*pp)->car_ == trans_l)
      {
	Cons<Translator> *r = trans_p_list_.remove_cons (pp);
	r->car_ =0;
	trans_l->daddy_trans_l_ =0;
	delete r;
	return trans_l;
      }

  return 0;
}


Translator*
Translator_group::get_simple_translator (String type) const
{
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    {
      if (classname (p->car_) == type)
	return p->car_;
    }
  if (daddy_trans_l_)
    return daddy_trans_l_->get_simple_translator (type);
  return 0;
}


bool
Translator_group::is_bottom_translator_b () const
{
  return !accepts_str_arr_.size ();
}



Translator_group*
Translator_group::get_default_interpreter()
{
  if (accepts_str_arr_.size())
    {
      Translator*t = output_def_l ()->find_translator_l (accepts_str_arr_[0]);
      if (!t)
	{
	  warning (_f ("can't find or create: `%s'", accepts_str_arr_[0]));
	  t = this;
	}
      Translator_group * g= dynamic_cast <Translator_group*>(t->clone ());
      add_translator (g);

      if (!g->is_bottom_translator_b ())
	return g->get_default_interpreter ();
      else
	return g;
    }
  return this;
}

void
Translator_group::each (Method_pointer method)
{
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    (p->car_->*method) ();
}


void
Translator_group::each (Const_method_pointer method) const
{
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    (p->car_->*method) ();
}

void
Translator_group::do_print() const
{
#ifndef NPRINT
  if (!flower_dstream)
    return ;

  gh_display (properties_dict_->self_scm_);
  if (status == ORPHAN)
    {
      DEBUG_OUT << "consists of: ";
      for (int i=0; i < consists_str_arr_.size (); i++)
	DEBUG_OUT << consists_str_arr_[i] << ", ";
      DEBUG_OUT << "\naccepts: ";
      for (int i=0; i < accepts_str_arr_.size (); i++)
	DEBUG_OUT << accepts_str_arr_[i] << ", ";
    }
  else
    {
      if (id_str_.length_i ())
	DEBUG_OUT << "ID: " << id_str_ ;
      DEBUG_OUT << " iterators: " << iterator_count_<< '\n';
    }
  each (&Translator::print);
#endif
}

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

void
Translator_group::do_add_processing ()
{
   for (int i=0; i < consists_str_arr_.size(); i++)
    {
      String s = consists_str_arr_[i];
      Translator * t = output_def_l ()->find_translator_l (s);
      if (!t)
	warning (_f ("can't find: `%s'", s));
      else
	add_translator (t->clone ());
    }
   for (int i=0; i-- < consists_end_str_arr_.size (); i++)
     {
       String s = consists_end_str_arr_[i];
       Translator * t = output_def_l ()->find_translator_l (s);
       if (!t)
	 warning (_f ("can't find: `%s'", s));
       else
	 add_translator (t->clone ());
    }
}

Translator_group*
Translator_group::where_defined (SCM sym) const
{
  if (properties_dict_->elem_b (sym))
    {
      return (Translator_group*)this;
    }

  return (daddy_trans_l_) ? daddy_trans_l_->where_defined (sym) : 0;
}

SCM
Translator_group::get_property (SCM sym) const
{
  if (properties_dict_->elem_b (sym))
    {
      return properties_dict_->get (sym);
    }

  if (daddy_trans_l_)
    return daddy_trans_l_->get_property (sym);
  
  return SCM_UNDEFINED;
}

void
Translator_group::set_property (String id, SCM val)
{
  properties_dict_->set (ly_symbol2scm (id.ch_C()), val);
}

