/*
  Translator_group.cc -- implement Translator_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-output-def.hh"
#include "translator-group.hh"
#include "translator.hh"
#include "debug.hh"
#include "rational.hh"
#include "dictionary-iter.hh"

#include "killing-cons.tcc"

Translator_group::Translator_group (Translator_group const&s)
  : Translator(s)
{
  consists_str_arr_ = s.consists_str_arr_;
  accepts_str_arr_ = s.accepts_str_arr_;
  iterator_count_ =0;
  properties_dict_ = s.properties_dict_;
}

Translator_group::~Translator_group ()
{
  assert (removable_b());
  trans_p_list_.junk ();
}


Translator_group::Translator_group()
{
  iterator_count_  = 0;
}

void
Translator_group::check_removal()
{
  Link_array<Translator_group> groups (group_l_arr ());
  
  for (int i =0; i < groups.size(); i++)
    {
      groups[i]->check_removal();
      if (groups[i]->removable_b())
	terminate_translator (groups[i]);
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
Translator_group::set_element (String s, bool add)
{
  if (!get_translator_l (s))
    error ("Program has no such type");

  if (add)
    consists_str_arr_.push (s);
  else
    for (int i=consists_str_arr_.size (); i--; )
      if (consists_str_arr_[i] == s)
	consists_str_arr_.del (i);
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

  Link_array<Translator_group> groups (group_l_arr ());
  Translator_group* r = 0;
  for (int i =0; !r && i < groups.size(); i++)
    {
      r = groups[i]->find_existing_translator_l (n,id);
    }

  return r;
}

Link_array<Translator_group>
Translator_group::path_to_acceptable_translator (String type) const
{
 Link_array<Translator_group> accepted_arr;
  for (int i=0; i < accepts_str_arr_.size (); i++)
    {
      Translator *t = output_def_l ()->find_translator_l (accepts_str_arr_[i]);
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
	= g->path_to_acceptable_translator (type);
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

  Link_array<Translator_group> path = path_to_acceptable_translator (n);

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
      warning (_f ("can't find or create `%s\' called `%s\'", n, id));
      ret =0;
    }
  return ret;
}


bool
Translator_group::do_try_music (Music* req_l)
{
  bool hebbes_b =false;

  Link_array<Translator> nongroups (nongroup_l_arr ());
  
  for (int i =0; !hebbes_b && i < nongroups.size() ; i++)
    hebbes_b =nongroups[i]->try_music (req_l);
  if (!hebbes_b && daddy_trans_l_)
    hebbes_b = daddy_trans_l_->try_music (req_l);
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

Link_array<Translator_group>
Translator_group::group_l_arr () const
{
  Link_array<Translator_group> groups;
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    {
      if (dynamic_cast <Translator_group *> (p->car_))
	groups.push (dynamic_cast <Translator_group *> (p->car_));
    }
  return groups;
}

Link_array<Translator>
Translator_group::nongroup_l_arr () const
{
  Link_array<Translator> groups;
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    {
      if (!dynamic_cast <Translator_group *> (p->car_))
	groups.push (p->car_);
    }
  return groups;
}
/**
   End translator: call "destructor", remove from hierarchy, and delete
 */

void
Translator_group::terminate_translator (Translator*r_l)
{
  DOUT << "Removing " << classname (r_l) << " at " << now_mom () << '\n';
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
  Link_array<Translator> nongroups (nongroup_l_arr ());
  for (int i=0; i < nongroups.size(); i++)
    {
      if (classname (nongroups[i]) == type)
	return nongroups[i];
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
	  warning (_f ("can't find or create `%s\'", accepts_str_arr_[0]));
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
  if (!check_debug)
    return ;
  for (Dictionary_iter<Scalar> i (properties_dict_); i.ok (); i++)
    {
      DOUT << i.key () << "=" << i.val () << '\n';
    }
  if (status == ORPHAN)
    {
      DOUT << "consists of: ";
      for (int i=0; i < consists_str_arr_.size (); i++)
	DOUT << consists_str_arr_[i] << ", ";
      DOUT << "\naccepts: ";
      for (int i=0; i < accepts_str_arr_.size (); i++)
	DOUT << accepts_str_arr_[i] << ", ";
    }
  else
    {
      if (id_str_.length_i ())
	DOUT << "ID: " << id_str_ ;
      DOUT << " iterators: " << iterator_count_<< '\n';
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
Translator_group::do_process_requests ()
{
  each (&Translator::process_requests);
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
      Translator * t = output_def_l ()->find_translator_l (consists_str_arr_[i]);
      if (!t)
	warning (_f ("can't find `%s\'", consists_str_arr_[i]));
      else
	add_translator (t->clone ());
    }
}

Scalar
Translator_group::get_property (String id,
				Translator_group **where_l) const
{
  if (properties_dict_.elem_b (id))
    {
      if (where_l)
	*where_l = (Translator_group*) this; // ugh
      return properties_dict_[id];
    }
  
  if (daddy_trans_l_)
    return daddy_trans_l_->get_property (id, where_l);

  if (where_l)
    *where_l = 0;
  return "";
}

void
Translator_group::set_property (String id, Scalar val)
{
  properties_dict_[id] = val;
}
