/*
  Translator_group.cc -- implement Translator_group

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-output-def.hh"
#include "translator-group.hh"
#include "translator.hh"
#include "debug.hh"
#include "pcursor.hh"
#include "rational.hh"

Translator_group::Translator_group (Translator_group const&s)
  : Translator(s)
{
  consists_str_arr_ = s.consists_str_arr_;
  accepts_str_arr_ = s.accepts_str_arr_;
  iterator_count_ =0;
}

Translator_group::~Translator_group ()
{
  assert (removable_b());
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



IMPLEMENT_IS_TYPE_B1(Translator_group, Translator);

void
Translator_group::add_translator (Translator *trans_p)
{
  trans_p_list_.bottom().add (trans_p);
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
  for (PCursor<Translator*> i (trans_p_list_.top ()); i.ok (); i++)
    {
      if (i->access_Translator_group ())
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
      if (!t || !t->access_Translator_group ())
	continue;
      accepted_arr.push (t->access_Translator_group ());
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
	  Translator_group * new_group = path[i]->clone ()->access_Translator_group ();
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
Translator_group::do_try_request (Request* req_l)
{
  bool hebbes_b =false;

  Link_array<Translator> nongroups (nongroup_l_arr ());
  
  for (int i =0; !hebbes_b && i < nongroups.size() ; i++)
    hebbes_b =nongroups[i]->try_request (req_l);
  if (!hebbes_b && daddy_trans_l_)
    hebbes_b = daddy_trans_l_->try_request (req_l);
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
  for (PCursor<Translator*> i (trans_p_list_.top ()); i.ok (); i++)
    {
      if (i->access_Translator_group ())
	groups.push (i->access_Translator_group ());
    }
  return groups;
}

Link_array<Translator>
Translator_group::nongroup_l_arr () const
{
  Link_array<Translator> groups;
  for (PCursor<Translator*> i (trans_p_list_.top ()); i.ok (); i++)
    {
      if (!i->access_Translator_group ())
	groups.push (i.ptr ());
    }
  return groups;
}

void
Translator_group::terminate_translator (Translator*r_l)
{
  DOUT << "Removing " << r_l->name() << " at " << now_moment () << '\n';
  r_l->removal_processing();
  Translator * trans_p =remove_translator_p (r_l);

  delete trans_p;
}

Translator *
Translator_group::remove_translator_p (Translator*trans_l)
{
  assert (trans_l);
  
  PCursor<Translator*> trans_cur (trans_p_list_.find (trans_l));
  Translator * t =  trans_cur.remove_p();
  /*
    For elegant design, we would do this too.  Alas, it does not work yet..

    t-> removal_processing ();
  */
  t-> daddy_trans_l_ = 0;
  return t;
}


Translator*
Translator_group::get_simple_translator (char const *type) const
{
  Link_array<Translator> nongroups (nongroup_l_arr ());
  for (int i=0; i < nongroups.size(); i++)
    {
      if (nongroups[i]->name() == type)
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
      Translator_group * g= t->clone ()->access_Translator_group ();
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
  for (PCursor<Translator*> i (trans_p_list_.top ()); i.ok (); i++)
    (i.ptr()->*method) ();
}

void
Translator_group::each (Const_method_pointer method) const
{
  for (PCursor<Translator*> i (trans_p_list_.top ()); i.ok (); i++)
    (i.ptr()->*method) ();
}

void
Translator_group::do_print() const
{
#ifndef NPRINT
  if (!check_debug)
    return ;
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
