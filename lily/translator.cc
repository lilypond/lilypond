/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "translator.hh"
#include "debug.hh"
#include "translator-group.hh"
#include "dictionary-iter.hh"
#include "rational.hh"

Translator::~Translator ()
{
}

Translator::Translator ()
{
  status = ORPHAN;
  daddy_trans_l_ = 0;
  output_def_l_ = 0;
}

Translator::Translator (Translator const &s)
  : Input (s)
{
  status = ORPHAN;
  daddy_trans_l_ =0;
  output_def_l_ = s.output_def_l_;
  properties_dict_ = s.properties_dict_;
  type_str_ = s.type_str_;
}

bool
Translator::is_alias_b (String s) const
{
  return s == type_str_;
}

bool
Translator::do_try_request (Request *)
{
  return false;
}
			    

Moment
Translator::now_moment () const
{
  return daddy_trans_l_->now_moment ();
}


void
Translator::add_processing ()
{
  if (status > ORPHAN)
    return;
  
  do_add_processing ();
  status = VIRGIN;
}

void
Translator::do_add_processing ()
{
}

void
Translator::print () const
{
#ifndef NPRINT
  DOUT << name () << " {";
  if (name () != type_str_)
    DOUT << "type = " << type_str_;
  for (Dictionary_iter<Scalar> i (properties_dict_); i.ok (); i++)
    {
      DOUT << i.key () << "=" << i.val () << '\n';
    }
  do_print ();
  DOUT << "}\n";
#endif
}

void
Translator::do_print () const
{
}

IMPLEMENT_IS_TYPE_B(Translator);


void
Translator::creation_processing ()
{
  if (status >= CREATION_INITED)
    return ;
  
  do_creation_processing ();
  status = CREATION_INITED;
}

void
Translator::post_move_processing ()
{
  if (status >= MOVE_INITED)
    return;

  creation_processing ();
  do_post_move_processing ();
  status = MOVE_INITED;
}

void
Translator::removal_processing ()
{
  if (status == ORPHAN)
    return;
  creation_processing ();
  do_removal_processing ();
  // elegancy ...
  // status = ORPHAN;
}


bool
Translator::try_request (Request * r)
{
  if (status < MOVE_INITED)
    post_move_processing ();

  return do_try_request (r);
}

void
Translator::process_requests ()
{
  if (status < PROCESSED_REQS)
    post_move_processing ();
  else if (status >= PROCESSED_REQS)
    return; 
  
  status = PROCESSED_REQS;
  do_process_requests ();
}

void
Translator::pre_move_processing ()
{
  do_pre_move_processing ();
  status = CREATION_INITED;
}

Scalar
Translator::get_property (String id)
{
  if (properties_dict_.elem_b (id))
    {
      return properties_dict_[id];
    }
  
  if (daddy_trans_l_)
    return daddy_trans_l_->get_property (id);

  return "";
}

void
Translator::set_property (String id, Scalar val)
{
  properties_dict_[id] = val;
}


Music_output_def *
Translator::output_def_l () const
{
  return output_def_l_;
}
