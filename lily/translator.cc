/*
  translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "translator.hh"
#include "debug.hh"
#include "translator-group.hh"

#include "moment.hh"

char const*
Translator::name() const
{
  return classname(this);
}

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
  type_str_ = s.type_str_;
}

bool
Translator::is_alias_b (String s) const
{
  return s == type_str_;
}

bool
Translator::do_try_music (Music *)
{
  return false;
}
			    

Moment
Translator::now_mom () const
{
  return daddy_trans_l_->now_mom ();
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
  DEBUG_OUT << classname (this) << " {";
  if (classname (this) != type_str_)
    DEBUG_OUT << "type = " << type_str_;
  do_print ();
  DEBUG_OUT << "}\n";
#endif
}

void
Translator::do_print () const
{
}




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
Translator::try_music (Music * r)
{
  if (status < MOVE_INITED)
    post_move_processing ();

  return do_try_music (r);
}

void
Translator::process_music ()
{
  if (status < PROCESSED_REQS)
    post_move_processing ();
  else if (status >= PROCESSED_REQS)
    return; 
  
  status = PROCESSED_REQS;
  do_process_music ();
}

void
Translator::pre_move_processing ()
{
  do_pre_move_processing ();
  status = CREATION_INITED;
}



Music_output_def *
Translator::output_def_l () const
{
  return output_def_l_;
}

SCM
Translator::get_property (char const * id) const
{
  return daddy_trans_l_->get_property (ly_symbol2scm (id));
}

SCM
Translator::get_property (SCM sym) const
{
  return daddy_trans_l_->get_property (sym);
}

void
Translator:: do_pre_move_processing ()
{
}

void
Translator::do_post_move_processing ()
{
}

void
Translator::do_process_music ()
{
}

void
Translator::do_creation_processing ()
{
}

void
Translator::do_removal_processing ()
{
}
