/*
  context-handle.cc -- implement Context_handle

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context-handle.hh"
#include "context.hh"

Context_handle::Context_handle ()
{
  outlet_ = 0;
}

Context_handle::Context_handle (Context_handle const &s)
{
  outlet_ = 0;
  if (s.outlet_)
    up (s.outlet_);
}

Context_handle::~Context_handle ()
{
  /*
    Don't do

    if (outlet_)
    down ();

    with GC, this is asynchronous.
  */
}

void
Context_handle::up (Context *t)
{
  outlet_ = t;
  t->iterator_count_++;
}

void
Context_handle::down ()
{
  outlet_->iterator_count_--;
  outlet_ = 0;
}

void
Context_handle::operator = (Context_handle const &s)
{
  set_context (s.outlet_);
}

void
Context_handle::set_context (Context *trans)
{
  if (outlet_ == trans)
    return;
  if (outlet_)
    down ();
  if (trans)
    up (trans);
}

Context *
Context_handle::get_outlet () const
{

  return outlet_;
}

int
Context_handle::get_count () const
{
  return outlet_->iterator_count_;
}
