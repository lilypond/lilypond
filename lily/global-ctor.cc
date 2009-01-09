/*
  global-ctor.cc -- implement global constructors

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "global-ctor.hh"

#include "std-vector.hh"

static vector<Global_ctor> *ctor_global_statics_;

void
add_constructor (Global_ctor c)
{
  if (!ctor_global_statics_)
    ctor_global_statics_ = new vector<Global_ctor>;
  ctor_global_statics_->push_back (c);
}

void
call_constructors ()
{
  for (vsize i = 0; i < ctor_global_statics_->size (); i++)
    (ctor_global_statics_->at (i)) ();
}
