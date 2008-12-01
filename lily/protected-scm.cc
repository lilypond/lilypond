/*
  protected-scm.cc -- implement Protected_scm

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "protected-scm.hh"

Protected_scm::Protected_scm ()
{
  object_ = SCM_UNDEFINED;
}

Protected_scm::Protected_scm (SCM s)
{
  object_ = SCM_NIMP (s) ? scm_gc_protect_object (s) : s;
}

Protected_scm::Protected_scm (Protected_scm const &s)
{
  object_ = (SCM_NIMP (s.object_) ? scm_gc_protect_object (s.object_)
	     : s.object_);
}

Protected_scm::~Protected_scm ()
{
  if (SCM_NIMP (object_))
    scm_gc_unprotect_object (object_);
}

Protected_scm &
Protected_scm::operator = (SCM s)
{
  if (object_ == s)
    return *this;

  if (SCM_NIMP (object_))
    scm_gc_unprotect_object (object_);

  object_ = SCM_NIMP (s) ? scm_gc_protect_object (s) : s;
  return *this;
}

Protected_scm &
Protected_scm::operator = (Protected_scm const &s)
{
  return operator = (s.object_);
}

Protected_scm::operator SCM () const
{
  return object_;
}

SCM
Protected_scm::to_SCM () const
{
  return object_;
}
