/*
  scheme-listener.hh -- Declare Scheme_listener

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Erik Sandberg  <mandolaerik@gmail.com>
*/

#ifndef SCHEME_LISTENER_HH
#define SCHEME_LISTENER_HH

#include "listener.hh"
#include "ly-smobs.icc"

/*
 Scheme_listener is only used internally by scheme-listener-scheme.cc
*/

class Scheme_listener
{
public:
  Scheme_listener (SCM callback);
  DECLARE_LISTENER (call);
protected:
  DECLARE_SMOBS (Scheme_listener);
private:
  SCM callback_;
};

#endif /* SCHEME_LISTENER_HH */
