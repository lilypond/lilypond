/*
  swallow-reg.hh -- declare Swallow_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SWALLOW_REG_HH
#define SWALLOW_REG_HH

#include "register.hh"

/**
  This register swallows everything given to it silently. The purpose of
  this is to prevent spurious "request junked" warnings.
 */
class Swallow_register : public Request_register {
protected:
    bool acceptable_request_b(Request*) const;
    bool try_request(Request*) ;
public:
    NAME_MEMBERS();
};
#endif // SWALLOW_REG_HH
