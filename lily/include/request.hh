/*
  request.hh -- declare Request baseclasses.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef REQUEST_HH
#define REQUEST_HH


#include "string.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "input.hh"
#include "music.hh"
#include "direction.hh"

#define DEFAULTACCESSOR(T)  virtual T *access_ ## T () { return 0; }


/**
   An atom of musical information .
 Hungarian postfix: req
 @see lilygut manpage
 */
class Request : public Music {

public:
    
  /* *************** */

  virtual ~Request(){}

  DECLARE_MY_RUNTIME_TYPEINFO;
  VIRTUAL_COPY_CONS(Request,Music);
    
  virtual MInterval time_int() const;
  virtual Moment duration() const { return 0; }

  /*  accessors for children
      maybe checkout RTTI
  */

  DEFAULTACCESSOR(Barcheck_req)
  DEFAULTACCESSOR(Script_req)
  DEFAULTACCESSOR(Span_req)
  DEFAULTACCESSOR(Spacing_req)
  DEFAULTACCESSOR(Musical_req)
  DEFAULTACCESSOR(Command_req)
    
  bool equal_b (Request*) const;
protected:
  virtual bool do_equal_b (Request*) const;
  virtual void do_print() const;
};


#define REQUESTMETHODS(T)	\
virtual T * access_ ## T() { return this;}\
DECLARE_MY_RUNTIME_TYPEINFO;\
VIRTUAL_COPY_CONS(T, Request);\
virtual void do_print() const



/** Put a script above or below this ``note'' or bar. eg upbow, downbow. Why
  a request? These symbols may conflict with slurs and brackets, so
  this also a request */

class Script_req  : public virtual Request { 
public:
  Direction dir_;
  General_script_def *scriptdef_p_;
  
  bool do_equal_b (Request*) const;

  Script_req();
  REQUESTMETHODS(Script_req);
  ~Script_req();
  Script_req (Script_req const&);
};

    
#endif
