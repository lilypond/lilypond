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


/** An atom of musical information.  This is an abstract class for any
  piece of music that does not contain other Music.
  
  Hungarian postfix: req

 */
class Request : public Music {

public:
    
  virtual ~Request(){}

  
  VIRTUAL_COPY_CONS(Music);
    
  bool equal_b (Request*) const;
protected:
  virtual bool do_equal_b (Request*) const;
  virtual void do_print() const;
};


#define REQUESTMETHODS(T)	\
\
VIRTUAL_COPY_CONS(Request);\
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


/**
  Requests to start or stop something.
 This type of request typically results in the creation of a #Spanner#
*/
class Span_req  : public virtual Request  {
public:
  /// should the spanner start or stop, or is it unwanted?
  Direction spantype_;
  bool do_equal_b (Request*) const;
  REQUESTMETHODS(Span_req);

  Span_req();
};


#endif
