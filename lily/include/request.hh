/*
  request.hh -- declare Request baseclasses.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef REQUEST_HH
#define REQUEST_HH


#include "string.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "input.hh"
#include "music.hh"
#include "direction.hh"

/**
 a voice element wants something printed.
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
    virtual Barcheck_req *barcheck() { return 0; }
    virtual Script_req *script() { return 0;}
    virtual Span_req *span() { return 0; }
    virtual Spacing_req * spacing() { return 0; }
    virtual Blank_req * blank() { return 0; }
    virtual Musical_req *musical() { return 0; }
    virtual Command_req * command() { return 0; }
    bool equal_b (Request*) const;
protected:
    virtual bool do_equal_b (Request*) const;
    virtual void do_print() const;
};

#define REQUESTMETHODS(T,accessor)	\
virtual T * accessor() { return this;}\
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

    /* *************** */
    bool do_equal_b (Request*) const;

    Script_req();
    REQUESTMETHODS(Script_req,script);
    ~Script_req();
    Script_req (Script_req const&);
};

    
#endif
