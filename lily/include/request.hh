/*
  request.hh -- declare Request baseclasses.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Request ();
  VIRTUAL_COPY_CONS(Music);
  bool equal_b (Request const*) const;
protected:
  virtual bool do_equal_b (Request const*) const;

};



class Script_req : public virtual Request
{
public:
  void set_direction (Direction d);
  Direction get_direction () const;

  VIRTUAL_COPY_CONS(Music);
  Script_req ();
};


/**
  Requests to start or stop something.
 This type of request typically results in the creation of a #Spanner#
*/
class Span_req  : public virtual Request  {
public:
  String get_span_type_str () const;
  void set_span_type_str (String);
  void set_span_dir (Direction d);
  Direction get_span_dir () const;  
  
  Span_req();
protected:
  
  virtual bool do_equal_b (Request const*) const;

  VIRTUAL_COPY_CONS(Music);
};

/**
  Start a tie at this note, end it at the next
 */
class Tie_req : public Request {
public:
  VIRTUAL_COPY_CONS(Music);
};

#endif
