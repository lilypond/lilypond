/*
  command-request.hh -- declare non-musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COMMANDREQUEST_HH
#define COMMANDREQUEST_HH

#include "request.hh"
#include "array.hh"
#include "duration.hh"
#include "musical-pitch.hh"
#include "protected-scm.hh"

class Break_req : public Request {
public:

  Break_req ();
protected:
  VIRTUAL_COPY_CONS(Music);
};

class Mark_req : public Request {
public:
  virtual bool do_equal_b (Request const*) const;
  SCM mark_label ();
  VIRTUAL_COPY_CONS(Music);
};

/*
    int metronome_i_;
 */
class Tempo_req : public Request
{
public:
  Duration dur_;


  Tempo_req();
protected:

  VIRTUAL_COPY_CONS(Music);
  bool do_equal_b (Request const *) const;
};

/// check if we're at start of a  measure.
class Barcheck_req  : public Request  {
public:
  bool do_equal_b (Request const *) const;
  VIRTUAL_COPY_CONS(Music);
};

class Breathing_sign_req : public Request {
  VIRTUAL_COPY_CONS(Music);
};

/**
    Handle key changes.
*/
class Key_change_req  : public Request
{
public:
  SCM pitch_alist ();
  
protected:
  VIRTUAL_COPY_CONS(Music);
  void transpose (Musical_pitch  d);
  bool do_equal_b (Request const * )const; 
};

#endif // COMMANDREQUEST_HH
