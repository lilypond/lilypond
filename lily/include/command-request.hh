/*
  command-request.hh -- declare non-musical requests

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COMMANDREQUEST_HH
#define COMMANDREQUEST_HH

#include "request.hh"
#include "array.hh"
#include "duration.hh"
#include "pitch.hh"


class Break_req : public Request {
public:
protected:
  VIRTUAL_COPY_CONS (Music);
};

class Mark_req : public Request {
public:
  virtual bool do_equal_b (Request const*) const;
  SCM mark_label ();
  VIRTUAL_COPY_CONS (Music);
};

/*
    int metronome_i_;
 */
class Tempo_req : public Request
{
public:
  Tempo_req ();
protected:
  VIRTUAL_COPY_CONS (Music);
};

class Breathing_sign_req : public Request {
  VIRTUAL_COPY_CONS (Music);
};

class Porrectus_req : public Request {
  VIRTUAL_COPY_CONS (Music);
};

/**
    Handle key changes.
*/
class Key_change_req  : public Request
{
public:
  SCM pitch_alist ();
  
protected:
  VIRTUAL_COPY_CONS (Music);
  bool do_equal_b (Request const * ) const;
  void transpose (Pitch  d);
};

#endif // COMMANDREQUEST_HH

