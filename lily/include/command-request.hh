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

SCM transpose_key_alist (SCM,SCM);

#endif // COMMANDREQUEST_HH

