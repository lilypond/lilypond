/*
  request.hh -- declare Request baseclasses.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef REQUEST_HH
#define REQUEST_HH


#include "string.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "input.hh"
#include "music.hh"
#include "duration.hh"
#include "pitch.hh"

/** An atom of musical information.  This is an abstract class for any
  piece of music that does not contain other Music.
  

 */
class Request : public Music {
public:
  Request ();
  VIRTUAL_COPY_CONS (Music);
  virtual void compress (Moment);
  virtual void transpose (Pitch);
  virtual Moment get_length () const;
  virtual Pitch to_relative_octave (Pitch);
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



#endif
