/*
  request-iter.hh -- declare Request_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REQUEST_ITER_HH
#define REQUEST_ITER_HH

#include "simple-music-iterator.hh"

/**
   Walk through a Request_chord
 */
class Request_chord_iterator : public Simple_music_iterator
{
  Request_chord * elt_l () const;
  /**
     Find a bottom notation context to deliver requests to.
   */
  virtual Translator_group* get_req_translator_l ();


  /*
    Since Request_chord_iterator has no list-cursor internally, we
    must use a status variable to adminstrate where we are */
  
  enum { NONE_DONE, START_DONE, END_DONE }  status_;
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  Request_chord_iterator ();
  Request_chord_iterator (Request_chord_iterator const&);

  virtual SCM get_music (Moment) const;
protected:
  virtual void process (Moment);
  virtual void construct_children();
};


#endif // REQUEST_ITER_HH
