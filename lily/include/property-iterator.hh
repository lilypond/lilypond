/*
  property-iterator.hh -- declare Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PROPERTY_ITERATOR_HH
#define PROPERTY_ITERATOR_HH

#include "music-iterator.hh"




/**
  Iterate a property.  
 */
class Property_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  /* construction */
protected:
  virtual void do_process (Moment);
};

class Push_property_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
protected:
  /* construction */
  virtual void do_process (Moment);
};

class Pop_property_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
protected:
  /* construction */
  virtual void do_process (Moment);
};


#endif // PROPERTY_ITERATOR_HH
