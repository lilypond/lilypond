/*
  change-iterator.hh -- declare 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef CHANGE_ITERATOR_HH
#define CHANGE_ITERATOR_HH

#include "music-iterator.hh"


class Change_iterator : public Music_iterator
{
  Change_translator *change_l_;

  void  error (String);
  
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  Change_iterator (Change_translator* prop_l);
protected:
  virtual void process_and_next (Moment);
};

#endif
