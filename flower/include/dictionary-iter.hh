/*
  dictionary-iter.hh -- declare 

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_ITER_HH
#define DICTIONARY_ITER_HH

#include "string.hh"
#include "assoc-iter.hh"
#include "dictionary.hh"

template<class V>
class Dictionary_iter : public Assoc_iter<String, V>
{
public:
  Dictionary_iter (Dictionary<V> const &d)
    : Assoc_iter<String, V> (d)
  {
  }
};

#endif // DICTIONARY_ITER_HH
