/*
  dictionary.hh -- declare Dictionary

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_HH
#define DICTIONARY_HH

#include "string.hh"
#include "array.hh"

#include "hash-table.hh"


unsigned int string_hash (String);


template<class V>
struct Dict_initialiser
{
  char *key_;
  V value_;
};


template<class V>
class Dictionary : public Hash_table<String, V>
{
public:
  Dictionary ()
    {
      hash_func_ = string_hash;
    }
  Dictionary (Dict_initialiser<V> *p)
    {
      hash_func_ = string_hash;
      for (Dict_initialiser<V> *q = p; q->key_; q++)
	elem (q->key_) = q->value_;
	  
    }

  friend class Dictionary_iter<V>;
};


#endif // DICTIONARY_HH
