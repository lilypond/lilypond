/*
  dictionary.hh -- declare Dictionary

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_HH
#define DICTIONARY_HH

#include "string.hh"
#include "array.hh"

#include <map>

unsigned int string_hash (String);


template<class V>
struct Dict_initialiser
{
  char *key_;
  V value_;
};


/*
  interface to STL function.
 */
template<class V>
class Dictionary : public map<String, V>
{
public:
  Dictionary ()
    {
    }
  Dictionary (Dict_initialiser<V> *p)
    {
      hash_func_ = string_hash;
      for (Dict_initialiser<V> *q = p; q->key_; q++)
	(*this) [q->key_] = q->value_;
	  
    }
  bool elem_b (String s)
  {
    map<String,V>::const_iterator ki (find (s));
    return ki != end ();
  }
  
};


#endif // DICTIONARY_HH
