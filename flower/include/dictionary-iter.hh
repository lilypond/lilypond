/*
  dictionary-iter.hh -- declare Dictionary_iter

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_ITER_HH
#define DICTIONARY_ITER_HH

#include "dictionary.hh"
#include "hash-table-iter.hh"


template<class V>
class Dictionary_iter : public Hash_table_iter<String,V>
{
public:
  Dictionary_iter (Dictionary<V> const &d)
    : Hash_table_iter<String,V> (d)
    {

    }
  
  
};
#endif // Hash_table_ITER_HH
