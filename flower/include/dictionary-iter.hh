/*
  dictionary-iter.hh -- declare Dictionary_iter

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_ITER_HH
#define DICTIONARY_ITER_HH

#include "dictionary.hh"

template<class K, class V>
class Hash_table_iter
{
  Hash_table<K,V> *dict_l_;
  int i;
public:
  Hash_table_iter(Hash_table<K,V> const &dict)
    {
      i =0;
      dict_l_ =(Hash_table<K,V> *) & dict;
      next_used ();
    }

  bool ok ()
    {
      return i < dict_l_->fixed_p_->dict_arr_.size ();
    }

  void next_used ()
    {
      while (ok () && dict_l_->fixed_p_->dict_arr_[i].free_b_)
	{
	  i ++;
	}
    }
  void operator ++(int)
    {
      i++;
      next_used ();
    }

  K key ()
    {
      return dict_l_->fixed_p_->dict_arr_[i].key_;
    }
  V val () 
    {
      return dict_l_->fixed_p_->dict_arr_[i].value_;      
    }
  V &val_ref ()
    {
      return dict_l_->fixed_p_->dict_arr_[i].value_;      
    }
};

template<class V>
class Dictionary_iter<V> : public Hash_table_iter<String,V>
{
public:
  Dictionary_iter (Dictionary<V> const &d)
    : Hash_table_iter<String,V> (d)
    {

    }
  
  
};
#endif // Hash_table_ITER_HH
