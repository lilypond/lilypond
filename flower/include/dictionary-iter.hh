/*
  dictionary-iter.hh -- declare Dictionary_iter

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_ITER_HH
#define DICTIONARY_ITER_HH

#include "dictionary.hh"



#include "dictionary.hh"

template<class V>
class Dictionary_iter
{
  Dictionary<V> *dict_l_;
  int i;
public:
  Dictionary_iter(Dictionary<V> const &dict)
    {
      i =0;
      dict_l_ =(Dictionary<V> *) & dict;
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

  String key ()
    {
      return dict_l_->fixed_p_->dict_arr_[i].name_;
    }
  V val ()
    {
      return dict_l_->fixed_p_->dict_arr_[i].value_;      
    }
};

#endif // DICTIONARY_ITER_HH
