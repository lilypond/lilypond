/*   
  hash-table-iter.hh -- declare Hash_table_iter

  source file of the Flower Library
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef HASH_TABLE_ITER_HH
#define HASH_TABLE_ITER_HH
#include "hash-table.hh"

template<class K, class V>
class Hash_table_iter
{
  Hash_table<K,V> *dict_l_;
  int i;
public:
  Hash_table_iter (Hash_table<K,V> const &dict)
    {
      i = 0;
      dict_l_ = (Hash_table<K,V> *) & dict;
      next_used ();
    }

  bool ok () const
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
  void operator ++ (int)
    {
      i++;
      next_used ();
    }

  K key () const
    {
      return dict_l_->fixed_p_->dict_arr_[i].key_;
    }
  V val () const
    {
      return dict_l_->fixed_p_->dict_arr_[i].value_;      
    }
  V &val_ref ()
    {
      return dict_l_->fixed_p_->dict_arr_[i].value_;      
    }
};


#endif /* HASH_TABLE_ITER_HH */

