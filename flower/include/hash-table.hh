/*   
  hash-table.hh -- declare Hash_table_entry, Hash_table
  
  source file of the Flower Library
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef HASH_TABLE_HH
#define HASH_TABLE_HH

unsigned int int_hash (int);
unsigned long prime_list (int idx);
template<class K, class V> struct Hash_table_iter;

template<class K>
unsigned int
pointer_hash (K *p)
{
  return int_hash ((unsigned int) p);
}

template<class K, class V>
struct Hash_table_entry
{
  K key_;
  V value_;
  bool free_b_;

  Hash_table_entry () {
    free_b_ = true;
  }
  Hash_table_entry (K s, V v)
    {
      key_ = s;
      value_ = v;
      free_b_ = false;
    }
};

/**
   A hash table of prime size.

   We use quadratic probing.  

  DEPRECATED. Use either SCM (preferred) or STL 
*/
template<class K, class V>
class Fixed_size_hash_table
{
public:
  Array<Hash_table_entry<K,V> > dict_arr_; 
  int size_idx_;
  Fixed_size_hash_table (int size_idx)
    {
      size_idx_ = size_idx;
      int sz = prime_list (size_idx_);
      dict_arr_.set_size (sz);
    }

  /// find #s#, or find first empty entry corresponding to #s#
  int lookup (K s, unsigned int initial_hash)
    {
      int sz =dict_arr_.size ();
      initial_hash = initial_hash % sz; 
      int i;
      int j = 0;
      while (j <= sz/2) {
	i = (initial_hash + j*j) % sz;
	
	if (dict_arr_[i].free_b_)
	  return i;

	if (dict_arr_[i].key_ == s)
	  return i;

	j++;
      }

      
      return -1;
    }

  /// remove #s# from the hash table.  
  V remove (K s, unsigned int initial_hash)
    {
      // TODO
      assert (false);
    }
};

/**
   Hash table with sliding sizes. 
 */
template<class K, class V>
class Hash_table
{
  Fixed_size_hash_table<K,V> * fixed_p_;
  /// set size to next prime, and copy contents
  void enlarge ()
    {
      Fixed_size_hash_table<K,V> *f = new Fixed_size_hash_table<K,V> (fixed_p_->size_idx_ +1);
      
      for (int i=0; i < fixed_p_->dict_arr_.size (); i++)
	{
	  if (fixed_p_->dict_arr_[i].free_b_)
	    continue;

	  K nm (fixed_p_->dict_arr_[i].key_);
	  unsigned int h = (*hash_func_) (nm);
	  int nl = f->lookup (nm, h);
	  
	  f->dict_arr_[nl] = Hash_table_entry<K,V> (nm, fixed_p_->dict_arr_[i].value_);
	}
      delete fixed_p_;
      fixed_p_ = f;
    }
public:
  Hash_table ()
    {
      hash_func_ = 0;
      fixed_p_ = new Fixed_size_hash_table<K,V> (0);
    }
  ~Hash_table ()
    {
      delete fixed_p_;
    }
  void operator = (Hash_table<K,V> const &src)
    {
      if (&src == this)
	return;
      
      delete fixed_p_;
      fixed_p_ = new Fixed_size_hash_table<K,V> (*src.fixed_p_);
      hash_func_ = src.hash_func_;
    }
  Hash_table (Hash_table<K,V> const &src)
    {
      fixed_p_ = new Fixed_size_hash_table<K,V> (*src.fixed_p_);
      hash_func_ = src.hash_func_;
    }

  void clear ()
    {
      int i= fixed_p_->size_idx_;
      delete fixed_p_;
      fixed_p_ = new Fixed_size_hash_table<K,V> (i);
    }
  bool elem_b (K s) const
    {
      int l =  fixed_p_->lookup (s, (*hash_func_) (s));

      return (l >= 0 && !fixed_p_->dict_arr_[l].free_b_) ;
    }

  /**
     Find and return element.  If #s# is not in the table, create an
     entry in the table, and init */
  V& elem (K s)
    {
      int l;
      unsigned int h = (*hash_func_) (s);
      while ((l= fixed_p_->lookup (s,h)) <0)
	{
	  enlarge ();
	}
      
       fixed_p_->dict_arr_[l].free_b_ = false;
       fixed_p_->dict_arr_[l].key_ = s;
       return fixed_p_->dict_arr_[l].value_;
    }
  bool try_retrieve (K k, V *v)
  {
    int l =  fixed_p_->lookup (k, (*hash_func_) (k));
    if (l < 0 || fixed_p_->dict_arr_[l].free_b_)
      return false;
    else
      {
	*v = fixed_p_->dict_arr_[l].value_;
	return true;
      }
  }
  V elem (K s) const
    {
      return const_elem (s);
    }
  V const_elem (K k) const
  {
      V retval;
      assert (elem_b (k));
      retval = ((Hash_table<K,V>*)this)->elem (k);
      return retval;
  }
  V& operator [] (K k)
    {
      return elem (k);
    }

  V operator [] (K k) const
    {
      return const_elem (k);
    }

  V remove (K s)
    {
      return fixed_p_->remove (s, (*hash_func_) (s));
    }
  friend class Hash_table_iter<K,V>;
public:
  unsigned int (*hash_func_) (K);
};


#endif /* HASH_TABLE_HH */

