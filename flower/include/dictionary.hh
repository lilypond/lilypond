/*
  dictionary.hh -- declare Dictionary

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_HH
#define DICTIONARY_HH

#include "string.hh"
#include "array.hh"

unsigned long prime_list (int idx);
template<class K, class V>
struct Hash_table_entry
{
  K key_;
  V value_;
  bool free_b_;

  Hash_table_entry() {
    free_b_ = true;
  }
  Hash_table_entry (K s, V v)
    {
      key_ = s;
      value_ = v;
      free_b_ = false;
    }
};

unsigned int hash (String);
unsigned int hash (int);

template<class V>
struct Dict_initialiser
{
  char *key_;
  V value_;
};

/**
   A hash table of prime size.

   We use quadratic probing.  
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
      int sz = prime_list(size_idx_);
      dict_arr_.set_size (sz);
    }

  /// find #s#, or find first empty entry corresponding to #s#
  int lookup  (K s)
    {
      int sz =dict_arr_.size ();
      int i = hash (s) % sz;
      int j = 0;
      while (j <= sz/2) {
	if (dict_arr_[i].free_b_)
	  return i;
	    
	if (dict_arr_[i].key_ == s)
	  return i;

	j++;
	i = (i + j*j) % sz;
      }

      return -1;
    }

  /// remove #s# from the hash table.  
  V remove (K s)
    {
      assert (false);		// Untested routine.
      int sz =dict_arr_.size ();
      int i = hash (s) % sz;
      int j = 0;
      V retval;
      while (j <= sz/2 && dict_arr_[i].key_ != s)
	{
	  assert (!dict_arr_[i].free_b_);
	    
	
	  j ++;
	  i = (i + j*j) % sz;
	}

      j++;
      int nexti = (i + j*j) % sz;

      while (j <= sz/2 && !dict_arr_[i].free_b_)
	{
	  dict_arr_[i] = dict_arr_[nexti];
	  j++;
	  i = nexti;
	  nexti = (nexti + j*j)%sz;
	}
      
      return retval;
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
      for (int i=0; i < fixed_p_->dict_arr_.size(); i++)
	{
	  if (fixed_p_->dict_arr_[i].free_b_)
	    continue;

	  K nm (fixed_p_->dict_arr_[i].key_);
	  int nl = f->lookup (nm);
	  
	  f->dict_arr_[nl] = Hash_table_entry<K,V> (nm, fixed_p_->dict_arr_[i].value_);
	}
      delete fixed_p_;
      fixed_p_ = f;
    }
public:
  Hash_table ()
    {
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
    }
  Hash_table (Hash_table<K,V> const &src)
    {
      fixed_p_ = new Fixed_size_hash_table<K,V> (*src.fixed_p_);
    }

  void clear ()
    {
      int i= fixed_p_->size_idx_;
      delete fixed_p_;
      fixed_p_ = new Fixed_size_hash_table<K,V> (i);
    }
  bool elem_b (K s) const
    {
      int l =  fixed_p_->lookup (s);

      return (l >= 0 && !fixed_p_->dict_arr_[l].free_b_) ;
    }

  /**
     Find and return element.  If #s# is not in the table, create an entry in the table, and init
   */
  V& elem (K s)
    {
      int l;
      while ((l= fixed_p_->lookup (s)) <0)
	{
	  enlarge ();
	}

      
       fixed_p_->dict_arr_[l].free_b_ = false;
       fixed_p_->dict_arr_[l].key_ = s;
       return fixed_p_->dict_arr_[l].value_;
    }
  V elem (K s) const
    {
      return const_elem (s);
    }
  V const_elem (K k) const
  {
      V retval;
      if (elem_b (k))
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
      return fixed_p_->remove (s);      
    }
  friend class Hash_table_iter<K,V>;
};

template<class V>
class Dictionary : public Hash_table<String, V>
{
public:
  Dictionary ()
    {}
  Dictionary (Dict_initialiser<V> *p)
    {
      for (Dict_initialiser<V> *q = p; q->key_; q++)
	elem (q->key_) = q->value_;
	  
    }

  friend class Dictionary_iter<V>;
};


#endif // DICTIONARY_HH
