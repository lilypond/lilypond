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
template<class V>
struct Dict_entry
{
  String name_;
  V value_;
  bool free_b_;

  Dict_entry() {
    free_b_ = true;
  }
  Dict_entry (String s, V v)
    {
      name_ = s;
      value_ = v;
      free_b_ = false;
    }
};

unsigned int hash (String);

/**
   A hash table of prime size.

   We use quadratic probing.  
 */
template<class V>
class Fixed_size_dictionary
{
public:
  Array<Dict_entry<V> > dict_arr_; 
  int size_idx_;
  Fixed_size_dictionary (int size_idx)
    {
      size_idx_ = size_idx;
      int sz = prime_list(size_idx_);
      dict_arr_.set_size (sz);
    }

  /// find #s#, or find first empty entry corresponding to #s#
  int lookup  (String s)
    {
      int sz =dict_arr_.size ();
      int i = hash (s) % sz;
      int j = 0;
      while (j <= sz/2) {
	if (dict_arr_[i].free_b_)
	  return i;
	    
	if (dict_arr_[i].name_ == s)
	  return i;

	j++;
	i = (i + j*j) % sz;
      }

      return -1;
    }

  /// remove #s# from the hash table.  
  V remove (String s)
    {
      assert (false);		// Untested routine.
      int sz =dict_arr_.size ();
      int i = hash (s) % sz;
      int j = 0;
      V retval;
      while (j <= sz/2 && dict_arr_[i].name_ != s)
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
    }
};

/**
   Hash table with sliding sizes. 
 */
template<class V>
class Dictionary
{
  Fixed_size_dictionary<V> * fixed_p_;

  /// set size to next prime, and copy contents
  void enlarge ()
    {
      Fixed_size_dictionary<V> *f = new Fixed_size_dictionary<V> (fixed_p_->size_idx_ +1);
      for (int i=0; i < fixed_p_->dict_arr_.size(); i++)
	{
	  if (fixed_p_->dict_arr_[i].free_b_)
	    continue;

	  String nm (fixed_p_->dict_arr_[i].name_);
	  int nl = f->lookup (nm);
	  
	  f->dict_arr_[nl] = Dict_entry<V> (nm, fixed_p_->dict_arr_[i].value_);
	}
      delete fixed_p_;
      fixed_p_ = f;
    }
public:
  Dictionary ()
    {
      fixed_p_ = new Fixed_size_dictionary<V> (0);
    }
  ~Dictionary ()
    {
      delete fixed_p_;
    }
  void operator = (Dictionary<V> const &src)
    {
      if (&src == this)
	return;
      
      delete fixed_p_;
      fixed_p_ = new Fixed_size_dictionary<V> (*src.fixed_p_);
    }
  Dictionary (Dictionary<V> const &src)
    {
      fixed_p_ = new Fixed_size_dictionary<V> (*src.fixed_p_);
    }
  bool elem_b (String s) const
    {
      int l =  fixed_p_->lookup (s);

      return (l >= 0 && !fixed_p_->dict_arr_[l].free_b_) ;
    }

  /**
     Find and return element.  If #s# is not in the table, create an entry in the table, and init
   */
  V& elem (String s)
    {
      int l;
      while ((l= fixed_p_->lookup (s)) <0)
	{
	  enlarge ();
	}

      
       fixed_p_->dict_arr_[l].free_b_ = false;
       fixed_p_->dict_arr_[l].name_ = s;
       return fixed_p_->dict_arr_[l].value_;
    }
  V elem (String s) const
    {
      return const_elem (s);
    }
  V const_elem (String k) const
  {
      V retval;
      if (elem_b (k))
	retval = ((Dictionary<V>*)this)->elem (k);
      return retval;
  }
  V& operator [] (String k)
    {
      return elem (k);
    }

  V operator [] (String k) const
    {
      return const_elem (k);
    }
  

  V remove (String s)
    {
      return fixed_p_->remove (s);      
    }
  friend class Dictionary_iter<V>;
};


#endif // DICTIONARY_HH
