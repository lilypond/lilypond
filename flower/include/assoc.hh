#ifndef ASSOC_HH
#define ASSOC_HH

#include "array.hh"
#include <assert.h>

/**
  A helper for Assoc
 */
template<class K, class V>
struct Assoc_ent_ {
  bool free;
  K key;
  V val;
};


/** mindblowingly stupid Associative array implementation.
  Hungarian: map

  TODO: a decent hash for strings.
 */
template<class K, class V>
struct Assoc {
  Array< Assoc_ent_<K,V> > arr;

  /* ************** */
    
  int find (K key) const {
    for (int i = 0; i < arr.size(); i++) {
      if (!arr[i].free && key == arr[i].key)
	return i;
    }
    return -1;
  }
  int find_creat (K key) {
    int free = -1;
    for (int i = 0; i < arr.size(); i++) {
      if (!arr[i].free && key == arr[i].key) {		
	return i;
      } else if (arr[i].free) {
	free = i;
      }
    }
    if (free >= 0){
      arr[free].free = false;
      arr[free].key = key;
      return free;
    }

    Assoc_ent_<K,V> ae;
    ae.free = false;
    ae.key = key;
    arr.push (ae);
    return arr.size() -1;
  }
public:
  bool elem_b (K key) const {
    return find (key) >= 0;
  }
  void del (K key) {
    assert (elem_b (key));
    int i= find (key);
    arr[i].free = true;
  }
  void add (K key, V val) {
    int i = find_creat (key);
    arr[i].val = val;
  }
  V& elem (K key) {
    return arr[find_creat (key)].val;
  }
  V& operator[](K key) {
    return elem (key);
  }
  V const & operator[](K key) const {
    return elem (key);
  }
  V const & elem (K key) const { 
    assert (elem_b (key));
    return arr[find (key)].val;
  }
  void clear () 
  {
    for (int i=0 ;  i < arr.size (); i++)
      arr[i].free = true;
  }
};

#endif
