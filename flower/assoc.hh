#ifndef ASSOC_HH
#define ASSOC_HH

#include "vray.hh"

template<class K,class V>
struct Assoc_ent_ {
    bool free;
    K key;
    V val;
};

template<class K, class V>
struct Assoc {
    svec< Assoc_ent_<K,V> > arr;

    /****************/
    
    int find(K key) const {
	for (int i = 0; i < arr.sz(); i++) {
	    if (!arr[i].free && key == arr[i].key)
		return i;
	}
	return -1;
    }
    int find_creat(K key) {
	int free = -1;
	for (int i = 0; i < arr.sz(); i++) {
	    if (key == arr[i].key) {		
		return i;
	    } else if (arr[i].free ) {
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
	arr.add(ae);
	return arr.sz() -1;
    }
public:
    bool elt_query(K key) const {
	return find(key) >= 0;
    }
    void del(K key) {
	assert(elt_query(key));
	int i= find(key);
	arr[i].free = true;
    }
    void
    add(K key, V val) {
	int i = find_creat(key);
	arr[i].val = val;
    }
    /**
    should create "set" template
    */
    V& operator[](K key) {
	return arr[find_creat(key)].val;
    }
    const V& operator[](K key) const {
	assert(elt_query(key));
	return arr[find(key)].val;
    }

};
#endif
