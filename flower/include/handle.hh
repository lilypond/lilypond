#ifndef HANDLE_HH
#define HANDLE_HH

/// reference counting handle
template<class T>
class Handle {
    T *obj;
    int *refs;

    /// let go of ref. Delete if necessary
    void down() {
	if (!(*refs--)) {
	    delete obj;
	    delete refs;
	}
	obj = 0;
	refs = 0;
    }
    /// point to new object. 
    void up (T *t, int *r) {
	if (!r) {
	    refs = new int;
	    *refs = 1;
	} else {
	    refs =r;
	    *refs++;
	}
	obj = t;
    }
    /// POST: *refs == 1
    void copy() {
	if (*refs != 1){
	    T * newobj = new T(*obj);
	    down();
	    up (newobj);
	}
    }
    Handle (Handle const &src) {
	up (src.obj, src.refs);
    }
    Handle (T & o) {
	up (&o);
    }
    void operator=(Handle const& src) {
	if (this == &src)
	    return;
	down();
	up (src.o, src.refs);
    }
    operator T const &() {
	return *obj;
    }
    operator T&() {
	copy();
	return *obj;
    }
}
#endif
