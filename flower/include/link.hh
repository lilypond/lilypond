// link.hh

#ifndef __LINK_HH
#define __LINK_HH
template<class T>
class List;


/// class for List 
template<class T>
class Link
{
//    friend class Cursor<T>;
public:    
    Link( T const & thing );
    
    Link<T>* previous();
    Link<T>* next();

    /// put new Link item after me in list
    void add( T const & thing );
    /// put new Link item before me in list
    void insert( T const & thing );	
    void remove(List<T> &l);
    
    T& thing();
    void OK() const;
private:    
    Link( Link<T>* previous, Link<T>* next, T const & thing );

    T thing_;
    Link<T>* previous_;
    Link<T>* next_;
};

#include "link.inl"

#endif // __LINK_HH //
