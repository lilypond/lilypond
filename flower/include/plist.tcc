#include "plist.hh"

#define PL_instantiate(a)  template class Pointer_list<a*>; \
	template class PCursor<a*>;
#define IPL_instantiate(a) PL_instantiate(a); \
	template class IPointer_list<a*>
	
template<class T>
IPointer_list<T>::~IPointer_list()
{
    PCursor<T> c( *this );
    while (c.ok()) {
	delete c.remove_p();
    }
}

template<class T>
PCursor<T> 
Pointer_list<T>::find(T what ) const
{
    PCursor<T> i(*this);
    for (; i.ok(); i++)
	if (i.ptr() == what)
	   break;
    return i;    
}
