#include "plist.hh"

#define PL_instantiate(a)  template class PointerList<a*>; \
	template class PCursor<a*>;
#define IPL_instantiate(a) PL_instantiate(a); \
	template class IPointerList<a*>
	
template<class T>
IPointerList<T>::~IPointerList()
{
    PCursor<T> c( *this );
    while (c.ok()) {
	delete c.remove_p();
    }
}

template<class T>
PCursor<T> 
PointerList<T>::find(T what ) const
{
    PCursor<T> i(*this);
    for (; i.ok(); i++)
	if (i.ptr() == what)
	   break;
    return i;    
}
