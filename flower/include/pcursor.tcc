#include "pcursor.hh"

template<class T>
void
PCursor<T>::junk()
{
#if !defined (NDEBUG) && defined (PARANOID)
  list_l()->OK();
#endif

  delete ptr();
#if !defined (NDEBUG)&&defined (PARANOID)
  thing() = 0;
  list_l()->OK();
#endif
}
