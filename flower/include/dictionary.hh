/*
  dictionary.hh -- declare Dictionary

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef DICTIONARY_HH
#define DICTIONARY_HH

#include "string.hh"
#include "assoc.hh"

template<class T>
class Dictionary : public Assoc<String, T>
{
public:
  
};

#endif // DICTIONARY_HH
