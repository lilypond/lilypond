/*
  dictionary.hh -- declare Dictionary

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DICTIONARY_HH
#define DICTIONARY_HH

#include "string.hh"
#include "assoc.hh"

/**
  UGH:  write a String_hash template, 

  SEE:
  
       #include <search.h>

       ENTRY *hsearch(ENTRY item, ACTION action);

       int     hcreate (unsigned nel);

       void    hdestroy (void);

  (should be frobnified to allow multiple hashes)
 */
template<class T>
class Dictionary : public Assoc<String, T>
{
public:
  
};

#endif // DICTIONARY_HH
