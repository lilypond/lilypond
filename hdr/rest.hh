/*
  rest.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef REST_HH
#define REST_HH
#include "item.hh"

/// ball at the end of the stem
struct Rest : public Item
{
    int dots;
    int balltype;

    /****************/

    void preprocess();
    Rest(int dur,int dots);
    void print()const;
private:
    void brew_molecole();
};
/**
  takes care of:

  * help lines  
  * proper placing of dots 

  */
#endif 

