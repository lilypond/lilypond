/*
  notehead.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH
#include "item.hh"

/// ball at the end of the stem
struct Notehead : public Item
{
    int position;

    /// needed for the help-lines
    int staff_size;
    int dots;
    int balltype;
    int x_dir;
    
    /****************/
    
    void preprocess();

    Notehead(int staff_size);
    /**
      position of top line (5 linestaff: 8)
      */
    
    void print()const;
    static int compare(Notehead*&a, Notehead*&b) ;
private:
    void brew_molecole();
};
/**
  takes care of:

  * help lines  
  * proper placing of dots 

  */
#endif // NOTEHEAD_HH

