/*
  notename.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef NOTENAME_HH
#define NOTENAME_HH
#error OBSOLETE!
#include "string.hh"

struct Notename_tab {
    String notetab[7*5];
    
    void set (int l, int s, String nm);
    void lookup (int &large, int &small, String s);
};

void set_notename_tab (Notename_tab*n);
void lookup_notename (int &large, int &small, String s);


#endif // NOTENAME_HH

