/*
  notename-table.hh -- declare Notename_table

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef NOTENAME_TABLE_HH
#define NOTENAME_TABLE_HH

#include "pointer.hh"
#include "string.hh"
#include "lily-proto.hh"
#include "dictionary.hh"
#include "musical-pitch.hh"

class Notename_table :   public Dictionary<Musical_pitch>{
public:
  String get_name (Musical_pitch) const;
};
  
#endif // NOTENAME_TABLE_HH
