/*
  notename-table.hh -- declare Notename_table

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef NOTENAME_TABLE_HH
#define NOTENAME_TABLE_HH

#include "string.hh"
#include "lily-proto.hh"

class Notename_table {
  Dictionary<Musical_pitch> *pitch_dict_;
public:
  Notename_table ();
  ~Notename_table ();
  Notename_table (Notename_table const&);
  Musical_pitch get_pitch(String s) const;
  void add_note_name (String, Musical_pitch const *p);
  bool elem_b (String )const;
};
  
#endif // NOTENAME_TABLE_HH
