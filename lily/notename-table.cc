/*
  notename-table.cc -- implement Notename_table

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "notename-table.hh"
#include "dictionary-iter.hh"
#include "dictionary.hh"
#include "musical-pitch.hh"

void
Notename_table::add_note_name (String s,
			       Musical_pitch const *p)
{
  pitch_dict_->elem (s) = *p;
}

Notename_table::Notename_table ()
{
  pitch_dict_ = new Dictionary<Musical_pitch>;
}

Notename_table::~Notename_table()
{
  delete pitch_dict_;
}

Notename_table::Notename_table (Notename_table const &s)
{
  pitch_dict_ = new Dictionary<Musical_pitch> (*s.pitch_dict_);
}
bool
Notename_table::elem_b (String s)const
{
  return pitch_dict_->elem_b (s);
}
Musical_pitch
Notename_table::get_pitch (String s)const
{
  return (*pitch_dict_)[s];
}
