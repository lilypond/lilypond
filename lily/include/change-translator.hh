/*
  change-translator.hh -- declare Change_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef CHANGE_TRANSLATOR_HH
#define CHANGE_TRANSLATOR_HH

#include "music.hh"

class Change_translator : public Music
{
public:
  String change_to_type_str_;
  String change_to_id_str_;
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  VIRTUAL_COPY_CONS(Change_translator, Music);
  void do_print () const;
};


#endif // CHANGE_TRANSLATOR_HH
