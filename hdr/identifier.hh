
/*
  identifier.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef IDENTIFIER_HH
#define IDENTIFIER_HH
#include "identparent.hh"
#include "inputstaff.hh"
#include "inputmusic.hh"

#define make_id_class(Idclass, Class, accessor)	\
struct Idclass : Identifier {\
    Idclass(String s, Class*st):Identifier(s) { data = st; }\
    virtual Class* accessor() { return (Class*) data; }\
    ~Idclass() { delete accessor(); }\
}\


make_id_class(Staff_id, Input_staff, staff);
make_id_class(M_chord_id, Music_general_chord, mchord);
make_id_class(M_voice_id, Music_voice, mvoice);

#endif // IDENTIFIER_HH

