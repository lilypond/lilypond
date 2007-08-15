/*
  object-key-dumper.hh -- declare Object_key_dumper

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef OBJECT_KEY_DUMPER_HH
#define OBJECT_KEY_DUMPER_HH

#include <map>
using namespace std;

#include "object-key.hh"

typedef map<Object_key const *, Object_key const *, Object_key_less> Key_to_key_map;
typedef map<Object_key const *, int> Pointer_to_int_map;
typedef map<int, Object_key const *> Int_to_key_map;

class Object_key_dumper
{
  SCM file_contents_;
  Key_to_key_map serialized_keys_;
  Pointer_to_int_map key_serial_numbers_;
  int next_available_;

  SCM key_serial (int);
  SCM serialize_key (Object_key const *);
  DECLARE_SMOBS (Object_key_dumper);
public:
  Object_key_dumper ();
  SCM get_file_contents () const;
  SCM dump_key (Object_key const *);
};

DECLARE_UNSMOB (Object_key_dumper, key_dumper);

#endif /* OBJECT_KEY_DUMPER_HH */

