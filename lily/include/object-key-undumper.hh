/*
  object-key-undumper.hh -- declare Object_key_undumper

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef OBJECT_KEY_UNDUMPER_HH
#define OBJECT_KEY_UNDUMPER_HH

#include <map>
using namespace std;

#include "object-key.hh"

typedef map<int, Object_key const *> Int_to_key_map;

struct Object_key_undumper
{
  DECLARE_SMOBS (Object_key_undumper);
  Int_to_key_map keys_;
public:
  void parse_contents (SCM);
  Object_key_undumper ();
  Object_key const *get_key (int k);
};
DECLARE_UNSMOB (Object_key_undumper, key_undumper);

#endif
