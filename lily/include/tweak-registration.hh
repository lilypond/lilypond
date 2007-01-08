/*
  tweak-registration.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef TWEAK_REGISTRATION_HH
#define TWEAK_REGISTRATION_HH

#include <map>
using namespace std;

#include "lily-proto.hh"
#include "object-key.hh"

typedef map<Object_key const *, SCM, Object_key_less> Tweak_map;

class Tweak_registry
{
  Tweak_map tweaks_;
  Object_key_undumper *undumper_;

  DECLARE_SMOBS (Tweak_registry);

public:
  Tweak_registry ();

  Object_key_undumper *undumper () const;
  void clear ();
  void insert_grob_tweak (Grob *, SCM);
  void replace_grob_tweak (Grob *, SCM);
  SCM get_tweaks (Grob *);
  SCM list_tweaks ();
  void insert_tweak_from_file (SCM);
};

extern Tweak_registry *global_registry_;

DECLARE_UNSMOB (Tweak_registry, tweak_registry);

#endif /* TWEAK_REGISTRATION_HH */
