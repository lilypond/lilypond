/*
  tweak-registration.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef TWEAK_REGISTRATION_HH
#define TWEAK_REGISTRATION_HH

#include  <map>

#include "lily-proto.hh"
#include "smobs.hh"
#include "lily-guile.hh"

typedef std::map<Object_key const*, SCM, Object_key_less> Tweak_map ;

class Tweak_registry
{
  Tweak_map tweaks_;
  Object_key_undumper *undumper_;

  DECLARE_SMOBS(Tweak_registry,);
  
public:
  Object_key_undumper *undumper() const;
  void clear ();
  void insert_tweak (SCM);
  SCM get_tweaks (Grob *);
  SCM list_tweaks ();
  Tweak_registry ();
};

DECLARE_UNSMOB(Tweak_registry, tweak_registry);

#endif /* TWEAK_REGISTRATION_HH */

