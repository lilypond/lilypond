/*
  grob-selector.hh -- declare Grob_selector

  source file of the LilyPond music typesetter
  
  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef GROB_SELECTOR_HH
#define GROB_SELECTOR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "protected-scm.hh"

/**
 * Grob_selector:
 * @register_grob: register new #GROB.
 #
 **/
class Grob_selector
{
  static Scheme_hash_table *grobs_;
  static Protected_scm tweaks_;

public:
  static void register_grob (Context *context, Grob *grob);
  static SCM identify_grob (Context *context, Moment m, Grob *grob, int count);
  static SCM identify_grob (Grob *grob);
  static Grob *retrieve_grob (SCM grob_id);
  static void store_grob (SCM grob_id, Grob *grob);
  static void set_tweaks (SCM tweaks);
};

#endif /* GROB_SELECTOR_HH */
