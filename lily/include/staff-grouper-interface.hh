/*
  staff-grouper-interface.hh -- declare Staff_grouper_interface

  source file of the GNU LilyPond music typesetter

  (c) 2009 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef STAFF_GROUPER_INTERFACE_HH
#define STAFF_GROUPER_INTERFACE_HH

#include "grob.hh"

class Staff_grouper_interface
{
public:
  DECLARE_GROB_INTERFACE ();

  static Grob *get_last_grob (Grob *);
};

#endif /* STAFF_GROUPER_INTERFACE_HH */
