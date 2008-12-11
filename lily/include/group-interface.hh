/*
  group-interface.hh -- declare Group_interface

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef GROUP_INTERFACE_HH
#define GROUP_INTERFACE_HH

#include "grob.hh"
#include "std-string.hh"
/**
   Look at Score element ELT as thing which has a list property called
   NAME_. Normally the list would contain Grobs, but
   sometimes it can be different things.

   todo: reename as list_interface?
*/

struct Group_interface
{
public:
  static int count (Grob *, SCM);
  static void add_thing (Grob *, SCM, SCM);
};

#endif /* GROUP_INTERFACE_HH */

