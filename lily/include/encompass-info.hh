/*
  encompass-info.hh -- declare Encompass_info

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#ifndef ENCOMPASS_INFO_HH
#define ENCOMPASS_INFO_HH

#include "lily-proto.hh"
#include "direction.hh"
#include "offset.hh"

struct Encompass_info
{
  Encompass_info ();
  Encompass_info (Note_column const*, Direction);

  Offset o_;
  Real interstaff_f_;
};

#endif // ENCOMPASS_INFO_HH
