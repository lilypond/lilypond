/*
  encompass-info.cc -- implement Encompass_info

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "proto.hh"
#include "stem.hh"
#include "note-column.hh"
#include "paper-def.hh"
#include "encompass-info.hh"

Encompass_info::Encompass_info ()
{
  assert (0);
}

Encompass_info::Encompass_info (Note_column const* note, Direction dir)
{
  Paper_def* paper = note->paper ();
  Real interline = paper->interline_f ();
  // UGH
  Real notewidth = paper->note_width () * 0.8;
  Real internote = interline / 2;

  Stem* stem = note->stem_l_;
  /* 
    set o_.x () to middle of notehead or on the exact position of stem,
    according to slur direction
   */
  o_.x () = stem->hpos_f ();

  /*
     stem->dir == dir
                      ________
           |   |     /        \
          x|  x|       |x  |x
        \________/     |   |

   */

  if (stem->dir_ != dir)
    o_.x () -= 0.5 * notewidth * stem->dir_;

  o_.y () = stem->height ()[dir];
  /*
   leave a gap: slur mustn't touch head/stem
   */
  o_.y () += 2.5 * internote * dir;

  if (stem->dir_ != dir)
    o_.y () += 1.0 * internote * dir;
}

