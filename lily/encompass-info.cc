/*
  encompass-info.cc -- implement Encompass_info

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>

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
  Real notewidth = paper->note_width ();
  Real internote = interline / 2;

  Stem* stem = note->stem_l_;
  /* 
    set o_.x () to middle of notehead or on eo_.x ()act o_.x () position of stem,
    according to slur direction
       */
  o_.x () = stem->hpos_f ();

  if (stem->dir_ != dir)
    {
      o_.x () += 0.5 * notewidth;
      // ugh
      if (dir == DOWN)
	o_.x () -= 0.5 * notewidth;
      else
	o_.x () += 0.5 * notewidth;
    }
  else if (stem->dir_ == UP)
    o_.x () += 1.0 * notewidth;

//  o_.x () -= left_o_.x ();

  o_.y () = stem->height ()[dir];

  /*
    leave a gap: slur mustn't touch head/stem
   */
  if (stem->dir_ != dir)
    o_.y () += 3.0 * internote * dir;
  else
    o_.y () += 2.0 * internote * dir;

  // ugh
  if (dir == DOWN)
    o_.y () += 1.5 * internote * dir;

//  o_.y () -= left_o_.y ();
}

