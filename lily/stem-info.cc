/*
  stem-info.cc -- implement Stem_info

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

*/

#include "proto.hh"
#include "dimen.hh"
#include "misc.hh"
#include "debug.hh"
#include "atom.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"

Stem_info::Stem_info ()
{
}

Stem_info::Stem_info (Stem const *s)
{
  x = s->hpos_f();
  dir_ = s->dir_;
  beams_i_ =  0 >? (abs (s->flag_i_) - 2);

  /*
    [todo] 
    * get algorithm
    * runtime

    Breitkopf + H\"artel:
    miny_f_ = interline + #beams * interbeam
    ideal8 = 2 * interline + interbeam
    ideal16,32,64,128 = 1.5 * interline + #beams * interbeam

    * B\"arenreiter:
    miny_f_ = interline + #beams * interbeam
    ideal8,16 = 2 interline + #beams * interbeam
    ideal32,64,128 = 1.5 interline + #beams * interbeam
       
    */

  Real notehead_y = s->paper()->interline_f ();
  // huh? why do i need the / 2
  //    Real interbeam_f = s->paper()->interbeam_f ();
  Real interbeam_f = s->paper()->interbeam_f () / 2;
         
  /* well eh, huh?
     idealy_f_  = dir_ * s->stem_begin_f() + beams_i_ * interbeam_f; 
     if (beams_i_ < 3)
     idealy_f_ += 2 * interline_f;
     else
     idealy_f_ += 1.5 * interline_f;
     */

  idealy_f_  = dir_ * s->stem_end_f();

  miny_f_ = dir_ * s->stem_begin_f() + notehead_y + beams_i_ * interbeam_f;

  idealy_f_ =  miny_f_ >? idealy_f_;
  //    assert (miny_f_ <= idealy_f_);
}

