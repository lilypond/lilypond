/*
  grob-info.hh -- declare Grob_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFFELEMINFO_HH
#define STAFFELEMINFO_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "parray.hh"

/**
  Data container for broadcasts.

  TODO: Store this in element info! 
  */
struct Grob_info {
  Translator * origin_trans_l_;
  friend class Engraver;

  Grob * grob_l_;

  /*
    Notice that CAUSE is not GC protected ; this might be a cause for
    GC errors if you don't use music or grobs as a cause.
  */
  SCM cause_;
public:
  Music * music_cause ();
  Link_array<Translator> origin_trans_l_arr (Translator*) const;
  Grob_info (Grob*, SCM);
  Grob_info ();
};


#endif // STAFFELEMINFO_HH
