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
  */
struct Grob_info {
  Translator * origin_trans_;
  friend class Engraver;

  Grob * grob_;

public:
  Music * music_cause ();
  Link_array<Translator> origin_transes (Translator*) const;
  Grob_info (Grob*);
  Grob_info ();
};

#endif // STAFFELEMINFO_HH
