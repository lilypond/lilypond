/*
  grob-info.hh -- declare Grob_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFFELEMINFO_HH
#define STAFFELEMINFO_HH

#include "lily-proto.hh"
#include "parray.hh"

/**
  Data container for broadcasts.

  TODO: Store this in element info! 
  */
struct Grob_info {
  Translator * origin_trans_l_;
  friend class Engraver;
public:
  Link_array<Translator> origin_trans_l_arr (Translator*) const;

  Grob * grob_l_;
  Music *req_l_;
  
  Grob_info (Grob*, Music*);
  Grob_info ();
};


#endif // STAFFELEMINFO_HH
