/*
  score-elem-info.hh -- declare Score_elem_info

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFFELEMINFO_HH
#define STAFFELEMINFO_HH

#include "scalar.hh"
#include "lily-proto.hh"
#include "varray.hh"

/**
  Data container for broadcasts 
  */
struct Score_elem_info {
    Score_elem * elem_l_;
    Request*req_l_;
    Array<Engraver*> origin_grav_l_arr_;

    /* *** */
    Score_elem_info(Score_elem*, Request*);
    Score_elem_info();
};


struct Feature {
    Scalar type_; 
    Scalar value_;
};
#endif // STAFFELEMINFO_HH
