/*   
  audio-item-info.hh -- declare Audio_item_info
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef AUDIO_ITEM_INFO_HH
#define AUDIO_ITEM_INFO_HH

#include "lily-proto.hh"
#include "parray.hh"

/**
  Data container for broadcasts 
  */
struct Audio_element_info {
  Audio_element * elem_l_;
  Music *req_l_;
  Translator *  origin_trans_l_;
  Link_array<Translator> origin_trans_l_arr (Translator*) const;  

  Audio_element_info (Audio_element*, Music*);
  Audio_element_info();
};


#endif
