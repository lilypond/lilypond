/*
  local-key-item.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef LOCALKEYITEM_HH
#define LOCALKEYITEM_HH
#include "item.hh"
#include "array.hh"
#include "musical-pitch.hh"

struct Local_key_cautionary_tuple
{
  Musical_pitch pitch_;
  bool cautionary_b_;

  Local_key_cautionary_tuple ()
    {
      cautionary_b_ = false;
    }
  static int compare (Local_key_cautionary_tuple const&s1, Local_key_cautionary_tuple const&s2)
    {
      return Musical_pitch::compare (s1.pitch_, s2.pitch_);
    }
};

/**
  Accidentals which can be different for each octave.

  TODO:
  update item if Items are removed

  TODO
  
  figure out private/public
  
 */
class Local_key_item : public Item {
  Array<Local_key_cautionary_tuple> accidental_arr_;
  Link_array<Item> support_items_;
public:
  int c0_position_i_;
  Local_key_item ();
  void add_support (Item*);
  void add_pitch (Musical_pitch, bool cautionary);
protected:
  virtual void do_pre_processing();    
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual Molecule* do_brew_molecule_p() const;
};
#endif // LOCALKEYITEM_HH

