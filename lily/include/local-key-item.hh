/*
  local-key-item.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
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
  bool natural_b_;

  Local_key_cautionary_tuple ()
  {
    cautionary_b_ = false;
    natural_b_ = false;
  }
  static int compare (Local_key_cautionary_tuple const&s1, Local_key_cautionary_tuple const&s2)
  {
    return Musical_pitch::compare (s1.pitch_, s2.pitch_);
  }
};

/**
  Accidentals which can be different for each octave.

  
  TODO

  Make an item for each accidental separately, and make a
  Accidental_column to group them.
  
 */

class Local_key_item : public Item
{
  Array<Local_key_cautionary_tuple> accidental_arr_;

  Molecule accidental (int,bool,bool) const;
public:

  void add_pitch (Musical_pitch, bool cautionary, bool natural);
protected:
  virtual void do_pre_processing();
  virtual Molecule* do_brew_molecule_p() const;
};
#endif // LOCALKEYITEM_HH

