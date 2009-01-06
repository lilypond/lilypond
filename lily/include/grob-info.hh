/*
  grob-info.hh -- declare Grob_info

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef STAFFELEMINFO_HH
#define STAFFELEMINFO_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "std-vector.hh"

/*
  Data container for broadcasts.
*/
class Grob_info
{
  Translator *origin_trans_;
  Grob *grob_;
  Direction start_end_;
  
  friend class Engraver;
public:
  Direction start_end () const { return start_end_; }
  Grob *grob () const { return grob_; }
  Translator *origin_translator () const { return origin_trans_; }

  Context *context () const;
  Stream_event *event_cause () const;
  Stream_event *ultimate_event_cause () const;
  vector<Context*> origin_contexts (Translator *) const;
  Grob_info (Translator *, Grob *);
  Grob_info ();

  Item *item () const;
  Spanner *spanner () const;
};

#endif // STAFFELEMINFO_HH
