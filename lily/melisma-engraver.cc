/*   
  melisma-engraver.cc --  implement Melisma_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "musical-request.hh"

/**
   Signal existence of melismas.
 */
class Melisma_engraver:public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  bool do_try_music (Music *);
};

ADD_THIS_TRANSLATOR(Melisma_engraver);

bool
Melisma_engraver::do_try_music (Music *m ) 
{
  if (dynamic_cast<Melisma_playing_req*>(m))
    {
      Scalar plain (get_property ("melismaBusy", 0));
      Scalar slur (get_property ("slurMelismaBusy", 0));
      Scalar tie (get_property ("tieMelismaBusy", 0));
      return plain.to_bool () || slur.to_bool () || tie.to_bool ();
    }
  return false;
}
