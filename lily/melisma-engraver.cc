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
      SCM plain (get_property ("melismaBusy"));
      SCM slur (get_property ("slurMelismaBusy"));
      SCM tie (get_property ("tieMelismaBusy"));
      return (to_boolean (plain))
	|| (to_boolean (slur))
	|| (to_boolean (tie));
    }
  return false;
}
