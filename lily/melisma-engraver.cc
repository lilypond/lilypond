/*   
  melisma-engraver.cc --  implement Melisma_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "musical-request.hh"
#include "score-element.hh"

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
      SCM beam (get_property ("beamMelismaBusy"));
      
      if( (to_boolean (plain))
	  || (to_boolean (slur))
	  || (to_boolean (tie))
	  || (to_boolean (beam))) {

	Score_element * melisma_p = new Score_element (get_property ("basicMelismaProperties"));
	announce_element (melisma_p, m);

	return true;
      }
    }
  return false;
}
