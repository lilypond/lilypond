/*   
  melisma-engraver.cc --  implement Melisma_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "musical-request.hh"
#include "grob.hh"
#include "translator-group.hh"

/**
   Signal existence of melismas.
 */
class Melisma_engraver:public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  bool try_music (Music *);
};

ADD_THIS_TRANSLATOR(Melisma_engraver);

bool
Melisma_engraver::try_music (Music *m ) 
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

	daddy_trans_l_->set_property("melismaEngraverBusy",SCM_BOOL_T);
	return true;
      }
    }
  daddy_trans_l_->set_property("melismaEngraverBusy",SCM_BOOL_F);
  return false;
}
