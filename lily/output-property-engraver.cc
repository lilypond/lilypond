/*   
  output-property-engraver.cc --  implement Output_property_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grob.hh"
#include "output-property-music-iterator.hh"

class Output_property_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
protected:

  /*
    should do this with \once and \push ?


      \property Voice.outputProperties \push #pred = #modifier

      where both MODIFIER and PRED are functions taking a
      grob.
      
   */

  
  Link_array<Music> props_;

  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
};


bool
Output_property_engraver::try_music (Music* m)
{
  if (m->get_mus_property ("iterator-ctor") ==
      Output_property_music_iterator::constructor_cxx_function)
    {
      props_.push (m);
      return true;
    }
  return false;
}

void
Output_property_engraver::acknowledge_grob (Grob_info inf)
{
  for (int i=props_.size (); i--;)
    {
      Music * o = props_[i];
      SCM pred = o->get_mus_property ("predicate");
      
      /*
	should typecheck pred. 
       */
      SCM result=gh_apply (pred,
			   gh_list (inf.elem_l_->self_scm (), SCM_UNDEFINED));
      if (to_boolean (result))
	{
	  SCM sym = o->get_mus_property ("grob-property");
	  SCM val = o->get_mus_property ("grob-value");
	  inf.elem_l_->set_grob_property (sym, val);
	}
    }
}

void
Output_property_engraver::stop_translation_timestep ()
{
  props_.clear ();
}

ADD_THIS_TRANSLATOR (Output_property_engraver);
