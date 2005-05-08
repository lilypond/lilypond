/*   
  output-property-engraver.cc -- implement Output_property_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "grob.hh"
#include "context.hh"

class Output_property_engraver : public Engraver
{
TRANSLATOR_DECLARATIONS (Output_property_engraver);
protected:
  Link_array<Music> props_;

  virtual void stop_translation_timestep ();
  virtual void acknowledge_grob (Grob_info);
  virtual bool try_music (Music*);
};


bool
Output_property_engraver::try_music (Music* m)
{
  if (m->is_mus_type ("layout-instruction"))
    {
      props_.push (m);
      return true;
    }
  return false;
}

void
Output_property_engraver::acknowledge_grob (Grob_info inf)
{
  for (int i = props_.size (); i--;)
    {
      Music * o = props_[i];
      SCM pred = o->get_property ("predicate");



      if (ly_c_procedure_p (pred))
	{
	  /*
	    should typecheck pred. 
	  */
	  SCM result = scm_call_1 (pred, inf.grob_->self_scm ());
	  if (to_boolean (result))
	    {
	      SCM sym = o->get_property ("grob-property");
	      SCM val = o->get_property ("grob-value");
	      inf.grob_->internal_set_property (sym, val);
	    }
	}
      else
	{
	  Context * d = inf.context ();
	  SCM proc = o->get_property ("procedure");
	  scm_call_3 (proc,
		      inf.grob_->self_scm (),
		      d->self_scm (), 
		      context ()->self_scm ());
	}
    }
}

void
Output_property_engraver::stop_translation_timestep ()
{
  props_.clear ();
}

Output_property_engraver::Output_property_engraver ()
{
}

ADD_TRANSLATOR (Output_property_engraver,
/* descr */       "Interpret Music of Output_property type, and apply a function "
" to any Graphic objects that satisfies the predicate.",
/* creats*/       "",
/* accepts */     "layout-instruction",
/* acks  */       "grob-interface",
/* reads */       "",
/* write */       "");
