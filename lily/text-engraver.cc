/*   
  text-engraver.cc --  implement Text_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "engraver.hh"
#include "side-position-interface.hh"
#include "item.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "rhythmic-head.hh"


/**
   typeset directions that are  plain text.
 */
class Text_engraver : public Engraver
{
  Link_array<Text_script_req> reqs_;
  Link_array<Item> texts_;
public:
  TRANSLATOR_DECLARATIONS(Text_engraver);
protected:
  virtual bool try_music (Music* m);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void create_grobs ();
  virtual void acknowledge_grob (Grob_info);
};

bool
Text_engraver::try_music (Music *m)
{
  if (dynamic_cast<Text_script_req*> (m)
      && m->get_mus_property ("text-type") != ly_symbol2scm ("dynamic"))
    {
      reqs_.push (dynamic_cast<Text_script_req*> (m));
      return true;
    }
  return false;
}

void
Text_engraver::acknowledge_grob (Grob_info inf)
{
  if (Rhythmic_head::has_interface (inf.grob_l_))
    {
      for (int i=0; i < texts_.size (); i++)
	{
	  Grob*t = texts_[i];
	  Side_position_interface::add_support (t,inf.grob_l_);

	  /*
	    ugh.
	   */
	  if (Side_position_interface::get_axis (t) == X_AXIS
	      && !t->get_parent (Y_AXIS))
	    t->set_parent (inf.grob_l_, Y_AXIS);
	  else if (Side_position_interface::get_axis (t) == Y_AXIS
	      && !t->get_parent (X_AXIS))
	    t->set_parent (inf.grob_l_, X_AXIS);
	}
    }
  
  if (Stem::has_interface (inf.grob_l_))
    {
      for (int i=0; i < texts_.size (); i++)
	{
	  Side_position_interface::add_support (texts_[i],inf.grob_l_);
	}
    }
}

void
Text_engraver::create_grobs ()
{
  if (texts_.size ())
    return;
  for (int i=0; i < reqs_.size (); i++)
    {
      Text_script_req * r = reqs_[i];
      
      // URG: Text vs TextScript
      String basic = "TextScript";

      if (r->get_mus_property ("text-type") == ly_symbol2scm ("finger"))
	{
	  basic = "Fingering";
	}

      Item *text = new Item (get_property (basic.ch_C ()));

      /*
	FIXME -> need to use basic props.
       */
      SCM axisprop = get_property ("scriptHorizontal");
      
      Axis ax = to_boolean (axisprop) ? X_AXIS : Y_AXIS;
      Side_position_interface::set_axis (text, ax);

#if 0
      if (r->style_str_ == "finger" && ax == Y_AXIS)
	{
	  /*
	    nicely center the scripts.
	   */ 
	  text->add_offset_callback (Side_position_interface::aligned_on_self_proc, X_AXIS);
	  text->add_offset_callback (Side_position_interface::centered_on_parent_proc, X_AXIS);
	}
#endif
      

      
      /*
	make sure they're in order by adding i to the priority field.
	*/
      text->set_grob_property ("script-priority",
			      gh_int2scm (200 + i));

      if (r->get_direction ())
	Side_position_interface::set_direction (text, r->get_direction ());
      
      text->set_grob_property ("text", r->get_mus_property ("text"));
      
      SCM nonempty = get_property ("textNonEmpty");
      if (to_boolean (nonempty))
	/*
	  empty text: signal that no rods should be applied.  
	 */
	text->set_grob_property ("no-spacing-rods" , SCM_BOOL_F);
		
      announce_grob (text, r);
      texts_.push (text);
    }
}

void
Text_engraver::stop_translation_timestep ()
{
  for (int i=0; i < texts_.size (); i++)
    {
      Item *ti = texts_[i];
      if (!to_boolean (get_property ("scriptHorizontal")))
	Side_position_interface::add_staff_support (ti);
      typeset_grob (ti);
    }
  texts_.clear ();
}

void
Text_engraver::start_translation_timestep ()
{
  reqs_.clear ();
}


Text_engraver::Text_engraver(){}

ENTER_DESCRIPTION(Text_engraver,
/* descr */       "Create text-scripts",
/* creats*/       "TextScript",
/* acks  */       "rhythmic-head-interface stem-interface",
/* reads */       "scriptHorizontal textNonEmpty",
/* write */       "");
