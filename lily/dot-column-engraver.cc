/*   
  dot-column-engraver.cc -- implement Dot_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "rhythmic-head.hh"
#include "dot-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "stem.hh"

class Dot_column_engraver : public Engraver
{
  Grob *dotcol_ ;
  Grob * stem_;
  Link_array<Item> heads_;
public:
  TRANSLATOR_DECLARATIONS(
  Dot_column_engraver );
  
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();  
};


Dot_column_engraver::Dot_column_engraver ()
{
  dotcol_ =0;
  stem_ = 0;
}

void
Dot_column_engraver::stop_translation_timestep ()
{
  if (dotcol_)
    {

      /*
	Add the stem to the support so dots stay clear of flags.

	See [Ross, p 171]
       */
      if (stem_)
	dotcol_->set_grob_property ("stem", stem_->self_scm ());
      
      typeset_grob (dotcol_);
      dotcol_ =0;
    }
  heads_.clear ();
  stem_ =0;
}

void
Dot_column_engraver::acknowledge_grob (Grob_info info)
{
  Grob *d = unsmob_grob (info.grob_->get_grob_property ("dot"));
  if (d)
    {
      if (!dotcol_)
	{
	  dotcol_ = new Item (get_property ("DotColumn"));
	  announce_grob(dotcol_, SCM_EOL);
	}

      Dot_column::add_head (dotcol_, info.grob_);
    }
  else if (Stem::has_interface (info.grob_))
    {
      stem_ = info.grob_;
    }
}




ENTER_DESCRIPTION(Dot_column_engraver,
/* descr */       " Engraves dots on dotted notes shifted to the right of the note.\n"
"If omitted, then dots appear on top of the notes.",
/* creats*/       "DotColumn",
/* accepts */     "",
/* acks  */      "rhythmic-head-interface dot-column-interface stem-interface",
/* reads */       "",
/* write */       "");
