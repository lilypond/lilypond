/*
  span-bar-grav.cc -- implement Span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "lily-guile.hh"
#include "bar-line.hh"
#include "item.hh"
#include "span-bar.hh"
#include "engraver.hh"


/** 

  Make bars that span multiple "staves". Catch bars, and span a
  Span_bar over them if we find more than 2 bars.  Vertical alignment
  of staves changes the appearance of spanbars.  It is up to the
  aligner (Vertical_align_engraver, in this case, to add extra
  dependencies to the spanbars.

  */
class Span_bar_engraver : public Engraver
{
  Item * spanbar_;
  Link_array<Item> bars_;

public:
  TRANSLATOR_DECLARATIONS(Span_bar_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();

};


Span_bar_engraver::Span_bar_engraver ()
{
  spanbar_ =0;
}



void
Span_bar_engraver::acknowledge_grob (Grob_info i)
{
  int depth = i.origin_transes (this).size ();
  if (depth > 1
      && Bar_line::has_interface (i.grob_))
    {
      Item * it = dynamic_cast<Item*> (i.grob_);
      bars_.push (it);

      if (bars_.size () >= 2 && !spanbar_) 
	{
	  spanbar_ = new Item (get_property ("SpanBar"));

	  spanbar_->set_parent (bars_[0], X_AXIS);

	  announce_grob (spanbar_, SCM_EOL);
	}
    }
}
void
Span_bar_engraver::stop_translation_timestep ()
{
  if (spanbar_) 
    {
      for (int i=0; i < bars_.size () ; i++)
	Span_bar::add_bar (spanbar_,bars_[i]);

      SCM vissym =ly_symbol2scm ("break-visibility");
      SCM vis = bars_[0]->internal_get_grob_property (vissym);	  
      if (scm_equal_p (spanbar_->internal_get_grob_property (vissym), vis) != SCM_BOOL_T)
	spanbar_->internal_set_grob_property (vissym, vis);

      typeset_grob (spanbar_);
      spanbar_ =0;
    }
  bars_.set_size (0);
}







ENTER_DESCRIPTION(Span_bar_engraver,
/* descr */       "This engraver makes cross-staff barlines: It catches all normal
bar lines, and draws a single span-bar across them.",
/* creats*/       "SpanBar",
/* accepts */     "general-music",
/* acks  */      "bar-line-interface",
/* reads */       "",
/* write */       "");
