/*
  staff-margin-engraver.cc -- implement Staff_margin_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "translator.hh"
#include "engraver.hh"
#include "bar.hh"
#include "span-bar.hh"
#include "dimension-cache.hh"
#include "directional-element-interface.hh"
#include "timing-translator.hh"
#include "text-item.hh"
#include "side-position-interface.hh"
#include "bar-script-engraver.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"

/*
  TODO:

    * align over full score
    * text aligment: left, centred, right
    * merge with/derive from/add functionality to Bar_script_engraver
 */

/**
   Hang on left edge of staff to provide suppor for simple items.
 */
class Left_edge_item : public Item
{
public:
  VIRTUAL_COPY_CONS (Score_element);
};

/**
  put (instrument) text to left of line
 */
class Staff_margin_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Staff_margin_engraver ();

protected:
  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);

private:
  String type_;
  Text_item* text_p_;
  Left_edge_item* left_edge_p_;
  void create_text (SCM);
};

ADD_THIS_TRANSLATOR (Staff_margin_engraver);


Staff_margin_engraver::Staff_margin_engraver ()
{
  type_ = "margin";
  text_p_ = 0;
  left_edge_p_ = 0;
}

void
Staff_margin_engraver::acknowledge_element (Score_element_info info)
{
  SCM s = get_property ("instrument");
  
  if (now_mom () > Moment (0))
    s = get_property ("instr");

  if (gh_string_p (s))
    {
      if (Bar* b= dynamic_cast<Bar*> (info.elem_l_))
	{
	  create_text (s);
	  if (Span_bar* s= dynamic_cast<Span_bar*> (b))
	    {
	      assert (text_p_);
	      text_p_->set_parent (s, Y_AXIS);
	    }
	}
    }
}

void
Staff_margin_engraver::create_text (SCM text)
{
  if (!text_p_)
    {
      assert (!left_edge_p_);
      Left_edge_item* l = new Left_edge_item;
      
      l->set_elt_property ("breakable", SCM_BOOL_T);
      l->set_elt_property ("break-aligned", SCM_BOOL_T);

      announce_element (Score_element_info (l, 0));

      Staff_symbol_referencer_interface sl (l);
      sl.set_interface ();
      left_edge_p_ = l;
      
      Text_item* t = new Text_item;

      t->set_elt_property ("self-alignment-Y", gh_int2scm (0));
      t->add_offset_callback (Side_position_interface::aligned_on_self, Y_AXIS);

      t->set_parent (l, X_AXIS);
      t->set_parent (l, Y_AXIS);

      // 'just to be sure': see Clef_item::do_add_processing
      l->add_dependency (t);


      /*
	Hmm.
	In almost every score that uses "instrument" and "instr"
	we need two different paddings.
	Let's try one of those first:
	instrumentScriptPadding/instrScriptPadding
      */
      SCM s = get_property (String (now_mom () ? "instr" : "instrument")
			    + "ScriptPadding");
      if (!gh_number_p (s))
	s = get_property (type_ + "ScriptPadding");

      Real padding = 0;
      if (gh_number_p (s))
	padding = gh_scm2double (s);
  
      s = get_property (type_ + "ScriptTextStyle");
      if (gh_string_p (s))
	t->set_elt_property ("style", s);

      s = get_property (type_ + "ScriptHorizontalAlignment");
      /*
        Allow centred,
       */
      if (gh_number_p (s) && !gh_scm2int (s))
	{
	  t->set_elt_property ("self-alignment-X", gh_int2scm (0));
	  t->add_offset_callback (Side_position_interface::aligned_on_self, X_AXIS);
	  /*
%	    Centred is still broken, it always requires proper padding.
	   */
	  padding *= -1;
	  if (padding)
	    t->translate_axis (padding, X_AXIS);
	}
      /*
	left or right alignment.
      */
      else
	{
	  side_position (t).set_axis (X_AXIS);
	  side_position (t).add_support (l);
      
	  Direction d;
	  if (isdir_b (s))
	    d = to_dir (s);
	  /*
	    By default, align at left edge of Left_edge_item,
	    (which makes text right-aligned)
	    so that we don't collide with staff.
	  */
	  else
	    d = LEFT;
	  
	  directional_element (t).set (d);
	  padding *= -d;
	  if (padding)
	    t->set_elt_property ("padding", gh_double2scm (padding));
	}


      announce_element (Score_element_info (t, 0));
      text_p_ = t;
    }


  text_p_->set_elt_property ("text", text);
}

void
Staff_margin_engraver::do_pre_move_processing ()
{
  if (text_p_)
    {
      /*
	Let's not allow user settings for visibility function (yet).
	Although end-of-line would work, to some extent, we should
	make a properly ordered Right_edge_item, if that need arises.
       */
      text_p_->set_elt_property("visibility-lambda",
				scm_eval (ly_symbol2scm ("begin-of-line-visible")));
      typeset_element (text_p_);
      text_p_ = 0;
      assert (left_edge_p_);
      typeset_element (left_edge_p_);
      left_edge_p_ = 0;
    }
}
