/*   
  piano-pedal-engraver.cc --  implement Piano_pedal_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "engraver.hh"
#include "musical-request.hh"
#include "score-element.hh"
#include "item.hh"
#include "lookup.hh"
#include "lily-guile.hh"
#include "note-head.hh"
#include "stem.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"


/*
  TODO:
    sostenuto
    una-chorda / tre-corde
 */


/*
  Urg.
  This is almost text
  Problem is:
    * we have no kerning
    * symbols are at wrong place in font
*/

class Sustain_pedal : public Item
{
public:
  VIRTUAL_COPY_CONS (Score_element);

protected:
  virtual Molecule do_brew_molecule () const;
  virtual void after_line_breaking ();
};

void
Sustain_pedal::after_line_breaking ()
{
  Side_position_interface i (this);
  Direction d =  i.get_direction ();
  i.set_direction (d);
}

Molecule
Sustain_pedal::do_brew_molecule () const
{
  Molecule mol;
  SCM glyph = get_elt_property ("glyph");
  if (glyph == SCM_UNDEFINED)
    return mol;
  String text = ly_scm2string (glyph);

  for (int i = 0; i < text.length_i (); i++)
    {
      // leuke koor dump door tiepo, snapnie helemaal:
      //String idx = ("pedal-") + text[i];
      // urg, Byte* ??
      // braak: waarom vindt String het zo moeilijk om
      // String + char te doen?
      //String idx = "pedal-" + String (&text.byte_C ()[i], 1);
      String idx = String ("pedal-") + String (&text.byte_C ()[i], 1);
      Molecule m = lookup_l ()->afm_find (idx);
      if (m.empty_b ())
	continue;
      Real kern = 0;
      if (i)
	{
	  SCM s = scm_eval (gh_list (ly_symbol2scm ("pedal-kerning"),
				     ly_str02scm (String (&text.byte_C ()[i - 1], 1).ch_C ()),
				     ly_str02scm (String (&text.byte_C ()[i], 1).ch_C ()),
				     SCM_UNDEFINED));
	  if (gh_number_p (s))
	    {
	      Staff_symbol_referencer_interface st (this);
	      Real staff_space = st.staff_space ();
	      kern = gh_scm2double (s) * staff_space;
	    }
	}
      mol.add_at_edge (X_AXIS, RIGHT, m, kern);
    }
    
  return mol;
}

/**
   engrave Piano pedals
 */
class Piano_pedal_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Piano_pedal_engraver ();

protected:
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void acknowledge_element (Score_element_info);

private:
  Span_req* sustain_start_req_l_;
  Drul_array<Span_req*> sustain_req_l_drul_;
  Sustain_pedal* sustain_p_;
};

ADD_THIS_TRANSLATOR(Piano_pedal_engraver);

Piano_pedal_engraver::Piano_pedal_engraver ()
{
  sustain_p_ = 0;
  sustain_req_l_drul_[START] = 0;
  sustain_req_l_drul_[STOP] = 0;
  sustain_start_req_l_ = 0;
}

/*
  Urg: Code dup
  I'm a script
 */
void
Piano_pedal_engraver::acknowledge_element (Score_element_info i)
{
  if (sustain_p_)
    {
      if (Note_head* n = dynamic_cast<Note_head*> (i.elem_l_))
	{
	  Side_position_interface st (sustain_p_);
	  st.add_support (n);
	  if (st.get_axis( ) == X_AXIS
	      && !sustain_p_->parent_l (Y_AXIS))
	    sustain_p_->set_parent (n, Y_AXIS);
	}
      if (Stem* s = dynamic_cast<Stem*> (i.elem_l_))
	{
	  Side_position_interface st (sustain_p_);
	  st.add_support (s);
	}
    }
}

bool
Piano_pedal_engraver::do_try_music (Music *m)
{
  if (Span_req * s = dynamic_cast<Span_req*>(m))
    {
      if (s->span_type_str_ == "sustain")
	{
	  sustain_req_l_drul_[s->span_dir_] = s;
	  return true;
	}
    }
  return false;
}

void
Piano_pedal_engraver::do_process_music ()
{
  SCM s = SCM_UNDEFINED;
  if (sustain_req_l_drul_[STOP] && sustain_req_l_drul_[START])
    {
      if (!sustain_start_req_l_)
	{
	  sustain_req_l_drul_[STOP]->warning (_ ("can't find start of piano_pedal"));
	}
      else
	{
	  s = get_property ("stopStartSustain");
	  if (!gh_string_p (s))
	    s = ly_str02scm ("*Ped.");
	}
      sustain_start_req_l_ = sustain_req_l_drul_[START];
    }
  else if (sustain_req_l_drul_[STOP])
    {
      if (!sustain_start_req_l_)
	{
	  sustain_req_l_drul_[STOP]->warning (_ ("can't find start of piano_pedal"));
	}
      else
	{
	  s = get_property ("stopSustain");
	  if (!gh_string_p (s))
	    s = ly_str02scm ("*");
	}
      sustain_start_req_l_ = 0;
    }
  else if (sustain_req_l_drul_[START])
    {
      sustain_start_req_l_ = sustain_req_l_drul_[START];
      s = get_property ("startSustain");
      if (!gh_string_p (s))
	s = ly_str02scm ("Ped.");
    }
  if (s != SCM_UNDEFINED)
    {
      sustain_p_ = new Sustain_pedal;
      sustain_p_->set_elt_property ("glyph", s);


      Side_position_interface si (sustain_p_);
      si.set_axis (Y_AXIS);

      /* Hmm,
	 If set to empty, it can't be centred
	 Howto centre non-fat text?
	 sustain_p_->set_empty (X_AXIS);
      */
      sustain_p_->set_elt_property ("self-alignment-X", gh_int2scm (0));
      sustain_p_->add_offset_callback (Side_position_interface::aligned_on_self, X_AXIS);
      sustain_p_->add_offset_callback (Side_position_interface::centered_on_parent, X_AXIS);
      announce_element (Score_element_info (sustain_p_,
					    sustain_req_l_drul_[START]
					    ? sustain_req_l_drul_[START]
					    : sustain_req_l_drul_[STOP]));
    }
}

void
Piano_pedal_engraver::do_pre_move_processing ()
{
  if (sustain_p_)
    {
      side_position (sustain_p_).add_staff_support ();
      typeset_element (sustain_p_);
    }
  sustain_p_ = 0;
}

void
Piano_pedal_engraver::do_post_move_processing ()
{
  sustain_req_l_drul_[STOP] = 0;
  sustain_req_l_drul_[START] = 0;
}
