/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  TODO: This is way too hairy
*/

#include "stem.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "note-head.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "misc.hh"
#include "beam.hh"
#include "rest.hh"

void
Stem::set_direction (Direction d)
{
  if  (!dir_)
    warning ("Stem direction set already!");

  dir_ = d;

  /*
    todo
  */
}

Stem::Stem ()
{
  beams_i_drul_[LEFT] = beams_i_drul_[RIGHT] = -1;
  yextent_drul_[DOWN] = yextent_drul_[UP] = 0;
  flag_i_ = 2;
  dir_ = CENTER;
  beam_l_ = 0;
}

Interval_t<int>
Stem::head_positions () const
{
  /* 
    Mysterious FreeBSD fix by John Galbraith.  Somehow, the empty intervals 
    trigger FP exceptions on FreeBSD.  Fix: do not return infinity 

   */
  if (!head_l_arr_.size ())
    {
      return Interval_t<int> (100,-100);	
    }

  Interval_t<int> r;
  for (int i =0; i < head_l_arr_.size (); i++)
    {
      int p = head_l_arr_[i]->position_i_;
      r[BIGGER] = r[BIGGER] >? p;
      r[SMALLER] = r[SMALLER] <? p;
    }
  return r;
}

void
Stem::do_print () const
{
#ifndef NPRINT
  DOUT << "flag "<< flag_i_;
  if (beam_l_)
    DOUT << "beamed";
#endif
}

Real
Stem::stem_length_f () const
{
  return yextent_drul_[UP]-yextent_drul_[DOWN] ;
}

Real
Stem::stem_begin_f () const
{
  return yextent_drul_[Direction(-dir_)];
}

Real
Stem::chord_start_f () const
{
  return head_positions()[dir_] * staff_line_leading_f ()/2.0;
}

Real
Stem::stem_end_f () const
{
  return yextent_drul_[dir_];
}

void
Stem::set_stemend (Real se)
{
  // todo: margins
  if (dir_ && dir_ * head_positions()[dir_] >= se*dir_)
    warning (_ ("weird stem size; check for narrow beams"));

  
  yextent_drul_[dir_]  =  se;
  yextent_drul_[Direction(-dir_)] = head_positions()[-dir_];
}

int
Stem::type_i () const
{
  return head_l_arr_[0]->balltype_i_;
}

void
Stem::add_head (Rhythmic_head *n)
{
  n->stem_l_ = this;
  n->add_dependency (this);	// ?
  if (Note_head *nh = dynamic_cast<Note_head *> (n))
    {
      head_l_arr_.push (nh);
    }
  else if (Rest *r = dynamic_cast<Rest *> (n))
    {
      rest_l_arr_.push (r);
    }
}

bool
Stem::invisible_b () const
{
  return (!head_l_arr_.size () ||
    head_l_arr_[0]->balltype_i_ <= 0);
}

int
Stem::get_center_distance (Direction d) const
{
  int staff_center = 0;
  int distance = d*(head_positions()[d] - staff_center);
  return distance >? 0;
}

Direction
Stem::get_default_dir () const
{
  int du = get_center_distance (UP);
  int dd = get_center_distance (DOWN);

  if (sign (dd - du))
    return Direction (sign (dd -du));

  return Direction (int(paper_l ()->get_var ("stem_default_neutral_direction")));
}

Direction
Stem::get_dir () const
{
  return dir_;
}


void
Stem::set_default_stemlen ()
{
  Real length_f = 0.;
  SCM scm_len = get_elt_property(length_scm_sym);
  if (scm_len != SCM_BOOL_F)
    {
      length_f = gh_scm2double (SCM_CDR(scm_len));
    }
  else
    length_f = paper_l ()->get_var ("stem_length0");

  bool grace_b = get_elt_property (grace_scm_sym) != SCM_BOOL_F;
  String type_str = grace_b ? "grace_" : "";

  Real shorten_f = paper_l ()->get_var (type_str + "forced_stem_shorten0");

  if (!dir_)
    dir_ = get_default_dir ();

  /* 
    stems in unnatural (forced) direction should be shortened, 
    according to [Roush & Gourlay]
   */
  if (((int)chord_start_f ())
      && (dir_ != get_default_dir ()))
    length_f -= shorten_f;

  if (flag_i_ >= 5)
    length_f += 2.0;
  if (flag_i_ >= 6)
    length_f += 1.0;
  
  set_stemend ((dir_ > 0) ? head_positions()[BIGGER] + length_f:
	       head_positions()[SMALLER] - length_f);

  if (!grace_b && (dir_ * stem_end_f () < 0))
    set_stemend (0);
}

//xxx
void
Stem::set_default_extents ()
{
  if (!stem_length_f ())
    set_default_stemlen ();

}

void
Stem::set_noteheads ()
{
  if (!head_l_arr_.size ())
    return;
  head_l_arr_.sort (Note_head::compare);
  if (dir_ < 0)
    head_l_arr_.reverse ();

  Note_head * beginhead =   head_l_arr_[0];
  beginhead->set_elt_property (extremal_scm_sym, SCM_BOOL_T);
  if  (beginhead !=   head_l_arr_.top ())
    head_l_arr_.top ()->set_elt_property (extremal_scm_sym, SCM_BOOL_T);
  
  int parity=1;
  int lastpos = beginhead->position_i_;
  for (int i=1; i < head_l_arr_.size (); i ++)
    {
      int dy =abs (lastpos- head_l_arr_[i]->position_i_);

      if (dy <= 1)
	{
	  if (parity)
	    head_l_arr_[i]->flip_around_stem (dir_);
	  parity = !parity;
	}
      else
	parity = 1;
      lastpos = head_l_arr_[i]->position_i_;
    }
}

void
Stem::do_pre_processing ()
{
  if (yextent_drul_[DOWN]== yextent_drul_[UP])
    set_default_extents ();
  set_noteheads ();

  if (invisible_b ())
    {
      set_elt_property (transparent_scm_sym, SCM_BOOL_T);
    }
  set_empty (invisible_b ());
  set_spacing_hints ();
}



/**
   set stem directions for hinting the optical spacing correction.

   Modifies DIR_LIST property of the Stem's Score_column

   TODO: more advanced: supply height of noteheads as well, for more advanced spacing possibilities
 */
void
Stem::set_spacing_hints () 
{
  if (!invisible_b ())
    {
      SCM scmdir  = gh_int2scm (dir_);
      SCM dirlist = column_l ()->get_elt_property (dir_list_scm_sym);
      if (dirlist == SCM_BOOL_F)
	dirlist = SCM_EOL;
      else
	dirlist = SCM_CDR (dirlist);

      if (scm_sloppy_memq (scmdir, dirlist) == SCM_EOL)
	{
	  dirlist = gh_cons (scmdir, dirlist);
	  column_l ()->set_elt_property (dir_list_scm_sym, dirlist);
	}
    }
}

Molecule
Stem::flag () const
{
  String style;
  SCM st = get_elt_property (style_scm_sym);
  if ( st != SCM_BOOL_F)
    {
      st = SCM_CDR(st);
      style = ly_scm2string (st);
    }

  char c = (dir_ == UP) ? 'u' : 'd';
  Molecule m = lookup_l ()->afm_find (String ("flags-") + to_str (c) + 
				      to_str (flag_i_));
  if (!style.empty_b ())
    m.add_molecule(lookup_l ()->afm_find (String ("flags-") + to_str (c) + style));
  return m;
}

Interval
Stem::do_width () const
{
  Interval r (0, 0);
  if (beam_l_ || abs (flag_i_) <= 2)
    ;	// TODO!
  else
    {
      r = flag ().dim_.x ();
      r += note_delta_f ();
    }
  return r;
}




const Real ANGLE = 20* (2.0*M_PI/360.0); // ugh!

Molecule*
Stem::do_brew_molecule_p () const
{
  Molecule *mol_p =new Molecule;
  Drul_array<Real> stem_y = yextent_drul_;
  Real dy = staff_line_leading_f ()/2.0;

  Real head_wid = 0;
  if (head_l_arr_.size ())
    head_wid = head_l_arr_[0]->extent (X_AXIS).length ();
  stem_y[Direction(-dir_)] += dir_ * head_wid * tan(ANGLE)/(2*dy);
  
  if (!invisible_b ())
    {
      Real stem_width = paper_l ()->get_var ("stemthickness");
      Molecule ss =lookup_l ()->filledbox (Box (Interval (-stem_width/2, stem_width/2),
						 Interval (stem_y[DOWN]*dy, stem_y[UP]*dy)));
      mol_p->add_molecule (ss);
    }

  if (!beam_l_ && abs (flag_i_) > 2)
    {
      Molecule fl = flag ();
      fl.translate_axis(stem_y[dir_]*dy, Y_AXIS);
      mol_p->add_molecule (fl);
    }

  if (head_l_arr_.size())
    {
      mol_p->translate_axis (note_delta_f (), X_AXIS);
    }
  return mol_p;
}

Real
Stem::note_delta_f () const
{
  Real r=0;
  if (head_l_arr_.size())
    {
      Interval head_wid(0,  head_l_arr_[0]->extent (X_AXIS).length ());
      Real rule_thick(paper_l ()->rule_thickness ());
      Interval stem_wid(-rule_thick/2, rule_thick/2);
      if (dir_ == CENTER)
	r = head_wid.center ();
      else
	r = head_wid[dir_] - stem_wid[dir_];
    }
  return r;
}

Real
Stem::hpos_f () const
{
  return note_delta_f () + Item::hpos_f ();
}

void
Stem::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  if (Note_head*h=dynamic_cast<Note_head*> (o))
    head_l_arr_.substitute (h, dynamic_cast<Note_head*>(n));
  if (Rest *r=dynamic_cast<Rest*> (o))
    rest_l_arr_.substitute (r, dynamic_cast<Rest*>(n));
  if (Beam* b = dynamic_cast<Beam*> (o))
    {
      if (b == beam_l_) 
	beam_l_ = dynamic_cast<Beam*> (n);
    }
  Staff_symbol_referencer::do_substitute_element_pointer (o,n);
}
