/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>

  TODO: This is way too hairy
*/

#include "stem.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper-def.hh"
#include "note-head.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "p-col.hh"
#include "misc.hh"
#include "beam.hh"
#include "rest.hh"

const int STEMLEN=7;

IMPLEMENT_IS_TYPE_B1 (Stem,Item);

Stem::Stem () 
{
  /*
    TODO: staff-size
   */
  abbrev_flag_i_ = 0;
  beam_l_ = 0;
  beams_left_i_ = 0;
  beams_right_i_ = 0;

  yextent_drul_[DOWN] = yextent_drul_[UP] = 0;
  flag_i_ = 2;
  dir_ = CENTER;
  stem_xdir_ = LEFT;
  staff_size_i_ = 8;

  beam_gap_i_ = 0;
}

int
Stem::min_head_i () const
{
  int m = 1000;
  for (int i =0; i < head_l_arr_.size (); i++)
    m = m <? head_l_arr_[i]->position_i_;
  return m;
}

int
Stem::max_head_i () const
{
  int m = -1000;
  for (int i =0; i < head_l_arr_.size (); i++)
    m = m >? head_l_arr_[i]->position_i_;
  return m;
  
}

void
Stem::do_print () const
{
#ifndef NPRINT
  DOUT << "flag "<< flag_i_ << "abbrev_flag_i_" << abbrev_flag_i_;
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
Stem::stem_start_f () const
{
  return (dir_ < 0)? yextent_drul_[UP] : yextent_drul_[DOWN];
}

Real
Stem::stem_end_f () const
{
  return (dir_ < 0)? yextent_drul_[DOWN] : yextent_drul_[UP];
}


void
Stem::set_stemend (Real se)
{
  // todo: margins
  if (!  ((dir_ > 0 && se >= max_head_i ()) || 
	  (se <= min_head_i () && dir_ <0)))	
    warning ("Weird stem size; check for narrow beams");

  yextent_drul_[UP]  = (dir_ < 0) ? max_head_i () : se;
  yextent_drul_[DOWN]  = (dir_ < 0) ? se  : min_head_i ();
}

int
Stem::type_i () const
{
  return head_l_arr_[0]->balltype_i_;
}

void
Stem::add (Rhythmic_head *n)
{
  n->add_dependency (this);	// ?
  if (n->is_type_b (Note_head::static_name ()))
    {
      head_l_arr_.push ((Note_head*)n);
    }
  else if (n->is_type_b (Rest::static_name ()))
    {
      rest_l_arr_.push ((Rest*)n);
    }    
}

bool
Stem::invisible_b () const
{
  
  return (!head_l_arr_.size () || 
    head_l_arr_[0]->balltype_i_ <= 0);

}

// if dir_ is set we return fake values.
int
Stem::get_center_distance_from_top ()
{
  if (dir_)
    return (dir_ > 0) ? 0 : 1;

  int staff_center = 0;
  int max = max_head_i () - staff_center;
  return max >? 0;
}

// if dir_ is set we return fake values.
int
Stem::get_center_distance_from_bottom ()
{
  if (dir_)
    return (dir_ > 0) ? 1 : 0;

  int staff_center = 0;
  int min = staff_center - min_head_i ();
  return min >? 0;
}

Direction
Stem::get_default_dir ()
{
  if (dir_)
    return dir_;
  return (get_center_distance_from_top () >=
	  get_center_distance_from_bottom ()) ? 
    (Direction)-1 : (Direction)1;
}


void
Stem::set_default_dir ()
{
  dir_ = get_default_dir ();
}

void
Stem::set_default_stemlen ()
{
  if (!dir_)
    set_default_dir ();

   
  Real dy = paper ()->interbeam_f ();
  Real len = STEMLEN;
  // ugh, should get nice *rule* for this
  if (abbrev_flag_i_ > 1)
    len += (abbrev_flag_i_ - 1)* dy / 2;
  set_stemend ((dir_ > 0) ? max_head_i () + len :
	       min_head_i () - len);
  

  if (dir_ * stem_end_f () < 0)
    {
      set_stemend (0);
    }
 
  
}

void
Stem::set_default_extents ()
{
  if (!stem_length_f ())
    set_default_stemlen ();

  set_stemend ((dir_< 0) ? 
	       max_head_i ()-stem_length_f (): min_head_i () + stem_length_f ());
  
  if (dir_ == UP)
    stem_xdir_ = RIGHT;
  if (invisible_b ())
    stem_xdir_ = CENTER;
}

/*
  TODO
  
  move into note_column.cc

  */
void
Stem::set_noteheads ()
{
  if (!head_l_arr_.size ())
    return;
  head_l_arr_.sort (Note_head::compare);
  if (dir_ < 0) 
    head_l_arr_.reverse ();
  
  head_l_arr_[0]->extremal_i_ = -1;
  head_l_arr_.top ()->extremal_i_ = 1;
  int parity=1;
  int lastpos = head_l_arr_[0]->position_i_;
  for (int i=1; i < head_l_arr_.size (); i ++) 
    {
      int dy =abs (lastpos- head_l_arr_[i]->position_i_);
	
      if (dy <= 1) 
	{
	  if (parity)
	    head_l_arr_[i]->x_dir_ = (stem_xdir_ == LEFT) ? LEFT : RIGHT;
	  parity = !parity;
	}
      else
	parity = 0;
      lastpos = head_l_arr_[i]->position_i_;
    }
}

void
Stem::do_pre_processing ()
{
  if (yextent_drul_[DOWN]== yextent_drul_[UP])
    set_default_extents ();
  set_noteheads ();
  flag_i_ = flag_i_;
  transparent_b_ = invisible_b ();
  set_empty (invisible_b ());
}


Interval
Stem::do_width () const
{
  Interval r (0, 0);
  if (abbrev_flag_i_)
    {
      r = abbrev_mol ().extent ().x ();
    }
  else if (beam_l_ || abs (flag_i_) <= 2)
    ;	// TODO!
  else
    {
      Paper_def*p= paper ();
      r = p->lookup_l ()->flag (flag_i_, dir_).dim_.x ();
      r += note_delta_f ();
    }
  return r;
}


  
Molecule
Stem::abbrev_mol () const
{
  Real dy = paper ()->interbeam_f ();
  Real w = 1.5 * paper ()->lookup_l ()->ball (2).dim_.x ().length ();
  Real beamdy = paper ()->interline_f () / 2;
 
  int beams_i = 0;
  Real slope = paper ()->internote_f () / 4;
 
  if (beam_l_) {
    // huh?
      slope = 2 * beam_l_->slope;
    // ugh, rather calc from Abbreviation_req
      beams_i = beams_right_i_ >? beams_left_i_; 
  }
  paper ()->lookup_l ()->beam (slope, 20 PT);
  
  Molecule beams;
  Atom a (paper ()->lookup_l ()->beam (slope, w));
  a.translate (Offset(- w / 2, stem_end_f () - (w / 2 * slope)));
  // ugh
    if (!beams_i)
      a.translate_axis (dy + beamdy - dir_ * dy, Y_AXIS);
    else
      a.translate_axis (2 * beamdy - dir_ * (beamdy - dy), Y_AXIS);
  
  for (int i = 0; i < abbrev_flag_i_; i++) 
    {
      Atom b (a);
      b.translate_axis (-dir_ * dy * (beams_i + i), Y_AXIS);
      beams.add (b);
    }
  
  return beams;
}

Molecule*
Stem::brew_molecule_p () const 
{
  Molecule *mol_p =new Molecule;
  
  Real bot  = yextent_drul_[DOWN];
  Real top = yextent_drul_[UP];
  
  assert (bot!=top);
  
  Paper_def *p =paper ();
  
  Real dy = p->internote_f ();
  if (!invisible_b ())
    {
      Atom ss =p->lookup_l ()->stem (bot*dy,top*dy);
      mol_p->add (Atom (ss));
    }
  
  if (!beam_l_ &&abs (flag_i_) > 2)
    {
      Atom fl = p->lookup_l ()->flag (flag_i_, dir_);
      mol_p->add_at_edge (Y_AXIS, dir_, Molecule (Atom (fl)));
      assert (!abbrev_flag_i_);
    }
  
  if (abbrev_flag_i_)
    mol_p->add (abbrev_mol ());

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
      Interval head_wid(0,  head_l_arr_[0]->width ().length ());
      Real rule_thick(paper ()->rule_thickness ());
      Interval stem_wid(-rule_thick/2, rule_thick/2);
      r = head_wid[stem_xdir_] - stem_wid[stem_xdir_];
    }
  return r;
}
Real
Stem::hpos_f () const
{
  return note_delta_f () +Item::hpos_f ();
}

/*
TODO:  head_l_arr_/rest_l_arr_ in  do_substitute_dependent ()
 */
void
 Stem::do_substitute_dependency (Score_elem*o,Score_elem*n)
{
  Item * o_l = o->item ();
  Item * n_l = n? n->item () : 0;
  head_l_arr_.substitute ((Note_head*)o_l, (Note_head*)n_l);
  rest_l_arr_.substitute ((Rest*)o_l, (Rest*)n_l);
}
