/*
  paper-def.hh -- declare Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PAPER_DEF_HH
#define PAPER_DEF_HH
#include "lily-proto.hh"

#include "real.hh"

#include "moment.hh"
#include "varray.hh"
#include "interval.hh"
#include "music-output-def.hh"

/** 

  Symbols, dimensions and constants pertaining to visual output.

  This struct takes care of all kinds of symbols, dimensions and
  constants. Most of them are related to the point-size of the fonts,
  so therefore, the lookup table for symbols is also in here.

  TODO: 
  
  add support for multiple fontsizes 

  add support for other len->wid conversions.

  Input_engraver should be in here.
 */
class Paper_def : public Music_output_def {
  Lookup *lookup_p_;
  Dictionary<Real> *real_vars_p_;
  static int default_count_i_;
protected:
  VIRTUAL_COPY_CONS(Paper_def,Music_output_def);
public:    
  virtual ~Paper_def ();
  DECLARE_MY_RUNTIME_TYPEINFO;

  Array<Interval> shape_int_a_;

  /**
    Set or overwrite a variable
   */
  void set_var (String, Real);
  /**
    Read a variable.  Crash if it doesn't exist.
   */
  Real get_var (String) const;
  void reinit ();
  Paper_def ();
  void set (Lookup*);

  Paper_def (Paper_def const&);
  /// The distance between beams
  Real interbeam_f () const;
  /**
    The distance between lines
    */
  Real interline_f () const;
  /// half the distance between lines
  Real internote_f () const;

  /// thickness of the standard line 
  Real rule_thickness () const;
  Interval line_dimensions_int (int) const;
  Real linewidth_f () const;
  /// height of the staff
  Real standard_height () const;

  /// width of a crotchet ball
  Real note_width () const;
  void print () const;

  Lookup const * lookup_l ();	// TODO naming

  /** convert a duration to an idealspacing
    influence using the geometric_ and  parameters.
    */
  Real duration_to_dist (Moment, Real) const;
  Real geometric_spacing (Moment) const;
  Real arithmetic_constant (Moment minimal_mom) const;
  Real arithmetic_spacing (Moment mom,Real constant) const;
  virtual int get_next_default_count () const;
  String TeX_output_settings_str () const;
};

#endif // Paper_def_HH

