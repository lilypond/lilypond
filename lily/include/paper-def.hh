/*
  paper-def.hh -- declare Paper_def

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PAPER_DEF_HH
#define PAPER_DEF_HH
#include "lily-proto.hh"

#include "real.hh"
#include "string.hh"
#include "moment.hh"


/** 

  symbols, dimensions and constants

  This struct takes care of all kinds of symbols, dimensions and
  constants. Most of them are related to the point-size of the fonts,
  so therefore, the lookup table for symbols is also in here.

  TODO: 
  
  add support for multiple fontsizes 
  split into "Input_paper_def" and Paper_def

  add support for other len->wid conversions.

  Input_engraver should be in here.
 */
class Paper_def {
    Lookup *lookup_p_;
    Assoc<String, Real> *real_vars_p_;

    Input_translator * itrans_p_;
public:    
    String outfile_str_;
    
    
    /* *************** */
    void set_var(String, Real);
    Real get_var (String)const;
    void reinit();
    Paper_def();
    void set(Lookup*);
    void set (Input_translator * );
    Global_translator * get_global_translator_p()const;
    ~Paper_def();
    Paper_def(Paper_def const&);
    /// The distance between beams
    Real interbeam_f()const;
    /**
      The distance between lines
     */
    Real interline_f()const;
    /// half the distance between lines
    Real internote_f()const;

    /// thickness of the standard line 
    Real rule_thickness()const;
    Real whole_width()const;
    Real linewidth_f()const;
    /// height of the staff
    Real standard_height()const;

    /// width of a quaver ball
    Real note_width() const;
    void print() const;

    Lookup const * lookup_l();	// TODO naming

    /** convert a duration to an idealspacing
      influence using the geometric_ and  parameters.
      */
    Real duration_to_dist(Moment);
};

#endif // Paper_def_HH

