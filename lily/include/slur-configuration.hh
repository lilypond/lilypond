/* 
  slur-configuration.hh -- declare Slur_configuration
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
  
*/

#ifndef SLUR_CONFIGURATION_HH
#define SLUR_CONFIGURATION_HH

#include "drul-array.hh"
#include "bezier.hh"
#include "lily-proto.hh"
#include "parray.hh"
#include "main.hh"

class Slur_configuration
{
public:
  Drul_array<Offset> attachment_;
  Real score_;
  Bezier curve_;

#if DEBUG_SLUR_SCORING
  String score_card_;
#endif

  Slur_configuration ();

  void generate_curve (Slur_score_state const &state, Real r0, Real h_inf);
  void score (Slur_score_state const&);

protected:
  void score_extra_encompass (Slur_score_state const&);
  void score_slopes  (Slur_score_state const&);
  void score_edges (Slur_score_state const&);
  void score_encompass (Slur_score_state const&);
};

#endif /* SLUR_CONFIGURATION_HH */

