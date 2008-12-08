/*
  measure-grouping-spanner.hh -- declare

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MEASURE_GROUPING_SPANNER_HH
#define MEASURE_GROUPING_SPANNER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"


class Measure_grouping
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));

  DECLARE_GROB_INTERFACE();
};

#endif /* MEASURE_GROUPING_SPANNER_HH */

