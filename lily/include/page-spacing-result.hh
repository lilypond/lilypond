/* 
  page-spacing-result.hh -- declare  Page_spacing_result
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2007--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#ifndef PAGE_SPACING_RESULT_HH
#define PAGE_SPACING_RESULT_HH

#include "std-vector.hh"
#include "lily-proto.hh"

// This enum is a bitfield: since we use one System_count_status
// to represent the system count of several pages simultaneously,
// it could be that one page has too many systems while another
// has too few.
typedef enum {
  SYSTEM_COUNT_OK = 0,
  SYSTEM_COUNT_TOO_MANY = 1,
  SYSTEM_COUNT_TOO_FEW = 2
} System_count_status;

struct Page_spacing_result {
  vector<vsize> systems_per_page_;
  vector<Real> force_;
  Real penalty_;
  Real demerits_;
  int system_count_status_;

  Real average_force () const;
  vsize page_count () const;
  vsize system_count () const;
  void print () const;  
  Page_spacing_result ();
};

#endif /* PAGE_SPACING_RESULT_HH */
