#include "grob.hh"
#include "unpure-pure-container.hh"
#include "lily-imports.hh"

SCM
axis_offset_symbol (Axis a)
{
  return a == X_AXIS
         ? ly_symbol2scm ("X-offset")
         : ly_symbol2scm ("Y-offset");
}

SCM
axis_parent_positioning (Axis a)
{
  return (a == X_AXIS)
         ? Grob::x_parent_positioning_proc
         : Grob::y_parent_positioning_proc;
}

/*
  Replace

  (orig-proc GROB)

  by

  (+ (PROC GROB) (orig-proc GROB))
*/
void
add_offset_callback (Grob *g, SCM proc, Axis a)
{
  SCM sym = axis_offset_symbol (a);
  SCM data = get_property_data (g, sym);
  set_property (g, sym, Lily::grob_offset_function (proc, data));
}

/*
  replace

  (orig-proc GROB)

  by

  (PROC GROB (orig-proc GROB))
*/
void
chain_callback (Grob *g, SCM proc, SCM sym)
{
  SCM data = get_property_data (g, sym);
  set_property (g, sym, Lily::grob_compose_function (proc, data));
}

void
chain_offset_callback (Grob *g, SCM proc, Axis a)
{
  chain_callback (g, proc, axis_offset_symbol (a));
}
