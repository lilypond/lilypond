#include "item.hh"
#include "spanner.hh"

// thanks to GDBs wonderful casting abilities, we need these:
Item*
to_item (Graphical_element* g)
{
  return dynamic_cast<Item*>(g);
}
Spanner*
to_spanner (Graphical_element*g)
{
  return dynamic_cast<Spanner*>(g);
}
