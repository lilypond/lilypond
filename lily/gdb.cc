#include "paper-column.hh"
#include "spanner.hh"

extern "C"
{
  // thanks to GDBs wonderful casting abilities, we need these:
  Item *to_item (Grob *g) { return dynamic_cast<Item *> (g); }
  Spanner *to_spanner (Grob *g) { return dynamic_cast<Spanner *> (g); }

  Paper_column *to_pc (Grob *g) { return dynamic_cast<Paper_column *> (g); }
}
