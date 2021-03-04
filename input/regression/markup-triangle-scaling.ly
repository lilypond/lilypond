\version "2.23.1"

\header {
   texidoc = "Triangles should scale appropriately with font size."
}

\markup {
  \fontsize #-6  { "-6" \triangle ##f }
  \fontsize #-4  { "-4" \triangle ##f }
  \fontsize #-2  { "-2" \triangle ##f }
  \fontsize #0  { " 0" \triangle ##f }
  \fontsize #2  { "+2" \triangle ##f }
  \fontsize #4  { "+4" \triangle ##f }
  \fontsize #6  { "+6" \triangle ##f }
}
