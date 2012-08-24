\version "2.16.0"


\header
{

  texidoc = "PDF metadata need either Latin1 encoding (not UTF8) or full
  UTF-16BE with BOM. The title field uses full UTF-16 (russian characters,
  euro, etc), while the composer uses normal european diacrits (which need
  to be encoded as Latin1, not as UTF8). Closing parenthesis need to be
  escaped by a backslash AFTER encoding!"

  % Non-latin1 text, requiring UTF-16BE (with BOM) encoding in PDF metatdata:
  % closing parentheses and backslashed need to be escaped AFTER encoding!
  title = "UTF-16BE title:² € ĂĄœŖŮůſЖюљ)\\\n ¡"
  % Latin1 text, requiring at least PDFDocEncoding in PDF metadata, all Latin1
  % characters coincide, so no special encoding is required, just print out
  % the Latin1 characters (NOT the utf8 bytes!)
  composer = "Latin1 composer (with special chars): Jöhånñ Strauß"
  poet = "UTF-16BE with parentheses: ) € ĂĄœŖŮůſЖюљ"
}

\score
{
  \new Staff c'1
}
