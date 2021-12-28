%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2022 Bertrand Bordage <bordage.bertrand@gmail.com>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.16.0"

#(use-modules ((srfi srfi-1)
               #:select (alist-copy)))

#(define (add-text-replacements! alist)
  "Add replacements in ALIST for text. Must be called from within a \\paper block"
  (let*
   ((mod (current-module))
    ;; we can't use text-font-defaults directly here; it is bound in the shared module
    ;; from declarations-init.
    (cur (alist-copy (ly:modules-lookup (list mod) 'text-font-defaults)))
    )
   (module-set! mod 'text-font-defaults
    (assoc-set! cur 'replacement-alist
                     (append alist
                      (assoc-get 'replacement-alist cur '()))))))

#(define (include-special-characters)
   (add-text-replacements!
     '(;; Punctuation
       ("&hellip;" . "…")
       ("&ndash;" . "–")
       ("&mdash;" . "—")
       ("&iexcl;" . "¡")
       ("&iquest;" . "¿")
       ("&solidus;" . "∕") ; this is not a slash,
                           ; contrary to what is said in Unicode.

       ;; French, German and English quotes open/close
       ("&flq;" . "‹")
       ("&frq;" . "›")
       ("&flqq;" . "«")
       ("&frqq;" . "»")
       ("&glq;" . "‚")
       ("&grq;" . "‘")
       ("&glqq;" . "„")
       ("&grqq;" . "“")
       ("&elq;" . "‘")
       ("&erq;" . "’")
       ("&elqq;" . "“")
       ("&erqq;" . "”")

       ;; Word dividers
       ("&ensp;" . " ")
       ("&emsp;" . " ")
       ("&thinsp;" . " ")
       ("&nbsp;" . " ")
       ("&nnbsp;" . " ") ; narrow non-breaking space
       ("&zwj;" . "‍")
       ("&zwnj;" . "‌")
       ("&middot;" . "·") ; interpunct

       ;; General typography
       ("&bull;" . "•")
       ("&copyright;" . "©")
       ("&registered;" . "®")
       ("&trademark;" . "™")
       ("&dagger;" . "†")
       ("&Dagger;" . "‡")
       ("&numero;" . "№")
       ("&ordf;" . "ª")
       ("&ordm;" . "º")
       ("&para;" . "¶")
       ("&sect;" . "§")
       ("&deg;" . "°")
       ("&numero;" . "№")
       ("&permil;" . "‰")
       ("&brvbar;" . "¦")

       ;; Diacritics
       ("&acute;" . "´")
       ("&acutedbl;" . "˝")
       ("&grave;" . "`")
       ("&breve;" . "˘")
       ("&caron;" . "ˇ")
       ("&cedilla;" . "¸")
       ("&circumflex;" . "^")
       ("&diaeresis;" . "¨")
       ("&macron;" . "¯")

       ;; Non-ASCII Letters (Excluding Accented Letters)
       ("&aa;" . "å")
       ("&AA;" . "Å")
       ("&ae;" . "æ")
       ("&AE;" . "Æ")
       ("&auml;" . "ä")
       ("&Auml;" . "Ä")
       ("&dh;" . "ð")
       ("&DH;" . "Ð")
       ("&dj;" . "đ")
       ("&DJ;" . "Đ")
       ("&l;" . "ł")
       ("&L;" . "Ł")
       ("&ng;" . "ŋ")
       ("&NG;" . "Ŋ")
       ("&o;" . "ø")
       ("&O;" . "Ø")
       ("&oe;" . "œ")
       ("&OE;" . "Œ")
       ("&ouml;" . "ö")
       ("&Ouml;" . "Ö")
       ("&s;" . "ſ")
       ("&ss;" . "ß")
       ("&th;" . "þ")
       ("&TH;" . "Þ")
       ("&uuml;" . "ü")
       ("&Uuml;" . "Ü")

       ;; Mathematical symbols
       ("&plus;" . "+")
       ("&minus;" . "−")
       ("&times;" . "×")
       ("&div;" . "÷")
       ("&sup1;" . "¹")
       ("&sup2;" . "²")
       ("&sup3;" . "³")
       ("&sqrt;" . "√")
       ("&increment;" . "∆")
       ("&infty;" . "∞")
       ("&sum;" . "∑")
       ("&pm;" . "±")
       ("&bulletop;" . "∙")
       ("&partial;" . "∂")
       ("&neg;" . "¬")

       ;; Currency symbols
       ("&currency;" . "¤")
       ("&dollar;" . "$")
       ("&euro;" . "€")
       ("&pounds;" . "£")
       ("&yen;" . "¥")
       ("&cent;" . "¢"))))
