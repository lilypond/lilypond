;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2020 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define-public latin1-coding-vector
  #(.notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef
            .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef
            .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef
            .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef
            %% 0x20
            space exclam quotedbl numbersign dollar percent ampersand quoteright
            parenleft parenright asterisk plus comma hyphen period slash
            zero one two three four five six seven
            eight nine colon semicolon less equal greater question
            %% 0x40
            at A B C D E F G
            H I J K L M N O
            P Q R S T U V W
            X Y Z bracketleft backslash bracketright asciicircum underscore
            %% 0x60
            `quoteleft a b c d e f g
            h i j k l m n o
            p q r s t u v w
            x y z braceleft bar braceright asciitilde .notdef
            %% 0x80
            .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef
            .notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef
            dotlessi grave acute circumflex tilde macron breve dotaccent
            dieresis .notdef ring cedilla .notdef hungarumlaut ogonek caron
            %% 0xA0
            space exclamdown cent sterling currency yen brokenbar section
            dieresis copyright ordfeminine guillemotleft logicalnot hyphen registered macron
            degree plusminus twosuperior threesuperior acute mu paragraph periodcentered
            cedilla onesuperior ordmasculine guillemotright onequarter onehalf threequarters questiondown
            %% 0xC0
            Agrave Aacute Acircumflex Atilde Adieresis Aring AE Ccedilla
            Egrave Eacute Ecircumflex Edieresis Igrave Iacute Icircumflex Idieresis
            Eth Ntilde Ograve Oacute Ocircumflex Otilde Odieresis multiply
            Oslash Ugrave Uacute Ucircumflex Udieresis Yacute Thorn germandbls
            %% 0xE0
            agrave aacute acircumflex atilde adieresis aring ae ccedilla
            egrave eacute ecircumflex edieresis igrave iacute icircumflex idieresis
            eth ntilde ograve oacute ocircumflex otilde odieresis divide
            oslash ugrave uacute ucircumflex udieresis yacute thorn ydieresis))


(define-public (decode-byte-string str)
  "Return vector of glyphname symbols that correspond to string,
assuming that @var{str} is byte-coded using latin-1 encoding."

  (let* ((len (string-length str))
         (output-vector (make-vector len '.notdef)))
    (do
        ((idx 0 (1+ idx)))
        ((>= idx len) output-vector)
      (vector-set! output-vector idx
                   (vector-ref latin1-coding-vector
                               (char->integer (string-ref str idx)))))))
