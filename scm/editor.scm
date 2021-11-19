;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2005--2020 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (scm editor))

;; Also for standalone use, so cannot include any lily modules.
(use-modules
 (ice-9 regex)
 (srfi srfi-13)
 (srfi srfi-14))

(define PLATFORM
  (string->symbol
   (string-downcase
    (car (string-tokenize (vector-ref (uname) 0) char-set:letter)))))

(define (get-editor)
  (or (getenv "LYEDITOR")
      (getenv "XEDITOR")
      (getenv "EDITOR")

      ;; FIXME: how are default/preferred editors specified on
      ;; different platforms?
      (case PLATFORM
        ((windows) "lilypad")
        (else
         "emacs"))))

;; A bunch of stuff stolen from Emacs

(define (w32-using-nt)
  "Return non-nil if running on a Windows NT descendant.
That includes all Windows systems except for 9X/Me."
  (getenv "SystemRoot"))

(define (w32-shell-name)
  "Return the name of the shell being used."
  (or (getenv "SHELL")
      (and (w32-using-nt) "cmd.exe")
      "command.com"))

(define w32-system-shells '("cmd" "cmd.exe" "command" "command.com"
                            "4nt" "4nt.exe" "4dos" "4dos.exe"
                            "tcc" "tcc.exe" "ndos" "ndos.exe"))

(define (w32-system-shell-p shell-name)
  (and shell-name
       (member (string-downcase
                (basename shell-name))
               w32-system-shells)))

(define (w32-shell-dos-semantics)
  "Return non-nil if the interactive shell being used expects MS-DOS shell semantics."
  (or (w32-system-shell-p (w32-shell-name))
      (and (member (string-downcase (basename (w32-shell-name)))
                   '("cmdproxy" "cmdproxy.exe"))
           (w32-system-shell-p (getenv "COMSPEC")))))

(define-public (shell-quote-argument argument)
  "Quote ARGUMENT for passing as argument to an inferior shell.

This function is designed to work with the syntax of your system's
standard shell, and might produce incorrect results with unusual shells.
See Info node `(elisp)Security Considerations'."
  (cond
   ((and (eq? PLATFORM 'windows) (w32-shell-dos-semantics))

    ;; First, quote argument so that CommandLineToArgvW will
    ;; understand it.  See
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft%28v=vs.85%29.aspx
    ;; After we perform that level of quoting, escape shell
    ;; metacharacters so that cmd won't mangle our argument.  If the
    ;; argument contains no double quote characters, we can just
    ;; surround it with double quotes.  Otherwise, we need to prefix
    ;; each shell metacharacter with a caret.

    (set! argument
          ;; escape backslashes at end of string
          (regexp-substitute/global
           #f
           "(\\\\+)$"
           ;; escape backslashes and quotes in string body
           (regexp-substitute/global
            #f
            "(\\\\*)\""
            argument
            'pre 1 1 "\\\"" 'post)
           'pre 1 1 'post))

    (if (string-match "[%!\"]" argument)
        (string-append
         "^\""
         (regexp-substitute/global
          #f
          "[%!()\"<>&|^]"
          argument
          'pre "^" 0 'post)
         "^\"")
        (string-append "\"" argument "\"")))

   (else
    (if (string-null? argument)
        "''"
        ;; Quote everything except POSIX filename characters.
        ;; This should be safe enough even for really weird shells.
        (regexp-substitute/global
         #f
         "\n"
         (regexp-substitute/global
          #f
;;;       "[^-0-9a-zA-Z_./\n]" Negative ranges are too dangerous since
;;;       their UTF-8 implications aren't clear: we don't want
;;;       characters outside the ASCII range quoted since it is not
;;;       clear whether we need to quote bytes or characters. Inverting
;;;       the ranges doesn't work either since "man 7 regex" suggests
;;;       that "Ranges are very collating-sequence-dependent, and
;;;       portable programs should avoid relying on them." So we just
;;;       take care of a subset of (printable) characters that are known
;;;       to cause problems.
;;;       (']' has to be first because it cannot be escaped, and '-' has
;;;       to be last or it is (again) interpreted as a range.)
          "[]!\"#$%&'()*+,.:;<=>?@[\\^_`{|}~-]"
          argument
          'pre "\\" 0 'post)
         'pre  "'\n'" 'post)))
   ))


(define editor-command-template-alist
  '(("atom" . "atom %(file)s:%(line)s:%(column)s")
    ("emacs" .  "emacsclient --no-wait +%(line)s:%(column)s %(file)s || (emacs +%(line)s:%(column)s %(file)s&)")
    ("geany" . "geany --line %(line)s --column %(column)s %(file)s")
    ("gedit" . "gedit --wait %(file)s +%(line)s:%(column)s")
    ("gvim" . "gvim --remote +:%(line)s:norm%(column)s %(file)s")
    ("jedit" . "jedit -reuseview %(file)s +line:%(line)s")
    ("kate" . "kate --block --line %(line)s --column %(column)s %(file)s")
    ("lilypad" . "lilypad +%(line)s:%(char)s %(file)s")
    ("nedit" . "nc -noask +%(line)s %(file)s")
    ("syn" . "syn -line %(line)s -col %(char)s %(file)s")
    ("uedit32" . "uedit32 %(file)s -l%(line)s -c%(char)s")))

(define (get-command-template alist editor)
  (define (get-command-template-helper)
    (if (null? alist)
        (if (string-match "%\\(file\\)s" editor)
            editor
            (string-append editor " %(file)s"))
        (if (string-match (caar alist) editor)
            (cdar alist)
            (get-command-template (cdr alist) editor))))
  (if (string-match "%\\(file\\)s" editor)
      editor
      (get-command-template-helper)))

(define (re-sub re sub string)
  (regexp-substitute/global #f re string 'pre sub 'post))

(define (slashify x)
  (if (string-index x #\/)
      x
      (re-sub "\\\\" "/" x)))

(define-public (get-editor-command file-name line char column)
  (let* ((editor (get-editor))
         (template (get-command-template editor-command-template-alist editor))
         (command
          (re-sub "%\\(file\\)s" (shell-quote-argument file-name)
                  (re-sub "%\\(line\\)s" (format #f "~a" line)
                          (re-sub "%\\(char\\)s" (format #f "~a" char)
                                  (re-sub
                                   "%\\(column\\)s" (format #f "~a" column)
                                   (slashify template)))))))
    command))
