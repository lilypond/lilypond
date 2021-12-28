;;;; lilypond-song.el --- Emacs support for LilyPond singing
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2022 Brailcom, o.p.s.
;;;; Author: Milan Zamazal <pdm@brailcom.org>
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

;;; Commentary:

;; This file adds Emacs support for singing lyrics of LilyPond files.
;; It extends lilypond-mode with the following commands (see their
;; documentation for more information):
;; 
;; - M-x LilyPond-command-sing (C-c C-a)
;; - M-x LilyPond-command-sing-and-play (C-c C-q)
;; - M-x LilyPond-command-sing-last (C-c C-z)
;; 
;; Note these commands are not available from the standard LilyPond mode
;; command menus.

;;; Code:


(require 'cl)
(require 'lilypond-mode)

(ignore-errors (require 'ecasound))


;;; User options


(defcustom LilyPond-synthesize-command "lilysong"
  "Command used to sing LilyPond files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-play-command (or (executable-find "ecaplay") "play")
  "Command used to play WAV files."
  :group 'LilyPond
  :type 'string)

;; In case you would like to use fluidsynth (not recommended as fluidsynth
;; can perform wave file synthesis only in real time), you can use the
;; following setting:
;; (setq LilyPond-midi->wav-command "fluidsynth -nil -a file soundfont.sf2 '%s' && sox -t raw -s -r 44100 -w -c 2 fluidsynth.raw '%t'")
(defcustom LilyPond-midi->wav-command "timidity -Ow %m -s %r -o '%t' '%s'"
  "Command used to make a WAV file from a MIDI file.
%s in the string is replaced with the source MIDI file name,
%t is replaced with the target WAV file name.
%r is replaced with rate.
%m is replaced with lilymidi call."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-voice-rates
  '((".*czech.*" . 44100)
    (".*\\<fi\\(\\>\\|nnish\\).*" . 22050)
    (".*" . 16000))
  "Alist of regexps matching voices and the corresponding voice rates.
It may be necessary to define proper voice rates here in order to
avoid ecasound resampling problems."
  :group 'LilyPond
  :type '(alist :key-type regexp :value-type integer))

(defcustom LilyPond-use-ecasound (and (featurep 'ecasound)
                                      (executable-find "ecasound")
                                      t)
  "If non-nil, use ecasound for mixing and playing songs."
  :group 'LilyPond
  :type 'boolean)

(defcustom LilyPond-voice-track-regexp "voice"
  "Perl regexp matching names of MIDI tracks to be ignored on sing&play."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-lilymidi-command "\"`lilymidi --prefix-tracks -Q --filter-tracks '%s' '%f'`\""
  "Command to insert into LilyPond-midi->wav-command calls.
%f is replaced with the corresponding MIDI file name.
%s is replaced with `LilyPond-voice-track-regexp'."
  :group 'LilyPond
  :type 'string)


;;; Lyrics language handling


(defvar lilysong-language nil)
(make-variable-buffer-local 'lilysong-language)

(defvar lilysong-last-language nil)
(make-variable-buffer-local 'lilysong-last-language)

(defvar lilysong-languages '("cs" "en"))

(defvar lilysong-voices nil)

(defun lilysong-voices ()
  (or lilysong-voices
      (with-temp-buffer
        (call-process "lilysong" nil t nil "--list-voices")
        (call-process "lilysong" nil t nil "--list-languages")
        (goto-char (point-min))
        (while (not (eobp))
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                lilysong-voices)
          (forward-line))
        lilysong-voices)))
  
(defun lilysong-change-language ()
  "Change synthesis language or voice of the current document."
  (interactive)
  (setq lilysong-language
        (completing-read "Lyrics language or voice: "
                         (mapcar 'list (lilysong-voices)))))

(defun lilysong-update-language ()
  (unless lilysong-language
    (lilysong-change-language)))


;;; Looking for \festival* and \midi commands


(defun lilysong-document-files ()
  (let ((resulting-files ())
        (stack (list (LilyPond-get-master-file))))
    (while (not (null stack))
      (let ((file (expand-file-name (pop stack))))
        (when (and (file-exists-p file)
                   (not (member file resulting-files)))
          (push file resulting-files)
          (save-excursion
            (save-restriction
              (set-buffer (find-file-noselect file nil))
              (widen)
              (goto-char (point-min))
              (while (re-search-forward "^[^%\n]*\\\\include +\"\\([^\"]+\\)\"" nil t)
                (push (match-string 1) stack)))))))
    (nreverse resulting-files)))
     
(defvar lilysong-festival-command-regexp
  "^[^%\n]*\\\\festival\\(syl\\)? +#\"\\([^\"]+\\)\"")

(defun lilysong-find-song (direction)
  "Find XML file name of the nearest Festival command in the given DIRECTION.
DIRECTION is one of the symbols `forward' or `backward'.
If no Festival command is found in the current buffer, return nil.
The point is left at the position where the command occurrence was found."
  (save-match-data
    (when (funcall (if (eq direction 'backward)
                       're-search-backward
                     're-search-forward)
                   lilysong-festival-command-regexp nil t)
      (match-string-no-properties 2))))

(defun lilysong-current-song ()
  "Return the XML file name corresponding to the song around current point.
If there is none, return nil."
  (save-excursion
    (or (progn (end-of-line) (lilysong-find-song 'backward))
        (progn (beginning-of-line) (lilysong-find-song 'forward)))))

(defun lilysong-all-songs (&optional limit-to-region)
  "Return list of XML file names of the song commands in the current buffer.
If there are none, return an empty list.
If LIMIT-TO-REGION is non-nil, look for the commands in the current region
only."
  (let ((result '())
        (current nil))
    (save-excursion
      (save-restriction
        (when limit-to-region
          (narrow-to-region (or (mark) (point)) (point)))
        (goto-char (point-min))
        (while (setq current (lilysong-find-song 'forward))
          (push current result))))
    (nreverse result)))

(defun lilysong-walk-files (collector)
  (save-excursion
    (mapcar (lambda (f)
              (set-buffer (find-file-noselect f))
              (funcall collector))
            (lilysong-document-files))))

(defun lilysong-all-songs* ()
  "Return list of XML file names of the song commands in the current document."
  (remove-duplicates (apply #'append (lilysong-walk-files #'lilysong-all-songs))
                     :test #'equal))

(defvar lilysong-song-history nil)
(make-variable-buffer-local 'lilysong-song-history)

(defvar lilysong-last-song-list nil)
(make-variable-buffer-local 'lilysong-last-song-list)

(defvar lilysong-last-command-args nil)
(make-variable-buffer-local 'lilysong-last-command-args)

(defun lilysong-song-list (multi)
  (cond
   ((eq multi 'all)
    (lilysong-all-songs*))
   (multi
    (lilysong-select-songs))
   (t
    (lilysong-select-single-song))))

(defun lilysong-select-single-song ()
  (let ((song (lilysong-current-song)))
    (if song
        (list song)
      (error "No song found"))))

(defun lilysong-select-songs ()
  (let* ((all-songs (lilysong-all-songs*))
         (available-songs all-songs)
         (initial-songs (if (or (not lilysong-last-song-list)
                                (eq LilyPond-command-current
                                    'LilyPond-command-region))
                            (lilysong-all-songs t)
                          lilysong-last-song-list))
         (last-input (completing-read
                      (format "Sing file%s: "
                              (if initial-songs
                                  (format " (default `%s')"
                                          (mapconcat 'identity initial-songs
                                                     ", "))
                                ""))
                      (mapcar 'list all-songs)
                      nil t nil
                      'lilysong-song-history)))
    (if (equal last-input "")
        initial-songs
      (let ((song-list '())
            default-input)
        (while (not (equal last-input ""))
          (push last-input song-list)
          (setq default-input (second (member last-input available-songs)))
          (setq available-songs (remove last-input available-songs))
          (setq last-input (completing-read "Sing file: "
                                            (mapcar #'list available-songs)
                                            nil t default-input
                                            'lilysong-song-history)))
        (setq lilysong-last-song-list (nreverse song-list))))))

(defun lilysong-count-midi-words ()
  (count-rexp (point-min) (point-max) "^[^%]*\\\\midi"))

(defun lilysong-midi-list (multi)
  (if multi
      (let ((basename (file-name-sans-extension (buffer-file-name)))
            (count (apply #'+ (save-match-data
                                (lilysong-walk-files #'lilysong-count-midi-words))))
            (midi-files '()))
        (while (> count 0)
          (setq count (1- count))
          (if (= count 0)
              (push (concat basename ".midi") midi-files)
            (push (format "%s-%d.midi" basename count) midi-files)))
        midi-files)
    (list (LilyPond-string-current-midi))))


;;; Compilation


(defun lilysong-file->wav (filename &optional extension)
  (format "%s.%s" (save-match-data
                    (if (string-match "\\.midi$" filename)
                        filename
                      (file-name-sans-extension filename)))
          (or extension "wav")))

(defun lilysong-file->ewf (filename)
  (lilysong-file->wav filename "ewf"))

(defstruct lilysong-compilation-data
  command
  makefile
  buffer
  songs
  midi
  in-parallel)
(defvar lilysong-compilation-data nil)
(defun lilysong-sing (songs &optional midi-files in-parallel)
  (setq lilysong-last-command-args (list songs midi-files in-parallel))
  (lilysong-update-language)
  (add-to-list 'compilation-finish-functions 'lilysong-after-compilation)
  (setq songs (mapcar #'expand-file-name songs))
  (let* ((makefile (lilysong-makefile (current-buffer) songs midi-files))
         (command (format "make -f %s" makefile)))
    (setq lilysong-compilation-data
          (make-lilysong-compilation-data
           :command command
           :makefile makefile
           :buffer (current-buffer)
           :songs songs
           :midi midi-files
           :in-parallel in-parallel))
    (save-some-buffers (not compilation-ask-about-save))
    (unless (equal lilysong-language lilysong-last-language)
      (mapc #'(lambda (f) (when (file-exists-p f) (delete-file f)))
            (append songs (mapcar 'lilysong-file->wav midi-files))))
    (if (lilysong-up-to-date-p makefile)
        (lilysong-process-generated-files lilysong-compilation-data)
      (compile command))))

(defun lilysong-up-to-date-p (makefile)
  (equal (call-process "make" nil nil nil "-f" makefile "-q") 0))

(defun lilysong-makefile (buffer songs midi-files)
  (let ((temp-file (make-temp-file "Makefile.lilysong-el"))
        (language lilysong-language))
    (with-temp-file temp-file
      (let ((source-files (save-excursion
                            (set-buffer buffer)
                            (lilysong-document-files)))
            (master-file (save-excursion
                           (set-buffer buffer)
                           (LilyPond-get-master-file)))
            (lilyfiles (append songs midi-files)))
        (insert "all:")
        (dolist (f (mapcar 'lilysong-file->wav (append songs midi-files)))
          (insert " " f))
        (insert "\n")
        (when lilyfiles
          (dolist (f songs)
            (insert f " "))
          (when midi-files
            (dolist (f midi-files)
              (insert f " ")))
          (insert ": " master-file "\n")
          (insert "\t" LilyPond-lilypond-command " " master-file "\n")
          (dolist (f songs)
            (insert (lilysong-file->wav f) ": " f "\n")
            (insert "\t" LilyPond-synthesize-command " $< " (or language "") "\n"))
          ;; We can't use midi files in ecasound directly, because setpos
          ;; doesn't work on them.
          (let ((lilymidi LilyPond-lilymidi-command)
                (voice-rate (format "%d" (or (cdr (assoc-if (lambda (key) (string-match key language))
                                                            LilyPond-voice-rates))
                                             16000))))
            (when (string-match "%s" lilymidi)
              (setq lilymidi (replace-match LilyPond-voice-track-regexp nil nil lilymidi)))
            (dolist (f midi-files)
              (insert (lilysong-file->wav f) ": " f "\n")
              (let ((command LilyPond-midi->wav-command)
                    (lilymidi* lilymidi))
                (when (string-match "%s" command)
                  (setq command (replace-match f nil nil command)))
                (when (string-match "%t" command)
                  (setq command (replace-match (lilysong-file->wav f) nil nil command)))
                (when (string-match "%r" command)
                  (setq command (replace-match voice-rate nil nil command)))
                (when (string-match "%f" lilymidi*)
                  (setq lilymidi (replace-match f nil nil lilymidi*)))
                (when (string-match "%m" command)
                  (setq command (replace-match lilymidi nil nil command)))
                (insert "\t" command "\n")))
            ))))
    temp-file))

(defun lilysong-after-compilation (buffer message)
  (let ((data lilysong-compilation-data))
    (when (and data
               (equal compile-command
                      (lilysong-compilation-data-command data)))
      (unwind-protect
          (when (lilysong-up-to-date-p (lilysong-compilation-data-makefile data))
            (lilysong-process-generated-files data))
        (delete-file (lilysong-compilation-data-makefile data))))))

(defun lilysong-process-generated-files (data)
  (with-current-buffer (lilysong-compilation-data-buffer data)
    (setq lilysong-last-language lilysong-language))
  (lilysong-play-files (lilysong-compilation-data-in-parallel data)
                       (lilysong-compilation-data-songs data)
                       (lilysong-compilation-data-midi data)))


;;; Playing files


(defun lilysong-play-files (in-parallel songs midi-files)
  (funcall (if LilyPond-use-ecasound
               'lilysong-play-with-ecasound
             'lilysong-play-with-play)
           in-parallel songs midi-files))

(defun lilysong-call-play (files)
  (apply 'start-process "lilysong-el" nil LilyPond-play-command files))

(defun lilysong-play-with-play (in-parallel songs midi-files)
  (let ((files (mapcar 'lilysong-file->wav (append songs midi-files))))
    (if in-parallel
        (dolist (f files)
          (lilysong-call-play (list f)))
      (lilysong-call-play files))))

(defun lilysong-make-ewf-files (files)
  (let ((offset 0.0))
    (dolist (f files)
      (let* ((wav-file (lilysong-file->wav f))
             (length (with-temp-buffer
                       (call-process "ecalength" nil t nil "-s" wav-file)
                       (goto-char (point-max))
                       (forward-line -1)
                       (read (current-buffer)))))
        (with-temp-file (lilysong-file->ewf f)
          (insert "source = " wav-file "\n")
          (insert (format "offset = %s\n" offset))
          (insert "start-position = 0.0\n")
          (insert (format "length = %s\n" length))
          (insert "looping = false\n"))
        (setq offset (+ offset length))))))

(when (and (featurep 'ecasound)
           (not (fboundp 'eci-cs-set-param)))
  (defeci cs-set-param ((parameter "sChainsetup option: " "%s"))))

(defun lilysong-play-with-ecasound (in-parallel songs midi-files)
  (ecasound)
  (eci-cs-add "lilysong-el")
  (eci-cs-select "lilysong-el")
  (eci-cs-remove)
  (eci-cs-add "lilysong-el")
  (eci-cs-select "lilysong-el")
  (eci-cs-set-param "-z:mixmode,sum")
  (unless in-parallel
    (lilysong-make-ewf-files songs)
    ;; MIDI files should actually start with each of the songs
    (mapc 'lilysong-make-ewf-files (mapcar 'list midi-files)))
  (let* ((file->wav (if in-parallel 'lilysong-file->wav 'lilysong-file->ewf))
         (files (mapcar file->wav (append songs midi-files))))
    (dolist (f files)
      (eci-c-add f)
      (eci-c-select f)
      (eci-ai-add f))
    (eci-c-select-all)
    (eci-ao-add-default)
    (let* ((n (length songs))
           (right (if (<= n 1) 50 0))
           (step (if (<= n 1) 0 (/ 100.0 (1- n)))))
      (dolist (f songs)
        (let ((chain (funcall file->wav f)))
          (eci-c-select chain)
          (eci-cop-add "-erc:1,2")
          (eci-cop-add (format "-epp:%f" (min right 100)))
          (incf right step))))
    (eci-start)))


;;; User commands


(defun lilysong-arg->multi (arg)
  (cond
   ((not arg)
    nil)
   ((or
     (numberp arg)
     (equal arg '(4)))
    t)
   (t
    'all)))

(defun lilysong-command (arg play-midi?)
  (let* ((multi (lilysong-arg->multi arg))
         (song-list (lilysong-song-list multi))
         (midi-list (if play-midi? (lilysong-midi-list multi))))
    (message "Singing %s" (mapconcat 'identity song-list ", "))
    (lilysong-sing song-list midi-list (if play-midi? t (listp arg)))))

(defun LilyPond-command-sing (&optional arg)
  "Sing lyrics of the current LilyPond buffer.
Without any prefix argument, sing current \\festival* command.
With the universal prefix argument, ask which parts to sing.
With a double universal prefix argument, sing all the parts.
With a numeric prefix argument, ask which parts to sing and sing them
sequentially rather than in parallel."
  (interactive "P")
  (lilysong-command arg nil))

(defun LilyPond-command-sing-and-play (&optional arg)
  "Sing lyrics and play midi of the current LilyPond buffer.
Without any prefix argument, sing and play current \\festival* and \\midi
commands.
With the universal prefix argument, ask which parts to sing and play.
With a double universal prefix argument, sing and play all the parts."
  (interactive "P")
  (lilysong-command arg t))

(defun LilyPond-command-sing-last ()
  "Repeat last LilyPond singing command."
  (interactive)
  (if lilysong-last-command-args
      (apply 'lilysong-sing lilysong-last-command-args)
    (error "No previous singing command")))

(defun LilyPond-command-clean ()
  "Remove generated *.xml and *.wav files used for singing."
  (interactive)
  (flet ((delete-file* (file)
           (when (file-exists-p file)
             (delete-file file))))
    (dolist (xml-file (lilysong-song-list 'all))
      (delete-file* xml-file)
      (delete-file* (lilysong-file->wav xml-file)))
    (mapc 'delete-file* (mapcar 'lilysong-file->wav (lilysong-midi-list 'all)))))

(define-key LilyPond-mode-map "\C-c\C-a" 'LilyPond-command-sing)
(define-key LilyPond-mode-map "\C-c\C-q" 'LilyPond-command-sing-and-play)
(define-key LilyPond-mode-map "\C-c\C-x" 'LilyPond-command-clean)
(define-key LilyPond-mode-map "\C-c\C-z" 'LilyPond-command-sing-last)

(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Current" LilyPond-command-sing t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Selected" (LilyPond-command-sing '(4)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing All" (LilyPond-command-sing '(16)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Selected Sequentially" (LilyPond-command-sing 1) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play Current" LilyPond-command-sing-and-play t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play Selected" (LilyPond-command-sing-and-play '(4)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing and Play All" (LilyPond-command-sing-and-play '(16)) t])
(easy-menu-add-item LilyPond-command-menu nil
  ["Sing Last" LilyPond-command-sing-last t])


;;; Announce

(provide 'lilypond-song)


;;; lilypond-song.el ends here
