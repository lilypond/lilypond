;;;
;;; auto-beam.scm -- Auto-beam-engraver settings
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
;;;

;;; specify generic beam begin and end times

;;; format:
;;;
;;;   function shortest-duration-in-beam time-signature
;;;
;;; where
;;;
;;;     function = begin or end
;;;     shortest-duration-in-beam = numerator denominator; eg: 1 16
;;;     time-signature = numerator denominator, eg: 4 4
;;;
;;; unspecified or wildcard entries for duration or time-signature
;;; are given by * *

;;; maybe do:  '(end shortest-1 16 time-3 4) ?

;;; in 3 2 time:
;;;   end beams each 1 2 note
;;;   end beams with 16th notes each 1 4 note
;;;   end beams with 32th notes each 1 8 note

(define auto-beam-settings
   `(
     ((end * * 3 2) . ,(make-moment 1 2))
     ((end 1 16 3 2) . ,(make-moment 1 4))
     ((end 1 32 3 2) . ,(make-moment 1 8))

     ((begin 1 8 3 4) . ,(make-moment 1 4))

     ((end * * 3 4) . ,(make-moment 3 4))
     ((begin 1 16 3 4) . ,(make-moment 1 16))
     ((end 1 16 3 4) . ,(make-moment 1 4))
     ;;((begin 1 32 3 4) . ,(make-moment 1 8))
     ((end 1 32 3 4) . ,(make-moment 1 8))

     ((begin 1 16 3 8) . ,(make-moment 1 8))
     ((end * * 3 8) . ,(make-moment 3 8))

     ;; in common time:
     ;;   end beams each 1 2 note
     ;;   end beams with 32th notes each 1 8 note
     ;;   end beams with 1 8 triplets each 1 4 note

     ((end * * 4 4) . ,(make-moment 1 2))
     ((end 1 12 4 4) . ,(make-moment 1 4))
     ((end 1 16 4 4) . ,(make-moment 1 4))
     ((end 1 32 4 4) . ,(make-moment 1 8))

     ((end * * 2 4) . ,(make-moment 1 4))
     ((end 1 12 2 4) . ,(make-moment 1 4))
     ((end 1 16 2 4) . ,(make-moment 1 4))
     ((end 1 32 2 4) . ,(make-moment 1 8))

     ;; It seems that, because of a bug in the previous auto-beamer,
     ;; we had the effect of this setting x
     ;; ((end * * 2 8) . ,(make-moment 2 8))

     ((end * * 4 8) . ,(make-moment 1 4))
     ((end 1 16 4 8) . ,(make-moment 1 4))
     ((end 1 32 4 8) . ,(make-moment 1 8))

     ((end * * 4 16) . ,(make-moment 1 8))

     ((end * * 6 8) . ,(make-moment 3 8))
     ((end 1 16 6 8) . ,(make-moment 3 8))
     ((end 1 32 6 8) . ,(make-moment 1 8))

     ((end * * 9 8) . ,(make-moment 3 8))
     ((end 1 16 9 8) . ,(make-moment 3 8))
     ((end 1 32 9 8) . ,(make-moment 1 8))

     ((end * * 12 8) . ,(make-moment 3 8))
     ((end 1 16 12 8) . ,(make-moment 3 8))
     ((end 1 32 12 8) . ,(make-moment 1 8))
     (meta . ,(grob-description  "autoBeamSettings"))
     ))

;;; Users may override in most cases, simply by issuing
;;;
;;;    % from here on consider ending beam every 1 4 note
;;;    \property Voice.autoBeamSettings \push #'(end * * * *) = #(make-moment 1 4)
;;;
;;;    % no autobeaming
;;;    \property Voice.beamAuto = ##f  
;;;
;;; or, more globally, by doing:
;;;
;;; \paper{
;;;        \translator{
;;;            \VoiceContext
;;;            % consider ending beam at every 1 2 note
;;;            autoBeamSettings \push #'(end * * * *) = #(make-moment 1 2)
;;;        }
;;;    }
;;;
;;; see also input test auto-beam-override.ly

