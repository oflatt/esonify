;;; esonify.el --- Sonify your code

;; Copyright (C) 2018 by Oliver Flatt

;; Author: Oliver Flatt <oflatt@gmail.com>
;; URL: https://github.com/oflatt/esonify
;; Package-Version: 1
;; Version: 0.02
;; Package-Requires: ((sound-wav "0.02"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; esonify makes characters typed audible through different types of sound waves, also reading code to you through sound.
;; toggle-esonify turns it on and off

;;; Code:

(defvar esoundifyonp t "if esoundify is on")

(defconst soundpath (expand-file-name "~/esonify/sounds"))

(defconst alphabetmap #s(hash-table size 26 data (122 0 120 1 99 2 118 3 98 4 110 5 109 6 97 7 115 8 100 9 102 10 103 11 104 12 106 13 107 14 108 15 113 16 119 17 101 18 114 19 116 20 121 21 117 22 105 23 111 24 112 25 )))

(defun playdrum(num)
  (sound-wav-play (concat soundpath "/drum_" (number-to-string num) ".wav")))

(defun playsine(num)
  (sound-wav-play (concat soundpath "/sine" (number-to-string (gethash num alphabetmap)) "_1" ".wav")))

(defun playtriangle(num)
  (sound-wav-play (concat soundpath "/triangle" (number-to-string (% num 37)) "_30.wav")))

(defun makesound ()
  (if esoundifyonp
      (progn
	; make arrows the same as movement commands
	(if (symbolp last-command-event)
	    (let ((name (symbol-name last-command-event)))
	      (cond
	       ((string= name "up")
		(playtriangle ?\C-p))
	       ((string= name "down")
		(playtriangle ?\C-n))
	       ((string= name "left")
		(playtriangle ?\C-b))
	       ((string= name "right")
		(playtriangle ?\C-f))

	       (t
		(playtriangle (string-to-number name))))))
	
	(if (integerp last-command-event)
	    (cond
	     ; backspace
	     ((eq last-command-event 127)
	      (playdrum 25))
					; space
	     ((eq last-command-event 32)
	      (playdrum 2))
	      		; enter
	     ((eq last-command-event 13)
	      (playdrum 12))
	     
	     ((and (>= last-command-event 97) (<= last-command-event 122))
		   (playsine last-command-event))
	     ((and (>= last-command-event 65) (<= last-command-event 90))
	      (sound-wav-play (concat soundpath "/square" (number-to-string (gethash (+ last-command-event 32) alphabetmap)) "_25" ".wav")))
	     
	     (t
	      (playtriangle last-command-event)))))))

(add-hook 'post-command-hook 'makesound)

(defun toggle-esoundify ()
  (interactive)
  (if esoundifyonp
      (setq esoundifyonp nil)
    (setq esoundifyonp t)))
