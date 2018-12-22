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

(defvar esonify-start-delay 0.4)
(defvar esonify-read-speed 0.1)

(defvar esoundifyonp t "if esoundify is on")

(defconst esonify--el-source-dir
  (file-name-directory (or load-file-name (buffer-file-name))))
  
(defconst soundpath (expand-file-name "./sounds/" esonify--el-source-dir))

(defconst alphabetmap #s(hash-table size 26 data (122 0 120 1 99 2 118 3 98 4 110 5 109 6 97 7 115 8 100 9 102 10 103 11 104 12 106 13 107 14 108 15 113 16 119 17 101 18 114 19 116 20 121 21 117 22 105 23 111 24 112 25 )))

(defun playdrum(num)
  (sound-wav-play (concat soundpath "/drum_" (number-to-string num) ".wav")))

(defun playsine(num)
  (sound-wav-play (concat soundpath "/sine" (number-to-string (gethash num alphabetmap)) "_1" ".wav")))

(defun playtriangle(num)
  (sound-wav-play (concat soundpath "/triangle" (number-to-string (% num 37)) "_30.wav")))

(defvar linetoprocess nil)

(defvar currenttimer nil)

(defun processline ()
  (if (> (length linetoprocess) 0)
      (progn
	(processchar (string-to-char linetoprocess))
	(setq linetoprocess (substring linetoprocess 1))
	(setq currenttimer (run-at-time esonify-read-speed nil 'processline)))))

(defun processchar (c)
  (cond
					; backspace
   ((eq c 127)
    (playdrum 25))
					; space
   ((eq c 32)
    (playdrum 2))
					; enter
   ((eq c 13)
    (playdrum 12))
   
   ((and (>= c 97) (<= c 122))
    (playsine c))
   ((and (>= c 65) (<= c 90))
    (sound-wav-play (concat soundpath "/square" (number-to-string (gethash (+ c 32) alphabetmap)) "_25" ".wav")))
   
   (t
    (playtriangle c))))
   
(defun makesound ()
  (if esoundifyonp
      (progn
					; set up processing the current line
	(if
	    (timerp currenttimer)
	    (cancel-timer currenttimer))
	(setq linetoprocess (thing-at-point 'line t))
	(setq currenttimer (run-at-time esonify-start-delay nil 'processline))
	
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
	    (processchar last-command-event)))))

;;;###autoload
(define-minor-mode esonify-mode
  "esonify mode toggle"
  :group 'esonify
  :global t
  (if esonify-mode
      (add-hook 'post-command-hook 'makesound)
    (remove-hook 'post-command-hook 'makesound)))





