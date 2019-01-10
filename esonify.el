;;; esonify.el --- Sonify your code

;; Copyright (C) 2018 by Oliver Flatt

;; Author: Oliver Flatt <oflatt@gmail.com>
;; URL: https://github.com/oflatt/esonify
;; Version: 0.0
;; Package-Requires: ((deferred "0.3.1") (cl-lib "0.5"))

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

;; M-x esonify-mode
;; An emacs extension that sonifies your code. Skilled users will begin to be able to recognize some code based solely upon the music it generates.
;; Hear sine waves at different frequencies for lowercase letters, square waves for upper case letters, and triangle waves for keybindings.

;;; Code:

(defcustom esonify-play-curr-line-delay .4
  "If non-nil set how long to wait before playing the current line. If nil then there will be no playback of the current line."
  :group 'esonify)

(defcustom esonify-read-speed .1
  "Set how long to wait before playing consecutive characters in the current line."
  :group 'esonify)

(defgroup esonify nil
  "Sonify your code"
  :group 'Multimedia)

(defconst esonify--el-source-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Stores the source dir where esonify is stored locally.")

(require 'esonify-sound-wav)

(defconst esonify--soundpath
  (expand-file-name "./sounds/" esonify--el-source-dir)
  "Sores the path to the sounds.")

(defconst esonify--alphabet-map
  #s(hash-table size 26 data (122 0 120 1 99 2 118 3 98 4 110 5 109 6 97 7 115 8 100 9 102 10 103 11 104 12 106 13 107 14 108 15 113 16 119 17 101 18 114 19 116 20 121 21 117 22 105 23 111 24 112 25 ))
  "Maps ascii values from the ascii value for a lowercase letter to the order in which the character appears on a keyboard.")

(defun esonify--play-drum(num)
  "Plays one drum sound with pitch NUM."
  (esonify--sound-wav-play-inturrupt (concat esonify--soundpath "/drum_" (number-to-string num) ".wav")))

(defun esonify--play-sine(num)
  "Plays one sine sound with pitch NUM."
  (esonify--sound-wav-play-inturrupt (concat esonify--soundpath "/sine" (number-to-string (gethash num esonify--alphabet-map)) "_1" ".wav")))

(defun esonify--play-triangle(num)
  "Plays one triangle sound with pitch NUM."
  (esonify--sound-wav-play-inturrupt (concat esonify--soundpath "/triangle" (number-to-string (% num 37)) "_30.wav")))

(defvar esonify--line-to-process nil)

(defvar esonify--current-timer nil)

(defun esonify--process-line ()
  "Processes one line of text stored in 'esonify--line-to-process'."
  
  (if (> (length esonify--line-to-process) 0)
      (progn
	(esonify--processchar (string-to-char esonify--line-to-process))
	(setq esonify--line-to-process (substring esonify--line-to-process 1))
	(setq esonify--current-timer (run-at-time esonify-read-speed nil 'esonify--process-line)))))

(defun esonify--processchar (c)
  "Plays the sound corresponding to the char C."
  (cond
					; backspace
   ((eq c 127)
    (esonify--play-drum 25))
					; space
   ((eq c 32)
    (esonify--play-drum 2))
					; enter
   ((eq c 13)
    (esonify--play-drum 12))
   
   ((and (>= c 97) (<= c 122))
    (esonify--play-sine c))
   ((and (>= c 65) (<= c 90))
    (esonify--sound-wav-play-inturrupt (concat esonify--soundpath "/square" (number-to-string (gethash (+ c 32) esonify--alphabet-map)) "_25" ".wav")))
   
   (t
    (esonify--play-triangle c))))

(defun esonify--makesound ()
  "Plays the character last typed and start up processing the current line."
  (progn
					; set up processing the current line
    (if (timerp esonify--current-timer)
	(cancel-timer esonify--current-timer))
    (setq esonify--line-to-process (thing-at-point 'line t))
    (if esonify-play-curr-line-delay
	(setq esonify--current-timer (run-at-time esonify-play-curr-line-delay nil 'esonify--process-line)))
    
					; make arrows the same as movement commands
    (if (symbolp last-command-event)
	(let ((name (symbol-name last-command-event)))
	  (cond
	   ((string= name "up")
	    (esonify--play-triangle ?\C-p))
	   ((string= name "down")
	    (esonify--play-triangle ?\C-n))
	   ((string= name "left")
	    (esonify--play-triangle ?\C-b))
	   ((string= name "right")
	    (esonify--play-triangle ?\C-f))

	   (t
	    (esonify--play-triangle (string-to-number name))))))
    
    (if (integerp last-command-event)
	(esonify--processchar last-command-event))))

;;;###autoload
(define-minor-mode esonify-mode
  "esonify mode toggle"
  :group 'esonify
  :global t
  :require 'esonify
  (if esonify-mode
      (add-hook 'post-command-hook 'esonify--makesound)
    (remove-hook 'post-command-hook 'esonify--makesound)))

(provide 'esonify)

;;; esonify.el ends here
