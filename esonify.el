

(defvar esoundifyonp t "if esoundify is on")

(defconst soundpath (expand-file-name "~/esonify/sounds"))

(defconst alphabetmap #s(hash-table size 26 data (122 0 120 1 99 2 118 3 98 4 110 5 109 6 97 7 115 8 100 9 102 10 103 11 104 12 106 13 107 14 108 15 113 16 119 17 101 18 114 19 116 20 121 21 117 22 105 23 111 24 112 25 )))

(defun makesound ()
  (if esoundifyonp
      (progn
	(if (integerp last-command-event)
	    (cond
	     ((eq last-command-event 127)
	      ;do nothing for delete key for now
	      t)
	     ((and (>= last-command-event 97) (<= last-command-event 126))
		   (sound-wav-play (concat soundpath "/sine" (number-to-string (gethash last-command-event alphabetmap)) "_1" ".wav")))
	     ((and (>= last-command-event 64) (<= last-command-event 90))
	      (sound-wav-play (concat soundpath "/square" (number-to-string (gethash (+ last-command-event 32) alphabetmap)) "_25" ".wav")))
	     (t
	      (sound-wav-play (concat soundpath "/triangle" (number-to-string (% last-command-event 37)) "_30.wav"))))))))


(add-hook 'post-command-hook 'makesound)

(defun toggle-esoundify ()
  (interactive)
  (if esoundifyonp
      (setq esoundifyonp nil)
    (setq esoundifyonp t)))
