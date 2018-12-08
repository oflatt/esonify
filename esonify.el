

(defvar esoundifyonp t "if esoundify is on")

(defconst soundpath (expand-file-name "~/esonify/sounds"))

(defun makesound ()
  (if esoundifyonp
      (progn
	(if (integerp last-command-event)
	    (cond
	     ((eq last-command-event 127)
	      ;do nothing for delete key for now
	      t)
	     ((and (>= last-command-event 97) (<= last-command-event 126))
		   (sound-wav-play (concat soundpath "/sine" (number-to-string (- last-command-event 96)) "_1" ".wav")))
		  ((and (>= last-command-event 65) (<= last-command-event 90))
		   (sound-wav-play (concat soundpath "/square" (number-to-string (- last-command-event 64)) "_25" ".wav")))
		  (t
		   (sound-wav-play (concat soundpath "/triangle" (number-to-string (% last-command-event 37)) "_30.wav"))))))))


(add-hook 'post-command-hook 'makesound)

(defun toggle-esoundify ()
  (interactive)
  (if esoundifyonp
      (setq esoundifyonp nil)
    (setq esoundifyonp t)))
