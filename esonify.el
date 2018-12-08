
(setq debug-on-error t)
(defvar esoundifyonp t "if esoundify is on")

(defconst soundpath (expand-file-name "~/esonify/sounds"))
(print soundpath)
(defun makesound ()
  (if esoundifyonp
      (progn
	(if (integerp last-command-event)
	    (cond ((and (>= last-command-event 97) (<= last-command-event 127))
		   (sound-wav-play (concat soundpath "/sine" (number-to-string (- last-command-event 96)) "_1" ".wav")))
		  ((and (>= last-command-event 65) (<= last-command-event 90))
		   (sound-wav-play (concat soundpath "/square" (number-to-string (- last-command-event 64)) "_25" ".wav"))))))))


(add-hook 'post-command-hook 'makesound)

(defun toggle-esoundify ()
  (interactive)
  (if esoundifyonp
      (setq esoundifyonp nil)
    (setq esoundifyonp t)))
