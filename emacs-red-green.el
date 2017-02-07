(require 's)

(defvar red-green--foreground-color "snow1")
(defvar red-green--foreground-inactive-color "snow3")
(defvar red-green--red-color "firebrick")
(defvar red-green--green-color "green4")
(defvar emacs-red-green--output-since-last-command "")
(defvar emacs-red-green--buffer-name "guard")

(defun emacs-red-green--process-filter (output)
  "The process filter on the servers buffer, gives OUTPUT."
  (when (string= emacs-red-green--buffer-name (buffer-name))
    (setq emacs-red-green--output-since-last-command (concat emacs-red-green--output-since-last-command output))
    (emacs-red-green--process-output-while-full-chunk-exists)))

(defun emacs-red-green--has-full-chunk ()
  (string-match "[0-9]+ examples?, \\([0-9]+\\) failures?"  emacs-red-green--output-since-last-command))

(defun emacs-red-green--get-and-remove-full-chunk ()
  (if (emacs-red-green--has-full-chunk)
      (progn
        (let ((matched-chunk (match-string 0 emacs-red-green--output-since-last-command)))
          (setq emacs-red-green--output-since-last-command (s-trim-left (substring emacs-red-green--output-since-last-command (length matched-chunk))))
          matched-chunk))
    nil))

(defun emacs-red-green--process-output-while-full-chunk-exists ()
  (while (emacs-red-green--has-full-chunk)
    (emacs-red-green--process-chunk (emacs-red-green--get-and-remove-full-chunk))))

(defun tests-are-passing()
  (change-mode-line-color red-green--green-color))

(defun tests-are-failing()
  (change-mode-line-color red-green--red-color))

(defun change-mode-line-color(color)
  (set-face-foreground 'mode-line red-green--foreground-color)
  (set-face-foreground 'mode-line-inactive red-green--foreground-inactive-color)
  (set-face-background 'mode-line color)
  (set-face-background 'mode-line-inactive color))


(defun emacs-red-green--process-chunk (chunk)
  (string-match "[0-9]+ examples?, \\([0-9]+\\) failures?" chunk)
  (if (string= (match-string 1 chunk) "0")
      (tests-are-passing)
    (tests-are-failing)))

(define-minor-mode red-green-mode
  "Get your foos in the right places."
  :lighter " red-green"
  (if (bound-and-true-p red-green-mode)
      (add-hook 'comint-output-filter-functions 'emacs-red-green--process-filter)
    (remove-hook 'comint-output-filter-functions 'emacs-red-green--process-filter)))
