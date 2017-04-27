(require 's)
;;; Code:

(defvar red-green--foreground-color "snow1")
(defvar red-green--foreground-inactive-color "snow3")
(defvar red-green--red-color "firebrick")
(defvar red-green--green-color "green4")
(defvar red-green--output-since-last-command "")
(defvar red-green--buffer-name "guard")
(defvar red-green--regexes '(
                             "[0-9]+ examples?, \\([0-9]+\\) failures?"
                             "chrome [0-9]+ ([0-9]+/[0-9]+/\\([0-9]+\\))"
                             ))

(defvar red-green--pass-regexes '("TOTAL: [0-9]+ SUCCESS"))
(defvar red-green--fail-regexes '("TOTAL: [0-9]+ FAILED, [0-9]+ SUCCESS"))

(defun red-green--process-filter (output)
  "The process filter on the servers buffer, gives OUTPUT."
  (when (string= red-green--buffer-name (buffer-name))
    (setq red-green--output-since-last-command (concat red-green--output-since-last-command output))
    (trim-overflowing-output-buffer)
    (red-green--process-output-while-full-chunk-exists)))

(defun trim-overflowing-output-buffer ()
  (if (>= (length red-green--output-since-last-command) 200)
      (setq red-green--output-since-last-command
            (substring red-green--output-since-last-command 100 nil))))

(defun red-green--all-regexes ()
  "Return all regexes"
  (append red-green--regexes red-green--pass-regexes red-green--fail-regexes))

(defun red-green--has-full-chunk ()
  "Check that we have a chunk."
  (let ((has-chunk nil))
    (dolist (regex (red-green--all-regexes))
      (if (string-match regex  red-green--output-since-last-command)
          (setq has-chunk t)))
    has-chunk))

(defun red-green--get-and-remove-full-chunk ()
  "Return the full chunk."
  (if (red-green--has-full-chunk)
      (progn
        (let ((matched-chunk (match-string 0 red-green--output-since-last-command)))
          (setq red-green--output-since-last-command (s-trim-left (substring red-green--output-since-last-command (length matched-chunk))))
          matched-chunk))
    nil))

(defun red-green--process-output-while-full-chunk-exists ()
  "Keep processing chunks as long as we have them."
  (while (red-green--has-full-chunk)
    (red-green--process-chunk (red-green--get-and-remove-full-chunk))))

(defun tests-are-passing()
  (change-mode-line-color red-green--green-color))

(defun tests-are-failing()
  (change-mode-line-color red-green--red-color))

(defun change-mode-line-color(color)
  (set-face-foreground 'mode-line red-green--foreground-color)
  (set-face-foreground 'mode-line-inactive red-green--foreground-inactive-color)
  (set-face-background 'mode-line color)
  (set-face-background 'mode-line-inactive color))


(defun red-green--process-chunk (chunk)
  "Process a CHUNK."
  (red-green--run-full-regexes chunk)
  (red-green--run-pass-regexes chunk)
  (red-green--run-fail-regexes chunk))

(defun red-green--run-pass-regexes (chunk)
  "Check pass regexes for CHUNK."
  (dolist (regex red-green--pass-regexes)
    (if (string-match regex chunk)
        (tests-are-passing))))

(defun red-green--run-fail-regexes (chunk)
  "Check fail regexes for CHUNK."
  (dolist (regex red-green--fail-regexes)
    (if (string-match regex chunk)
        (tests-are-failing))))

(defun red-green--run-full-regexes (chunk)
  "Check full regexes for CHUNK."
  (dolist (regex red-green--regexes)
    (if (string-match regex chunk)
        (progn
          (if (string= (match-string 1 chunk) "0")
              (tests-are-passing)
            (tests-are-failing))))))

(define-minor-mode red-green-mode
  "Get your foos in the right places."
  :lighter " red-green"
  (if (bound-and-true-p red-green-mode)
      (add-hook 'comint-output-filter-functions 'red-green--process-filter)
    (remove-hook 'comint-output-filter-functions 'red-green--process-filter)))

(provide 'red-green)
;;; red-green ends here
