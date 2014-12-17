;; repipe.el -- Like regex-builder just with shell piping

(defvar *repipe-input-buffer* nil
  "Input buffer")

(defvar *repipe-stdout-buffer* nil
  "Input buffer")

(defvar *repipe-stderr-buffer* nil
  "Input buffer")

(defvar *repipe-cmd-buffer* "*repipe-cmd*"
  "Workspace buffer")

(defun repipe ()
  "Kick off repipe"
  (interactive)
  (switch-to-buffer *repipe-cmd-buffer*)
  (repipe-mode)
  ;; Initialization
  (setq *repipe-input-buffer* (get-buffer-create "*repipe-input-buffer*")
        *repipe-stdout-buffer* (get-buffer-create "*repipe-stdout-buffer*")
        *repipe-stderr-buffer* (get-buffer-create "*repipe-stderr-buffer*")))

(define-derived-mode repipe-mode nil "repipe"
  (define-key repipe-mode-map (kbd "C-c C-c") 'repipe-rerun-cmd))

(defun repipe-rerun-cmd ()
  "Re-run the buffer command"
  (interactive)
  (with-current-buffer *repipe-stderr-buffer*
    (erase-buffer))
  (with-current-buffer *repipe-stdout-buffer*
    (erase-buffer))
  (with-current-buffer *repipe-input-buffer*
    (shell-command-on-region (point-min)
                             (point-max)
                             (with-current-buffer *repipe-cmd-buffer*
                               (buffer-string))
                             *repipe-stdout-buffer*
                             nil
                             *repipe-stderr-buffer*
                             nil)))

(provide 'repipe)

