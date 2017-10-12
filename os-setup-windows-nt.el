;;;;;;;;;;;;;;;;;;;;;;
;; Windows nonsense ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Make emacs quiet, without this emacs can be very annoying
(setq visible-bell t)

;; Workaround for now
(defun ms-slime (&optional command coding-system)
  "Start an inferior^_superior Lisp and connect to its Swank server."
  (interactive)
  (slime-setup)
  (slime-start* (list :program inferior-lisp-program
                      :program-args '())))
(with-current-buffer "*scratch*"
  (cd "~"))
