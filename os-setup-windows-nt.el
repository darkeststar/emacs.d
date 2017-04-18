;;;;;;;;;;;;;;;;;;;;;;
;; Windows nonsense ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Make emacs quiet, without this emacs can be very annoying
(setq visible-bell t)

;; Workaround for now
(defun ms-slime ()
  (interactive)
  (slime slime-path))

(with-current-buffer "*scratch*"
  (cd "~"))
