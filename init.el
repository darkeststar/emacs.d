(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;;;;;;;;;;;;;;;;
;; External repos ;;
;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-list
      '(ace-jump-mode
        debbugs
        magit
        git-rebase-mode
        git-commit-mode
        dash
        s
        f
        elnode
        rust-mode
        julia-mode
        apel))

(setq package-archives
      '(
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("elpa" . "http://tromey.com/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("e6h" . "http://www.e6h.org/packages/")
        ))

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance Related config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq display-time-day-and-date t)
(display-time)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq inhibit-startup-screen t)
(load-theme 'wombat 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load escreen extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "elscreen")
(elscreen-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          IDO config           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)


;;;;;;;;;;;;;;
;; Uniquify ;;
;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator "|")
;; (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
;; (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;;;;;;;;;;;;;;;;
;; Winner mode ;;
;;;;;;;;;;;;;;;;;
(when (fboundp 'winner-mode)
      (winner-mode 1))

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;
(require 'dired-x)
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;;;;;;;;;;;;;;;;;;;
;; Ace Jump Mode ;;
;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-;") 'ace-jump-mode)

;;;;;;;;;;;;;
;; HL line ;;
;;;;;;;;;;;;;
(require 'hl-line)
(set-face-background 'hl-line "#050")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Random config         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(column-number-mode 1)
(put 'erase-buffer 'disabled nil)
(setq enable-recursive-minibuffers t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Experimental          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((local-init-file "~/.emacs.d/local-init.el"))
  (if (file-exists-p local-init-file)
      (load-file local-init-file)))
