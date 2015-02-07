(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;
;; External repos ;;
;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-list
      '(ace-jump-mode
        ace-window
        auto-complete
        elscreen
        debbugs
        magit
        git-rebase-mode
        git-commit-mode
        dash
        dash-functional
        s
        f
        rust-mode
        paredit
        julia-mode
	multiple-cursors
        apel
        slime
        ggtags
        bookmark+
        cyberpunk-theme
        rainbow-delimiters
        pretty-lambdada
        web-server))

(setq package-archives
      '(
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("elpa" . "http://tromey.com/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
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
(load-theme 'cyberpunk 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load escreen extension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define-key global-map (kbd "C-c j") 'ace-jump-mode)

;;;;;;;;;;;;;;;;
;; Ace Window ;;
;;;;;;;;;;;;;;;;
(require 'ace-window)
(global-set-key (kbd "C-c p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?g ?h ?q ?w ?e ?r ?u ?i ?o ?p ?z ?x ?c ?v ?m ?b ?n))

;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursors ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;;;;;;;;;;;;
;; HL line ;;
;;;;;;;;;;;;;
(require 'hl-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Custom el files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'defuns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters Mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (dot . t)
   (emacs-lisp . t)
   (python . t)
   (perl . t)
   (ruby . t)
   (sh . t)
   (awk . t)))

;;;;;;;;;;;;;;;;;
;; RePipe Mode ;;
;;;;;;;;;;;;;;;;;
(require 'repipe)

;;;;;;;;;;;;;;;;;;;;
;; Pretty Lambdas ;;
;;;;;;;;;;;;;;;;;;;;
(pretty-lambda-for-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/bin/sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GG tags setup               ;;
;; Note: Should have exuberant ;;
;;       ctags and pygments    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook
          (lambda ()
            (ggtags-mode 1)))

;;;;;;;;;;;;;;;;;;;
;; Paredit setup ;;
;;;;;;;;;;;;;;;;;;;
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'ielm-mode-hook
               'scheme-mode-hook
               ))
  (add-hook hook #'(lambda () (paredit-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Random config         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bookmark+)
(column-number-mode 1)
(put 'erase-buffer 'disabled nil)
(setq enable-recursive-minibuffers t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add local configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((local-init-file (expand-file-name "local-init.el" user-emacs-directory)))
  (if (file-exists-p local-init-file)
      (load-file local-init-file)))
