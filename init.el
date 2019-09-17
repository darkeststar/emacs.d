(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Hack to get around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Variables local to machine
(setq slime-path nil
      local-inferior-lisp-program nil)
 
(let ((local-init-file (expand-file-name "local-vars.el" user-emacs-directory)))
  (if (file-exists-p local-init-file)
      (load-file local-init-file)))

;; Setup OS-relates constraints
(setq use-ggtags (not (eq system-type 'windows-nt)))

;;;;;;;;;;;;;;;;;;;;
;; External repos ;;
;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-list
      '(ace-jump-mode
        ace-window
        auto-complete
        elscreen
        magit
        dash
        dash-functional
        s
        f
        php-mode
        password-generator
        ninja-mode
        json-mode
        inf-mongo
        gdscript-mode
        projectile
        restclient
        realgud
        realgud-lldb
        realgud-ipdb
        rust-mode
        paredit
        julia-mode
	multiple-cursors
        apel
        ggtags
        hy-mode
        undo-tree
        epl
        elnode
        dizzee
        chronos
        ace-flyspell
        docker
        docker-tramp
        dockerfile-mode
        afternoon-theme
        cyberpunk-theme
        rainbow-delimiters
        web-server
        yasnippet
        systemd
        symon
        markdown-mode
        go-mode
        counsel
        counsel-gtags
        counsel-tramp
        counsel-projectile
        swiper
        company
        projectile
        csharp-mode
        fill-column-indicator
        yaml-mode
        hyperbole
        uuidgen
        autumn-light-theme
        ein))

(setq package-archives
      `(
        ("elpa" . "https://tromey.com/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
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
   (shell . t)
   (awk . t)))

;;;;;;;;;;;;;
;; Hy Mode ;;
;;;;;;;;;;;;;
(require 'hy-mode)
(add-hook 'hy-mode-hook #'(lambda () (auto-complete-mode 1)))

;;;;;;;;;;;;;;;;;
;; RePipe Mode ;;
;;;;;;;;;;;;;;;;;
(require 'repipe)

;;;;;;;;;;;;;;;;;;;;
;; Pretty Lambdas ;;
;;;;;;;;;;;;;;;;;;;;
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(when slime-path
  (setq slime-contribs '(slime-fancy))
  (add-to-list 'load-path slime-path)
  (require 'slime-autoloads))

(when local-inferior-lisp-program
  (setq inferior-lisp-program local-inferior-lisp-program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GG tags setup               ;;
;; Note: Should have exuberant ;;
;;       ctags and pygments    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when use-ggtags
  (add-hook 'prog-mode-hook
            (lambda ()
              (ggtags-mode 1))))

;;;;;;;;;;;;;;;;;;;
;; Paredit setup ;;
;;;;;;;;;;;;;;;;;;;
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'ielm-mode-hook
               'scheme-mode-hook
               'hy-mode-hook))
  (add-hook hook #'(lambda () (paredit-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Random config         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bookmark+)
(require 'markdown-mode)
(require 'ein)
(column-number-mode 1)
(put 'erase-buffer 'disabled nil)
(setq enable-recursive-minibuffers t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; Make force these to buffers to open in the same window
(setq same-window-regexps
      (list
       "^\\*shell\\*"))

;;;;;;;;;;;;;;;;;;;
;; Add Undo Tree ;;
;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;
;; Add YASnippets ;;
;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy/Counsel/Swiper ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(ivy-mode 1)
(setq ivy-use-virtual-buffers t
      ivy-count-format "%d/%d ")

(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;
(projectile-mode 1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c r") 'projectile-command-map)
(setq projectile-completion-system 'ivy)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel Projectile ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(counsel-projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; fill column mode ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'fill-column-indicator)
(setq fci-rule-color "darkblue"
      fci-rule-width 1)
(setq-default fci-rule-column 80)

(dolist (mode '(c-mode-hook
                csharp-mode-hook
                emacs-lisp-mode-hook
                go-mode-hook
                lisp-mode-hook))
  (add-hook mode 'fci-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable bell sound ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;
;; Set custom file ;;
;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (expand-file-name ".custom-vars.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs more readable at night
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun night-mode ()
  (interactive)
  (require 'afternoon-theme)
  (set-face-attribute 'region nil :background "#666"))

(defun autumn-mode ()
  (interactive)
  (require 'autumn-light-theme)
  (load-theme 'autumn-light-theme 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   OS Specific setup    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((os-spec-file (concat "os-setup-" (downcase (symbol-name system-type)) ".el"))
       (el-file (expand-file-name os-spec-file user-emacs-directory)))
  (when (file-exists-p el-file)
    (load-file el-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add local configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((local-init-file (expand-file-name "local-init.el" user-emacs-directory)))
  (if (file-exists-p local-init-file)
      (load-file local-init-file)))
