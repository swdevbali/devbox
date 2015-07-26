;; require package managers, but we'll avoid them if at all possible
(require 'package)

(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(push '("org" . "http://orgmode.org/elpa/") package-archives)

;;=====================================
;;plugins
;;=====================================

(setq package-list '(
   exec-path-from-shell
   evil
   evil-leader
   color-theme
   key-chord
   ido-vertical-mode
   flx-ido
   osx-clipboard
   expand-region
   ace-jump-mode
   emmet-mode
   full-ack
   web-mode
   helm
   helm-projectile
   go-mode
   auto-complete
   zencoding-mode
   clojure-mode
   cider
   magit
   fsharp-mode
   2048-game
   js2-mode
   avy
   markdown-mode
   flycheck
   json-mode
   jabber
   diminish
   hl-line
   hl-line+
   persistent-scratch
   projectile))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load "~/.emacs.d/evil-tmux-navigator/navigate.el")
(load "~/.emacs.d/railscasts-theme/railscasts-theme.el")
(load "~/.emacs.d/secret-functions.el")

(require 'exec-path-from-shell)
(require 'evil)
(require 'evil-leader)
(require 'navigate)
(require 'color-theme)
(require 'key-chord)
(require 'ido)
(require 'ido-vertical-mode)
(require 'flx-ido)
(require 'projectile)
(require 'osx-clipboard)
(require 'expand-region)
(require 'ace-jump-mode)
(require 'helm)
(require 'helm-config)
(require 'go-mode)
(require 'auto-complete)
(require 'full-ack)
(require 'zencoding-mode)
(require 'web-mode)
(require 'magit)
(require 'markdown-mode)
(require 'emmet-mode)
(require 'fsharp-mode)
(require 'js2-mode)
(require 'avy)
(require 'flycheck)
(require 'json-mode)
(require 'persistent-scratch)
(require 'jabber)
(require 'diminish)
(require 'hl-line)
(require 'hl-line+)

(setq magit-last-seen-setup-instructions "1.4.0")

;;=====================================
;;highlight buffer when idle
;;=====================================
(toggle-hl-line-when-idle)
;;(hl-line-idle-interval 5)

;;=====================================
;;load jabber stuff
;;=====================================
(setq jabber-account-list `((,(amir/gmail)
                              (:password . ,(amir/gmail-password))
                              (:network-server . "talk.google.com")
                              (:connection-type . ssl))))

(define-jabber-alert echo "Show a message in the echo area"
  (lambda (msg)
    (unless (minibuffer-prompt)
      (message "%s" msg))))

;;=====================================
;;mode line changes to hide minor modes I don't care about
;;=====================================
(diminish 'projectile-mode "")
(diminish 'osx-clipboard-mode "")
(diminish 'undo-tree-mode "")
(diminish 'magit-auto-revert-mode "")
(diminish 'auto-complete-mode "")

;;=====================================
;;general configuration
;;=====================================
(menu-bar-mode -1) ;;remove menu bar
(define-key global-map (kbd "RET") 'newline-and-indent) ;;auto indent on new line
(setq-default truncate-lines t)

;;=====================================
;; backups and scratch buffer
;;=====================================
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(persistent-scratch-setup-default)

;;=====================================
;;evil leader
;;=====================================
(defun amir/touch ()
  "Run touch command on current file."
  (interactive)
  (when buffer-file-name
    (shell-command (concat "touch " (shell-quote-argument buffer-file-name)))
    (clear-visited-file-modtime)))

(defun amir/tab-space-four ()
  "Sets javascript and default tab space to four spaces."
  (interactive)
  (setq js-indent-level 4)
  (setq tab-width 4))

(defun amir/next-search-to-top ()
  "Primarily for presentations, finds next occurence of string and scrolls it to the top"
  (interactive)
  (progn
    (call-interactively (evil-next-search 1))
    (call-interactively (evil-scroll-line-to-top))))

(defun amir/previous-search-to-top ()
  "Primarily for presentations, finds previous occurence of string and scrolls it to the top"
  (interactive)
  (progn
    (evil-previous-search 1)
    (evil-scroll-line-to-top)))

(defun amir/tab-space-two ()
  "Sets javascript and default tab space to two spaces."
  (interactive)
  (setq js-indent-level 2)
  (setq tab-width 2))

(global-evil-leader-mode)

(evil-leader/set-leader ",")

(evil-leader/set-key
  "c" 'comment-dwim
  "e" 'amir/eval-dwim
  "v" 'web-mode
  "V" 'js2-mode
  "K" 'amir/edit-init-el
  "k" 'amir/reload-init-el
  "j" 'avy-goto-line
  "w" 'avy-goto-word-0
  "m" 'evil-window-vsplit
  "g" 'projectile-find-file
  "t" 'amir/touch
  "a" 'amir/foo-inline
  "f" 'next-error
  "d" 'previous-error
  "b" 'ido-switch-buffer
  "." 'ido-dired
  "s" 'magit-status
  "h" 'vc-print-log
  "4" 'amir/tab-space-four
  "2" 'amir/tab-space-two
  "n" 'amir/next-search-to-top
  ")" 'next-buffer
  "(" 'previous-buffer
  "p" 'amir/previous-search-to-top
  "o" 'flycheck-list-errors
  "`" 'amir/resize-window-dwim
  "TAB" 'balance-windows
  "q" 'amir/insert-file-name
  "i" 'install-packages)

(defun amir/edit-init-el ()
  (interactive)
  (evil-window-vnew nil "~/.emacs.d/init.el"))

(defun amir/reload-init-el ()
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun amir/eval-dwim ()
  (interactive)
  (pcase major-mode
    (`clojure-mode (cider-eval-defun-at-point))
    (`fsharp-mode (fsharp-eval-region (point) (mark)))
    (_ (eval-last-sexp nil))))

;;=====================================
;; avy
;;=====================================
(setq avy-styles-alist '((avy-goto-word-0 . at-full) (avy-goto-line . at-full)))
(setq avy-all-windows nil)

;;=====================================
;; evil configuration
;;=====================================
(evil-mode 1)

(define-key evil-normal-state-map [escape] 'keyboard-quit) (define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Remap org-mode meta keys for convenience
(mapcar (lambda (state)
    (evil-declare-key state org-mode-map
      (kbd "M-l") 'org-metaright
      (kbd "M-h") 'org-metaleft
      (kbd "M-k") 'org-metaup
      (kbd "M-j") 'org-metadown
      (kbd "M-L") 'org-shiftmetaright
      (kbd "M-H") 'org-shiftmetaleft
      (kbd "M-K") 'org-shiftmetaup
      (kbd "M-J") 'org-shiftmetadown))
  '(normal insert))

(defun evil-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX")  (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (evil-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(evil-terminal-cursor-change)

;;=====================================
;;key chord
;;=====================================
(key-chord-mode 1)
;;(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;;(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)


;;=====================================
;;color theme configuration
;;=====================================
(color-theme-initialize)

;;=====================================
;;projectile mode
;;=====================================
(projectile-global-mode)
;;(setq projectile-require-project-root nil)

;;=====================================
;;projectile vertical
;;=====================================
(ido-mode 1)
(ido-vertical-mode 1)

(add-hook 'ido-setup-hook 'vim-like-ido-keys)

(defun vim-like-ido-keys ()
  "Add vim like keybindings for ido."
  (define-key ido-completion-map (kbd "J") 'ido-next-match)
  (define-key ido-completion-map (kbd "K") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
)


;;=====================================
;;color changes
;;=====================================

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "cyan" :foreground "black"))))
 '(avy-lead-face-0 ((t (:background "cyan" :foreground "black"))))
 '(avy-lead-face-1 ((t (:background "cyan" :foreground "black"))))
 '(col-highlight ((t (:background "color-233"))))
 '(diff-added ((t (:inherit diff-changed :background "#ddffdd" :foreground "black"))))
 '(diff-header ((t (:background "grey80" :foreground "black"))))
 '(diff-removed ((t (:inherit diff-changed :background "#ffdddd" :foreground "black"))))
 '(hl-line ((t (:background "brightblack"))))
 '(jabber-activity-personal-face ((t (:foreground "red" :weight bold))))
 '(js2-external-variable ((t (:foreground "color-136"))))
 '(js2-function-param ((t (:foreground "color-81"))))
 '(lazy-highlight ((t (:background "black" :foreground "white" :underline t))))
 '(mode-line ((t (:background "color-234" :foreground "brightmagenta" :box nil))))
 '(neo-dir-link-face ((t (:foreground "cyan"))))
 '(neo-file-link-face ((t (:foreground "white"))))
 '(neo-header-face ((t (:foreground "color-33"))))
 '(secondary-selection ((t (:background "color-236"))))
 '(shadow ((t (:foreground "cyan"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-250"))))
 '(web-mode-html-tag-face ((t (:foreground "yellow")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;=====================================
;;flx fuzzy matching
;;=====================================
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode -1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
;(setq ido-use-faces nil)

;;=====================================
;;osx clipboard
;;=====================================
(osx-clipboard-mode 1)

;;=====================================
;;whitespace
;;=====================================

(defun amir/turn-on-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'amir/turn-on-show-trailing-whitespace)

(setq-default indent-tabs-mode nil
              tab-width 2)


(setq css-indent-offset 2)

;;=====================================
;;ensime for scala development
;;=====================================
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;(setenv "PATH" (concat "/usr/local/bin/sbt:" (getenv "PATH")))
;(setenv "PATH" (concat "/usr/local/bin/scala:" (getenv "PATH")))
;
;(when (memq window-system '(mac ns))
;  (exec-path-from-shell-initialize))

(defun amir/ruby-words ()
  (interactive)
  (modify-syntax-entry ?_ "w"))

(add-hook 'ruby-mode-hook 'amir/ruby-words)


;;=====================================
;; Tmux splits
;;=====================================

(defun amir/emacs-or-tmux (dir tmux-cmd)
  (interactive)
  (unless (ignore-errors (funcall (intern (concat "windmove-" dir))))
     (shell-command tmux-cmd)))

(global-set-key (kbd "C-j")
  '(lambda () (interactive) (amir/emacs-or-tmux "up"  "tmux select-pane -U")))
(global-set-key (kbd "C-k")
  '(lambda () (interactive) (amir/emacs-or-tmux "down"  "tmux select-pane -D")))
(global-set-key (kbd "C-l")
  '(lambda () (interactive) (amir/emacs-or-tmux "right" "tmux next-window")))
(global-set-key (kbd "C-h")
  '(lambda () (interactive) (amir/emacs-or-tmux "left"  "tmux previous-window")))

;;========================================
;;auto mode list for different file types
;;========================================
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;=========================================
;;jsx linter
;;========================================
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))


;;=====================================
;; tab width 2
;;=====================================
(setq js-indent-level 2)


;;=====================================
;; auto complete
;;=====================================
(global-auto-complete-mode 1)
(setq ac-auto-show-menu 0.1)
(ac-config-default)


;;=====================================
;; shell lines out
;;=====================================
(defun amir/foo-inline(cmd)
  (interactive)
  (shell-command-on-region (point) (mark) cmd nil t))

(defun amir/foo-above(cmd)
  (interactive)
  (let* ((buffer (generate-new-buffer "*shell-command*"))
         (output (progn
                   (shell-command-on-region (point) (mark) cmd buffer)
                   (with-current-buffer buffer
                     (buffer-string)))))
    (save-excursion
      (goto-char (min (point) (mark)))
      (insert output))))

(defun amir/foo-kill-ring(cmd)
  (interactive)
  (kill-new
   (let ((buffer (generate-new-buffer "*shell-command*")))
     (shell-command-on-region (point) (mark) cmd buffer)
     (with-current-buffer buffer
       (buffer-string))))
  (message "Output copied to kill ring"))

;;=====================================
;;change auto save directory
;;=====================================
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))


;;=====================================
;;tail log files
;;=====================================

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;;====================================
;; window sizing
;;====================================
(defun amir/resize-window-dwim ()
  (interactive)
  (let ((maximize (< (window-width) (/ (frame-width) 2))))
    (cond ((< (window-width) 80) (evil-window-set-width 80))
          (maximize
           (progn
             (evil-window-set-width (frame-width))
             (evil-window-set-height (frame-height))
             (redraw-display)))
          ((not maximize) (evil-window-set-width 80)))))

;;=======================================
;; good for inserting require statements
;; function will add the relative path to
;; selected file
;;=======================================
(defun amir/insert-file-name (filename &optional args)
  (interactive `(,(ido-read-file-name "File Name: ")
                 ,current-prefix-arg))
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;;=======================================
;; save on lose focus,
;;=======================================
(add-hook 'focus-out-hook 'save-buffer)
