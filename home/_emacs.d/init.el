;; require package managers, but we'll avoid them if at all possible
(require 'package)

(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(push '("org" . "http://orgmode.org/elpa/") package-archives)

;;=====================================
;;plugins
;;=====================================

(setq package-list '(
   cider
   auto-complete
   scala-mode2
   sbt-mode
   exec-path-from-shell
   evil
   evil-leader
   color-theme
   ido-vertical-mode
   flx-ido
   osx-clipboard
   expand-region
   emmet-mode
   full-ack
   web-mode
   helm
   helm-projectile
   zencoding-mode
   company
   magit
   fsharp-mode
   2048-game
   omnisharp
   js2-mode
   avy
   markdown-mode
   flycheck
   json-mode
   diminish
   hl-line
   hl-line+
   flymake-ruby
   scss-mode
   jsx-mode
   projectile))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load "~/.emacs.d/evil-tmux-navigator/navigate.el")
(load "~/.emacs.d/railscasts-theme/railscasts-theme.el")

(require 'auto-complete)
(require 'exec-path-from-shell)
(require 'evil)
(require 'evil-leader)
(require 'navigate)
(require 'color-theme)
(require 'ido)
(require 'ido-vertical-mode)
(require 'flx-ido)
(require 'projectile)
(require 'osx-clipboard)
(require 'expand-region)
(require 'helm)
(require 'helm-config)
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
(require 'diminish)
(require 'hl-line)
(require 'hl-line+)
(require 'omnisharp)
(require 'company)
(require 'flymake-ruby)
(require 'scss-mode)
(require 'jsx-mode)


;;=====================================
;;highlight buffer when idle
;;=====================================
(toggle-hl-line-when-idle)
(hl-line-when-idle-interval 5)

;;=====================================
;;mode line changes to hide minor modes I don't care about
;;=====================================
(diminish 'projectile-mode "")
(diminish 'osx-clipboard-mode "")
(diminish 'undo-tree-mode "")

;;=====================================
;;general configuration
;;===================================== song_id, delay
(menu-bar-mode -1) ;;remove menu bar
(define-key global-map (kbd "RET") 'newline-and-indent) ;;auto indent on new line
(setq-default truncate-lines t)
(setq visible-bell 1)

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
  "m" 'amir/split-and-find
  "g" 'projectile-find-file
  "t" 'amir/touch
  "a" 'amir/foo-inline
  "f" 'next-error
  "d" 'previous-error
  "b" 'ido-switch-buffer
  "." 'ido-dired
  "s" 'magit-status
  "h" 'vc-print-log
  ")" 'next-buffer
  "(" 'previous-buffer
  "p" 'amir/previous-search-to-top
  "o" 'flycheck-list-errors
  "`" 'amir/resize-window-dwim
  "TAB" 'amir/resize-equal
  "q" 'amir/insert-file-name
  "2" 'amir/resize-window-vertical+
  "3" 'amir/resize-window-vertical-
  "1" 'amir/resize-window-horizontal-
  "4" 'amir/resize-window-horizontal+
  "x" 'amir/write-quit
  "i" 'install-packages)

(defun amir/edit-init-el ()
  (interactive)
  (evil-window-vnew nil "~/.emacs.d/init.el"))

(defun amir/reload-init-el ()
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun amir/split-and-find ()
  (interactive)
  (evil-window-vsplit)
  (projectile-find-file))

(defun amir/eval-dwim ()
  (interactive)
    (pcase major-mode
      (`clojure-mode (cider-eval-defun-at-point))
      (`fsharp-mode
       (progn
         (fsharp-eval-region (point) (mark))
         (keyboard-quit)))
      (_ (eval-last-sexp nil))))

(defun amir/write-quit ()
  (interactive)
  (evil-save nil t)
  (kill-buffer-and-window))

;;=====================================
;; avy
;;=====================================
(setq avy-styles-alist '((avy-goto-word-0 . at-full) (avy-goto-line . at-full)))
(setq avy-all-windows nil)

;;=====================================
;; evil configuration
;;=====================================
(evil-mode 1)

;; (eval-after-load "evil-maps"
;;   (dolist (map '(evil-motion-state-map
;;                  evil-insert-state-map
;;                  evil-emacs-state-map))
;;     (define-key (eval map) "\C-n" 'company-complete-selection)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun amir/auto-complete-or-default ()
  (interactive)
  (company-complete-selection)
  (company-complete)
  (self-insert-command 1))

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
 '(custom-variable-tag ((t (:foreground "cyan" :weight bold))))
 '(diff-added ((t (:inherit diff-changed :background "#ddffdd" :foreground "black"))))
 '(diff-header ((t (:background "grey80" :foreground "black"))))
 '(diff-removed ((t (:inherit diff-changed :background "#ffdddd" :foreground "black"))))
 '(flymake-errline ((t (:background "color-52" :foreground "white"))))
 '(flymake-warnline ((t (:background "yellow" :foreground "white"))))
 '(hl-line ((t (:background "brightblack"))))
 '(jabber-activity-personal-face ((t (:foreground "red" :weight bold))))
 '(js2-external-variable ((t (:foreground "color-136"))))
 '(js2-function-param ((t (:foreground "color-81"))))
 '(lazy-highlight ((t (:background "black" :foreground "white" :underline t))))
 '(magit-diff-added ((t (:background "black" :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "black" :foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:background "black" :foreground "brightgreen"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "black" :foreground "white"))))
 '(magit-diff-removed ((t (:background "brightblack" :foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:background "black" :foreground "#aa2222"))))
 '(magit-section-highlight ((t (:background "black"))))
 '(mode-line ((t (:background "color-130" :foreground "white" :box nil))))
 '(mode-line-buffer-id ((t (:background "brightred" :foreground "white"))))
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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;;=========================================
;;jsx linter
;;========================================
(add-to-list 'auto-mode-alist '("\\.jsx$" . jsx-mode))

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
;;ruby syntax highlighting
;;=====================================
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;;=====================================
;; tab width 2
;;=====================================
(setq js-indent-level 2)


;;=====================================
;; auto complete
;;=====================================
;;(global-auto-complete-mode 1)
;;(setq ac-auto-show-menu 0.1)
;;(ac-config-default)

(company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.03)
(setq company-minimum-prefix-length 1)
(setq company-require-match 'nil)
(setq company-show-numbers 't)
(setq omnisharp-company-match-type 'company-match-flx)
(setq gc-cons-threshold 20000000)
(setq company-dabbrev-downcase 'nil)

(defun amir/company-complete-equal-sign ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert " =")))

(defun amir/company-complete-paren ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert "(")))

(defun amir/company-complete-end-paren ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert ")")))

(defun amir/company-complete-. ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert ".")))

(defun amir/company-complete-comma ()
  (interactive)
  (progn
    (company-complete-selection)
    (insert ",")))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map (kbd ".") 'amir/company-complete-.)
     (define-key company-active-map (kbd ",") 'amir/company-complete-comma)
     (define-key company-active-map (kbd "=") 'amir/company-complete-equal-sign)
     (define-key company-active-map (kbd ")") 'amir/company-complete-end-paren)
     (define-key company-active-map (kbd "(") 'amir/company-complete-paren)))


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
;;tail log files
;;=====================================
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;;====================================
;; window sizing
;;====================================
(defun amir/resize-equal ()
  (interactive)
  (balance-windows)
  (redraw-display))

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

(defun amir/resize-window-vertical+ ()
  (interactive)
  (evil-window-set-height (+ (window-height) 10)))

(defun amir/resize-window-vertical- ()
  (interactive)
  (evil-window-set-height (- (window-height) 10)))

(defun amir/resize-window-horizontal+ ()
  (interactive)
  (evil-window-set-width (+ (window-width) 10)))

(defun amir/resize-window-horizontal- ()
  (interactive)
  (evil-window-set-width (- (window-width) 10)))


;;=======================================
;; good for inserting require statements
;; function will add the relative path to
;; selected file
;;=======================================
(defun amir/insert-file-name (filename &optional args)
  (interactive `(,(ido-read-file-name "File Name: ") ,current-prefix-arg))
  (cond ((eq '- args) (insert (expand-file-name filename)))
        ((not (null args)) (insert filename))
        (t (progn
             (evil-append 1)
             (insert (file-relative-name filename))))))

;;=========================================
;; magit stuff
;;=========================================
(setq magit-last-seen-setup-instructions "1.4.0")
