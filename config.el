
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(server-start)
(setq confirm-kill-emacs nil)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Manohar M"
      user-mail-address "mm@linkedin.com")


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(menu-bar-mode -1)
(setq line-spacing 6)
(setq doom-font (font-spec :family "Fira Code" :size 16 )

      doom-big-font (font-spec :family "SF Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "Avenir Next" :size 18)
      doom-variable-pitch-font (font-spec :family "Avenir Next" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'spacemacs-dark)
(after! doom-themes (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
                          doom-themes-enable-italic nil)) ; if nil, italics is universally disabled




;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(global-so-long-mode 1)
(global-visual-line-mode t)
(setq-default cursor-type '(bar . 4))
(set-cursor-color "#66fc03")
(blink-cursor-mode t)
(setq select-enable-primary nil)
(beacon-mode t)
(setq beacon-color "#00FF00")
;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xclip-mode 1)
  (xterm-mouse-mode 1)
  (solaire-mode nil)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(setq xterm-set-window-title t)
(setq doom-unreal-buffer-functions '(minibufferp))

(load! "lisp/prot-eshell")
(define-key eshell-mode-map (kbd "s-<mouse-1>") #'prot-eshell-ffap-find-file)

(setq python-shell-interpreter "python3")

(add-to-list 'default-frame-alist '(right-divider-width . 0))

(setq ivy-height 14)

;; org manipulation functions
(setq org-directory "~/notes/")

(setq deft-directory "~/notes/"
      deft-extensions '("org" "md" "txt")
      deft-default-extension "org"
      deft-recursive nil
      deft-new-file-format "%Y%m%d_notes")

(defun mm/append-dt (file)
  concat (format-time-string "%Y%m%d_") file )

(defun mm/get-journal-file-monthly ()
  "Return filename for this months's journal entry."
  (let ((monthly-name (format-time-string "%Y%m"))
        )
    (expand-file-name (concat  "~/notes/journal/" (format "%s.org" monthly-name)))))


(defun swiper-all-buffer-p (buffer)
  (with-current-buffer buffer
    (not (eq major-mode 'minibuffer-inactive-mode))))

(defun mm/create-notes-file ()
  "Create an org file in ~/notes/."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name (format "%s.org"
                              name) "~/notes/")))

(defun mm/toggle-weekly-todo ()
  (interactive)
  (if (string-equal (buffer-name) "todo.org")   (previous-buffer) (find-file "~/notes/todo.org")))

(defun mm/toggle-eshell()
  (interactive)
  (if (string-equal (buffer-name) "*eshell*")   (previous-buffer) (eshell)))

(defun mm/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))




(after! org
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "INPROGRESS(p)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (setq org-journal-file-type 'monthly)
  (setq org-image-actual-width nil)
  (setq org-export-with-toc nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-author nil)
  (setq org-capture-templates '(("t" "Quick TODO" entry (file+headline  +org-capture-todo-file
                                                                        "Inbox") "* TODO %i %? \n%a")
                                ("n" "Quick Capture Notes with current selected" entry (file+headline
                                                                                        "notes.org"
                                                                                        "Quick Notes")
                                 "* %U\n** %?\n%i\n\n" )
                                ("N" "Quick Capture Notes with clipboard" entry (file+headline
                                                                                 "notes.org"
                                                                                 "Quick Notes")
                                 "* %U\n** %?\n%c\n\n" )
                                ("j" "Monthly Journal with selection" entry (file mm/get-journal-file-monthly )
                                 "* %U\n** %? \n %i" )
                                ("J" "Monthly Journal with clipboard" entry (file mm/get-journal-file-monthly )
                                 "* %U\n** %?\n%c\n" )
                                ("o" "New Note with selection" entry (file mm/create-notes-file)  "* %? \n  %i ")
                                ("O" "New Note with clipboard" entry (file mm/create-notes-file)  "* %? \n  %c ")
                                )))

(defun mm/new-org-tab ()
  (interactive)
  (progn (centaur-tabs--create-new-empty-buffer)
         (org-mode)))



(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename (concat (make-temp-name (concat (file-name-nondirectory (buffer-file-name)) "_"
                                                 (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "screencapture" nil nil nil "-i" filename)
  (insert (concat "[[./" filename "]]"))
  (org-display-inline-images))



(setq current-date-time-format "%Y%m%d_%H%M%S")
(defun mm/current-time()
  (format-time-string current-date-time-format (current-time)))

(defun org-insert-clipboard-image
    (&optional
     file)
  (interactive "F")
  (progn
    (shell-command (concat "pngpaste " file))
    (insert (concat "[[" file "]]"))
    (org-display-inline-images)))

(defun mm/org-insert-image ()
  "main function for clipboard images"
  (interactive)
  (let ((expected-file
         (concat  (buffer-file-name) "_" (mm/current-time) ".png")))
    (org-insert-clipboard-image expected-file)))


;; window manipulation functions

(defun mm/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window)))) 'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun mm/toggle-main-scratch()
  (interactive)
      (if (string-match (buffer-name) "*scratch*")
        (delete-window)
      (switch-to-buffer-other-window "*scratch*")))


(defun mm/toggle-scratch()
  (interactive)
  (let ((bname (buffer-name)))
    (if (string-match "*doom:scratch*" bname)
        (delete-window)
      (doom/open-project-scratch-buffer))))

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "^build$")
  (add-to-list 'projectile-globally-ignored-directories "^.venv$")
  (setq projectile-indexing-method 'alien))


;; text manipulation  functions

(defun mm/filter-text (command arg)
  "the vscode filter text function .If there is a region use that region , else
    use the whole buffer."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((p (if mark-active (region-beginning)
             (point-min)))
        (m (if mark-active (region-end)
             (point-max))))
    (shell-command-on-region p m command t t)))

(defun mm/replace_all_like_selected(x)
  (interactive "sReplace with: ")
  (let ((ed  (buffer-substring (mark) (point))))
    (replace-string  ed x nil (point-min) (point-max) )))

(defun mm/add_at_start()
  (interactive )
    (let ((p (if mark-active (region-beginning)
             (point-min)))
        (m (if mark-active (region-end)
             (point-max))))

    (replace-regexp "^" (read-string "Enter string to add at start:") nil p m ) )
 )
(defun mm/add_at_end()
  (interactive )
    (let ((p (if mark-active (region-beginning)
             (point-min)))
        (m (if mark-active (region-end)
             (point-max))))

    (replace-regexp "$" (read-string "Enter string to add at end:") nil p m ) )
 )

(defun mm/copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun mm/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; xah lee copy
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
)

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position."
  (interactive)
  (let (x1 x2)
    (setq x1 (point))
    (move-beginning-of-line 1)
    (setq x2 (point))
    (delete-region x1 x2)))

; Here's the code to bind them with emacs's default shortcut keys:

(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)
(global-set-key (kbd "C-k") 'my-delete-line)
(global-set-key (kbd "C-S-k") 'my-delete-line-backward)

(defun mm/savebuffer-and-gotonormalmode()
  (interactive)
  (save-buffer)
  ;; (evil-force-normal-state)
  )

(defun mm/single-up-dir()   (interactive) (find-alternate-file ".."))

;;dired
(global-unset-key (kbd "s-d"))
(global-set-key (kbd "s-d -") 'dired-jump)
(use-package! dired
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind ( ("s-<down>" . dired-single-buffer)
          ("s-<up>" . mm/single-up-dir)
          )
  )

(use-package! centaur-tabs
  :demand
  :config
  (setq 	  centaur-tabs-height 32
	          centaur-tabs-set-icons t
	          centaur-tabs-set-modified-marker t
	          centaur-tabs-show-navigation-buttons t
                  centaur-tabs-active-bar t)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t)
  :bind (("s-]" . centaur-tabs-forward)
         ("s-[" . centaur-tabs-backward)
         ("s-{" . centaur-tabs-move-current-tab-to-left)
         ("s-}" . centaur-tabs-move-current-tab-to-right)
         ("s-n" . centaur-tabs--create-new-empty-buffer)
         ("s-N" . mm/new-org-tab)
         ))


;; key bindings
;; s- keys work only in GUI
;;
;;
(global-set-key (kbd "C-S-k") 'kill-visual-line)
(global-set-key (kbd "C-d") 'mm/duplicate-line)
(global-set-key (kbd "C-s") '+default/search-buffer)
(global-set-key (kbd "C-i") 'better-jumper-jump-forward)
(global-set-key (kbd "C-o") 'better-jumper-jump-backward)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-x") 'counsel-M-x)
;;
;; misc based operations start with s-m .find better way
(global-unset-key (kbd "s-m"))
(global-set-key (kbd "s-m r") 'mm/replace_all_like_selected)
(global-set-key (kbd "s-m f") 'mm/filter-text)
(global-set-key (kbd "s-m s") 'mm/add_at_start)
(global-set-key (kbd "s-m e") 'mm/add_at_end)

;; iedit based op s-i
(global-set-key (kbd "s-i i") 'iedit-mode)
(global-set-key (kbd "s-i r") 'iedit-rectangle-mode)
(global-set-key (kbd "s-i q") 'iedit--quit)
(global-set-key (kbd "s-x") 'kill-region)


;; line based ops
(global-unset-key (kbd "s-l"))
(global-set-key (kbd "s-l l") 'kill-whole-line)
(global-set-key (kbd "s-l k") 'kill-line)
(global-set-key (kbd "s-l g") 'goto-line)
(global-set-key (kbd "s-l c") 'mm/copy-line)
(global-set-key (kbd "s-l d") 'mm/duplicate-line)
(global-set-key (kbd "s-l ]") 'end-of-buffer)
(global-set-key (kbd "s-l [") 'beginning-of-buffer)

;; window
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-o") 'other-window)

;; buffer
(global-unset-key (kbd "s-b"))
(global-set-key (kbd "s-b v") 'previous-buffer)
(global-set-key (kbd "s-b n") 'next-buffer)
(global-set-key (kbd "s-b w") 'switch-to-buffer-other-window)
(global-set-key (kbd "s-b b") 'ivy-switch-buffer)
(global-set-key (kbd "s-b <SPC>") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-k") 'kill-buffer-and-window)
(global-set-key (kbd "M-s-<right>") 'next-buffer)
(global-set-key (kbd "M-s-<left>") 'previous-buffer)
(global-set-key (kbd "C-M-s") 'mm/savebuffer-and-gotonormalmode)
(global-set-key (kbd "s-s") 'mm/savebuffer-and-gotonormalmode)

;; eshell
(global-unset-key (kbd "s-e"))
(global-set-key (kbd "s-e e") 'mm/toggle-eshell)
(global-set-key (kbd "s-e f") 'prot-eshell-ffap-find-file)
(global-set-key (kbd "s-e r") 'prot-eshell-complete-recent-dir)

;; toggle buffer
(global-unset-key (kbd "s-t"))
(global-set-key (kbd "s-t t") 'mm/toggle-main-scratch)
(global-set-key (kbd "s-t d") 'mm/toggle-weekly-todo)
(global-set-key (kbd "s-t e") 'mm/toggle-eshell)
(global-set-key (kbd "s-t p") 'mm/toggle-scratch)
(global-set-key (kbd "s-d d") 'neotree-toggle)
(global-set-key (kbd "s-d f") 'neotree-find)




;; project
(global-unset-key (kbd "s-p"))
(global-set-key (kbd "s-p p") 'projectile-switch-project)
(global-set-key (kbd "s-p [") 'centaur-tabs-counsel-switch-group)
(global-set-key (kbd "s-p o")  'centaur-tabs-open-in-external-application)


;; misc
(global-unset-key (kbd "s-f"))
(global-set-key (kbd "s-f f") '+ivy/projectile-find-file)
(global-set-key (kbd "s-f /") '+ivy/project-search)
(global-set-key (kbd "s-f w") 'projectile-find-file-other-window)
(global-set-key (kbd "s-f .") 'counsel-find-file)
(global-set-key (kbd "s-.") 'counsel-find-file)
(global-set-key (kbd "s-,") '+ivy/switch-buffer)
(global-set-key (kbd "s-f ,") 'find-file-other-window)

(global-set-key (kbd "s-Z") 'undo-fu-only-redo)
(global-unset-key (kbd "TAB"))

;; evil normal state idle
;; (defun evil-normalize-all-buffers ()
;;   "Force a drop to normal state."
;;   (unless (eq evil-state 'normal)
;;     (dolist (buffer (buffer-list))
;;       (set-buffer buffer)
;;       (unless (or (minibufferp)
;;                   (eq evil-state 'emacs))
;;         (evil-force-normal-state)))
;;     (message "Dropped back to normal state in all buffers")))

;; (defvar evil-normal-timer
;;   (run-with-idle-timer 20 t #'evil-normalize-all-buffers)
  ;; "Drop back to normal state after idle for 30 seconds.")

;;(setq  evil-want-C-i-jump nil)
;;(evil-define-key 'normal evil-normal-state-map (kbd "C-n") 'evil-next-line)
;;(evil-define-key 'insert evil-insert-state-map (kbd "C-n") 'evil-next-line)
;;(evil-define-key 'normal evil-normal-state-map (kbd "C-p") 'evil-previous-line)
(setq which-key-idle-delay 1)


(map! :leader
      (:desc "open buffers in project" "e" #'projectile-switch-to-buffer)
      (:desc "toggle weekly todo" "1" #'mm/toggle-weekly-todo)
      (:desc "open buffers in project" "<" #'projectile-switch-to-buffer)
      (:desc "open buffer" "," #'+ivy/switch-buffer)
      (:desc "find file" "<" #'counsel-find-file)
      (:desc "next tab" "<right>" #'centaur-tabs-forward)
      (:desc "previous tab" "<left>" #'centaur-tabs-backward)
      (:prefix-map ("r" . "rectangle")
       :desc "insert rectangle" "i" #'string-insert-rectangle
       :desc "insert rectangle iedit" "e" #'iedit-rectangle-mode
       :desc "replace rectangle" "r" #'replace-rectangle
       :desc "delete rectangle" "d" #'delete-rectangle
       :desc "cut rectangle" "t" #'kill-rectangle
       :desc "paste copied rectangle" "p" #'yank-rectangle
       :desc "copy rectangle area " "c" #'copy-rectangle-as-kill)
      (:prefix-map ("w" . "workspaces/windows")
       :desc "toggle window split" "2" #'mm/toggle-window-split
       :desc "delete other window" "1" #'delete-other-windows
       :desc "delete this window" "1" #'delete-window
       :desc "window swap states" "t" #'window-swap-states
       :desc "winner undo" "u" #'winner-undo
       :desc "winner redo" "r" #'winner-redo)
      (:prefix-map ("d" . "neotree .. ")
       :desc "neotree-toggle-for-project" "d" #'+neotree/open
       :desc "show current file in neotree" "f" #'neotree-find)
      (:prefix-map ("z" . "folds .. ")
       :desc "fold-toggle" "a" #'+fold/toggle
       :desc "fold-close" "c" #'+fold/close
       :desc "fold-closeall" "m" #'+fold/close-all
       :desc "fold-open" "o" #'+fold/open
       :desc "fold-openall" "r" #'+fold/open-all)
      (:prefix-map ("p" . "project")
       :desc "open project file in other window" "w" #'projectile-find-file-other-window
       :desc "open other open projects" "["  #'centaur-tabs-counsel-switch-group
       :desc "project search" "s" #'+ivy/project-search)
      (:prefix-map ("b" . "buffer")
       :desc "open project specific scratch window" "p" #'doom/open-project-scratch-buffer
       :desc "open buffer in other window" "w" #'+ivy/switch-buffer-other-window
       :desc "rename file and buffer" "r" #'mm/rename-file-and-buffer
       :desc "new empty buffer" "n" #'centaur-tabs--create-new-empty-buffer
       :desc "new empty org buffer" "o" #'mm/new-org-tab
       :desc "open buffer" "," #'switch-to-buffer)
      (:prefix-map ("f" . "file")
       :desc "find file fuzzy" "z" #'projectile-find-file
       :desc "open  file in other window" "w" #'find-file-other-window
       :desc "open buffer" "," #'switch-to-buffer)
      (:prefix-map ("k" . "mmmisc")
       :desc "unix pipe command on region" "s" #'mm/filter-text
       :desc "deleting matching lines" "d" #'delete-matching-lines
       :desc "replace all matching regexp" "r" #'mm/replace_all_like_selected
       :desc "iedit " ";" #'iedit-mode
       :desc "iedit rectangle" "'" #'iedit-rectangle-mode))
