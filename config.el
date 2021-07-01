;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

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

(setq doom-font (font-spec :family "SF Mono" :size 16 )
       doom-big-font (font-spec :family "SF Mono" :size 36)
       doom-variable-pitch-font (font-spec :family "Avenir Next" :size 18))
(setq doom-theme 'idea-darkula)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'idea-darkula)




;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(global-so-long-mode 1)
(setq-default cursor-type '(bar . 4))
(set-cursor-color "#66fc03")
(blink-cursor-mode t)
(setq select-enable-primary t)
(setq mouse-drag-copy-region t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq python-shell-interpreter "python3")

(add-to-list 'default-frame-alist '(right-divider-width . 0))










;; org manipulation functions


(setq org-directory "~/notes/")

(setq deft-directory "~/notes"
      deft-extensions '("org" "md" "txt")
      deft-default-extension "org"
      deft-new-file-format "%Y%m%d_notes")


(defun mm/append-dt (file)
  concat (format-time-string "%Y%m%d_") file )


(after! org
  (setq org-capture-templates '(("t" "Quick TODO" entry (file+headline  +org-capture-todo-file
                                                                        "Inbox") "* TODO %?\n%i\n%a")
                                ("n" "Quick Capture Notes" entry (file+headline   "notes.org"
                                                                                  "Quick Notes")
                                 "* %U\n** %?\n%i\n%a" )
                                ("N" "Quick Capture Notes Clipboard" entry (file+headline
                                                                            "notes.org"
                                                                            "Quick Notes")
                                 "* %U\n** %?\n%x\n\n" )
                                ("j" "Journal" entry (file+headline  +org-capture-journal-file
                                                                     "Journal") "* %U\n** %? \n%i\n%a"))))


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

(defun org-insert-clipboard-image
    (&optional
     file)
  (interactive "F")
  (let )
  (shell-command (concat "pngpaste " file))
  (insert (concat "[[" file "]]"))
  (org-display-inline-images))

(setq current-date-time-format "%Y%m%d_%H%M%S")

(defun mm/current-time()
  (format-time-string current-date-time-format (current-time)))

(defun mm/org-insert-image ()
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
  (let ((bname (buffer-name)))
    (if (string-match "*scratch*" bname)
        (delete-window)
      (switch-to-buffer-other-window "*scratch*"))))


(defun mm/toggle-scratch()
  (interactive)
  (let ((bname (buffer-name)))
    (if (string-match "*doom:scratch*" bname)
        (delete-window)
      (doom/open-project-scratch-buffer))))



(defun mm/woccur ()
  "Invoke a wgrep buffer on the current ivy results, if supported."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let ((caller (ivy-state-caller ivy-last)))
    (if-let (occur-fn (plist-get +ivy-edit-functions caller))
        (ivy-exit-with-action
         (lambda (_) (funcall occur-fn)))
      (if-let (occur-fn (plist-get ivy--occurs-list caller))
          (let ((buffer (generate-new-buffer
                         (format "*ivy-occur%s \"%s\"*"
                                 (if caller (concat " " (prin1-to-string caller)) "")
                                 ivy-text))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (funcall occur-fn))
              (setf (ivy-state-text ivy-last) ivy-text)
              (setq ivy-occur-last ivy-last)
              (setq-local ivy--directory ivy--directory))
            (ivy-exit-with-action
             `(lambda (_)
                (display-buffer ,buffer)
                (ivy-wgrep-change-to-wgrep-mode)
                (mm/toggle-window-split))))
        (user-error "%S doesn't support wgrep" caller)))))

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



(defun mm/duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))




(use-package! centaur-tabs
        :demand
        :config
        (centaur-tabs-group-by-projectile-project)
        (centaur-tabs-mode t)
        :bind (("s-]" . centaur-tabs-forward)
               ("s-n" . mm/new-org-tab)
               ("s-[" . centaur-tabs-backward)
               ("s-m" . centaur-tabs-open-in-external-application)
               ("s-p" . centaur-tabs-counsel-switch-group)))


;; key bindings

(global-set-key (kbd "C-S-a") 'hs-toggle-hiding)
(global-set-key (kbd "C-S-k") 'kill-visual-line)
(global-set-key (kbd "C-S-r") 'hs-show-all)
(global-set-key (kbd "C-d") 'mm/duplicate-line)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-s") '+default/search-buffer)
(global-set-key (kbd "C-s-;") 'iedit-rectangle-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-. r") 'mm/replace_all_like_selected)
(global-set-key (kbd "s-. s") 'mm/filter-text)
(global-set-key (kbd "s-x") 'kill-region)


;; window
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-;") 'iedit-mode)
(global-set-key (kbd "s-'") 'iedit-rectangle-mode)
(global-set-key (kbd "s-b") 'ivy-switch-buffer)
(global-set-key (kbd "s-o") 'other-window)

;; buffer
(global-set-key (kbd "<M-s-left>") 'previous-buffer)
(global-set-key (kbd "<M-s-right>") 'next-buffer)

(global-set-key (kbd "s-{") 'mm/woccur)
(global-set-key (kbd "s-k") 'kill-buffer-and-window)
(global-set-key (kbd "s-,") 'mm/toggle-main-scratch)
(global-set-key (kbd "s-<") 'mm/toggle-scratch)
(global-set-key (kbd "s-Z") 'undo-fu-only-redo)


(map! :leader
      (:prefix-map ("w" . "workspaces/windows")
       :desc "toggle window split" "2" #'mm/toggle-window-split
       :desc "window swap states" "t" #'window-swap-states)
      (:prefix-map ("p" . "project")
       :desc "open project file in other window" "w" #'projectile-find-file-other-window
       :desc "project search" "s" #'+ivy/project-search)
      (:prefix-map ("b" . "buffer")
       :desc "open project specific scratch window" "p" #'doom/open-project-scratch-buffer
       :desc "rename buffer" "r" #'rename-buffer
       :desc "open buffer" "," #'switch-to-buffer)
      (:prefix-map ("f" . "file")
       :desc "find file fuzzy" "z" #'projectile-find-file
       :desc "rename buffer" "r" #'rename-buffer
       :desc "open buffer" "," #'switch-to-buffer)
      (:prefix-map ("k" . "mmmisc")
      :desc "unix pipe command on region" "s" #'mm/filter-text
      :desc "deleting matching lines" "d" #'delete-matching-lines
      :desc "replace all matching regexp" "r" #'mm/replace_all_like_selected
      :desc "iedit " ";" #'iedit-mode
      :desc "iedit rectangle" "'" #'iedit-rectangle-mode))
