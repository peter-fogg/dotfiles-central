;; Totally my .emacs!

;; Load stuff up.
(add-to-list 'load-path "~/.emacs.d/elisp/")
(package-initialize)

;; Don't check package signatures -- seems to be necessary for gnu ELPA
(setq package-check-signature nil)

;; Get all the packages.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-refresh-contents)

;; Install all the things.
(defconst packages-to-install
  '(zenburn-theme
    paredit
    linum
    undo-tree
    haskell-mode
    rainbow-delimiters
    markdown-mode))

(dolist (package packages-to-install)
  (when (not (package-installed-p package))
      (package-install package)))

;; Load up custom functions.
(dolist (file '("functions.el"
                "hooks.el"
                "keys.el"
                "initialization.el"))
  (load-file (concat "~/.emacs.d/elisp/" file)))

;; The most important part of this file.
(load-theme 'zenburn t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (haskell-mode zenburn-theme undo-tree rainbow-delimiters paredit coffee-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(server-start)
