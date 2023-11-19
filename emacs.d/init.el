(eval-when-compile
  (require 'vl-setup))

(setup (:package vlaci))

(setup (:package gcmh)
  (:hook-into after-init-hook)
  (:set gcmh-verbose init-file-debug
        gcmh-high-cons-threshold (* 128 1024 1024)))

(setup autorevert
  (:set auto-revert-avoid-polling t)
  (:with-mode global-auto-revert-mode
    (:hook-into after-init-hook)))


(setup delsel
  (:with-mode delete-selection-mode
    (:hook-into after-init-hook)))


(setup (:package modus-themes)
  ;; See [[info:modus-themes#Override colors]]
  ;; and [[info:modus-themes#Customization Options]]
  (:set modus-operandi-palette-overrides
        '((bg-main "#e1d9c2")
          (fg-main "#333333")
          (bg-heading-1 bg-dim)
          (overline-heading-2 border))

        modus-themes-bold-constructs nil
        modus-themes-italic-constructs t
        modus-themes-paren-match '(bold intence)
        modus-themes-org-blocks 'gray-background
        modus-themes-disable-other-themes t

        modus-themes-links '(neutral-underline)

        modus-themes-headings
        '((1 . (medium variable-pitch 1.5))
          (2 . (rainbow variable-pitch 1.1))
          (4 . (variable-pitch))))

    (load-theme 'modus-operandi :no-confirm))

(setup (:package mini-echo)
  (:hook-into after-init-hook)
  (custom-theme-set-faces
   'user
   '(mini-echo-minibuffer-window ((t nil)))))
