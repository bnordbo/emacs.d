;; Frame
(tool-bar-mode -1)
(setq visible-bell t)
(toggle-frame-fullscreen)
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;; Fonts
(set-face-attribute 'default nil :family "InconsolataGo" :height 145)

;; Prevent horizontal splits for big windows.
(setq split-height-threshold nil)

;; Zoom mode.
(setq zoom-size '(100 .  40))

;; No ugly line wrapping.
(set-default 'truncate-lines t)

(provide 'init-visual)
