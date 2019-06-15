;; Frame
(tool-bar-mode -1)
(setq visible-bell t)
(toggle-frame-fullscreen)

;; Fonts
(set-face-attribute 'default nil :family "InconsolataGo" :height 145)

;; Prevent horizontal splits for big windows.
(setq split-height-threshold nil)

(provide 'init-visual)
