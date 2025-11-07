; Hack to get JIT working, or else I get "libgccjit.so: error: error invoking gcc driver"
; Should not be needed, c.f:
; - https://github.com/d12frosted/homebrew-emacs-plus/issues/187
; - https://github.com/d12frosted/homebrew-emacs-plus/issues/323
;(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11")

; Increase GC threshold during startup.
(setq gc-cons-threshold (* 100 1000 1000))
