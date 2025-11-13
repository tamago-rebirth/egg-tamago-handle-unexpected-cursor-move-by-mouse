;; -*- lexical-binding: nil -*-
;; ==============================================================
;; Egg/Tamago Input Mode Tracker
;; ==============================================================
;; This code tracks the cursor position and input mode in Egg.
;; It handles unexpected movements during Japanese conversion
;; and cleans up the conversion state properly.
;; ==============================================================

;;;=========================================================;;;
;;; Next line should not be necessary for final use,
;;; but during debugging I have additional source trees
;;; that contain half-baked tamago sources and thus
;;; want to make sure I only load
;;; debugged published files
;;;
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/egg")
;;;(if (not (fboundp 'make-coding-system))
;;;    (defun make-coding-system (coding-system &rest rest)
;;;      (define-coding-system coding-system ""
;;;        :mnemonic ?w :coding-type 'charset)))
(require 'egg)
;;; egg-integration-log
;;;

;;; Avoid cyclic loading usually loaded by (require 'egg) , correct?
;;; (load-file  "/usr/local/share/emacs/site-lisp/egg/leim-list.el")

;;; (load "/usr/local/share/emacs/site-lisp/egg/egg-tart")
(setq default-input-method "japanese-egg-wnn")
(setq wnn-jserver "127.0.0.1")
;;; (setq egg-default-startup-file "~/.eggrc-tamago") 
;;; Modify this to fit your needs. 
(setq egg-default-startup-file "/usr/local/share/emacs/site-lisp/egg/eggrc")

;;; Again, the following is something we should not need.
;;  I have no idea where I used to define this in the maze of
;;; my complex .emacs and files that are called from it.
;;; We should not need this, but somehow old work-in-progress
;;; code failed to set it!?
(setq egg-backend-type 'wnn)
;;;

;;; set this nil for terse operation
;;; I am debugging and thus setting it t.
(setq its-debug-enabled t)
(load-file  "~/wherever/m20.el")
(egg-load-monitor-hooks)
(message "cursor position monitoring code for egg input loaded: unload by M-x egg-unload-monitor-hooks")

;;;
;;; Protect undesirable movement by keys.
;;; Force C-p, C-n, etc. to exit ITS fence mode or egg conversion mode
;;; before moving the current position.
;;;
(load-file "~/wherever/its-exit-wrappers.el")


;; ==============================================================
;; Usage:
;; Place this code in your .emacs after (require 'egg)
;; It will automatically track and clean up Egg conversion regions
;; when the cursor moves unexpectedly and some.
;; ==============================================================
