;; its-exit-wrappers.el --- Add wrappers to exit ITS mode on cursor motion -*- lexical-binding: nil -*-

;;; Commentary:
;; Additive patch: provide interactive wrapper commands that exit ITS/fence mode
;; and then perform cursor-motion commands.  This file is intended to be loaded
;; after the main ITS/Egg files; it does not redefine existing keymaps but
;; binds keys into `its-mode-map' if present.
;;
;; This file also extends the ITS / Egg input system so that large cursor motions
;; (page up/down, buffer beginning/end) first exit the conversion state safely.
;;
;; Additive patch only: no existing keymaps or functions are removed or overwritten.
;;
;; Written by zephyrus_jp
;;

;;; Code:

(defun its-exit-and-next-line ()
  "Exit ITS/fence mode and then move to the next line.
If `its-exit-mode' is available it is called; otherwise fall back to
`egg-exit-conversion' when in conversion state, if available."
  (interactive)
  (condition-case _err
      (progn
        (cond
         ((and (fboundp 'its-exit-mode)) (its-exit-mode))
         ((and (fboundp 'egg-exit-conversion)) (egg-exit-conversion))))
    (error nil))
  (call-interactively 'next-line))

(defun its-exit-and-previous-line ()
  "Exit ITS/fence mode and then move to the previous line.
See `its-exit-and-next-line' for behavior."
  (interactive)
  (condition-case _err
      (progn
        (cond
         ((and (fboundp 'its-exit-mode)) (its-exit-mode))
         ((and (fboundp 'egg-exit-conversion)) (egg-exit-conversion))))
    (error nil))
  (call-interactively 'previous-line))

;;; Code:

(defun  its-exit-and-beginning-of-buffer ()
  "Exit ITS/fence mode, then move to the beginning of buffer.
If `its-exit-mode' exists, call it; otherwise fall back to `egg-exit-conversion'."
  (interactive)
  (condition-case _err
      (progn
        (cond
         ((fboundp 'its-exit-mode) (its-exit-mode))
         ((fboundp 'egg-exit-conversion) (egg-exit-conversion))))
    (error nil))
  (call-interactively 'beginning-of-buffer))

(defun its-exit-and-end-of-buffer ()
  "Exit ITS/fence mode, then move to the end of buffer.
If `its-exit-mode' exists, call it; otherwise fall back to `egg-exit-conversion'."
  (interactive)
  (condition-case _err
      (progn
        (cond
         ((fboundp 'its-exit-mode) (its-exit-mode))
         ((fboundp 'egg-exit-conversion) (egg-exit-conversion))))
    (error nil))
  (call-interactively 'end-of-buffer))

(defun its-exit-and-scroll-up ()
  "Exit ITS/fence mode, then scroll up one page.
This corresponds to the usual `C-v' key binding."
  (interactive)
  (condition-case _err
      (progn
        (cond
         ((fboundp 'its-exit-mode) (its-exit-mode))
         ((fboundp 'egg-exit-conversion) (egg-exit-conversion))))
    (error nil))
  (call-interactively 'scroll-up-command))

(defun its-exit-and-scroll-down ()
  "Exit ITS/fence mode, then scroll down one page.
This corresponds to the usual `M-v' key binding."
  (interactive)
  (condition-case _err
      (progn
        (cond
         ((fboundp 'its-exit-mode) (its-exit-mode))
         ((fboundp 'egg-exit-conversion) (egg-exit-conversion))))
    (error nil))
  (call-interactively 'scroll-down-command))

;; === Key bindings ===========================================================

;; Bind keys into its-mode-map if it exists; do not clobber the whole map.
;; TODO: create load and unload functions.
;; load should save the existing entries so that unload reverts the entries back
;; to the original state.

;; NOTE/CAUTION:
;; In the original tamago / egg, M-< and M-> are already used. 
;;    (define-key map "\M-<" 'its-half-width)
;;    (define-key map "\M->" 'its-full-width)


(when (boundp 'its-mode-map)
  ;; Move to buffer beginning / end
  (define-key its-mode-map (kbd "M-<") 'its-exit-and-beginning-of-buffer)
  (define-key its-mode-map (kbd "M->") 'its-exit-and-end-of-buffer)
  ;; Page scrolls
  (define-key its-mode-map (kbd "C-v") 'its-exit-and-scroll-up)
  (define-key its-mode-map (kbd "M-v") 'its-exit-and-scroll-down)
  (define-key its-mode-map "\C-n" 'its-exit-and-next-line)
  (define-key its-mode-map "\C-p" 'its-exit-and-previous-line))

(provide 'its-exit-wrappers)

;;; its-exit-wrappers.el ends here
;;;
;;; Save the file (e.g. its-exit-wrappers.el).
;;; 
;;; Load it after your ITS/Egg code: (load "/path/to/its-exit-wrappers.el") or (require 'its-exit-wrappers).
;;;
;;;If its-mode-map exists when loaded, C-n, C-p and others will now run the wrappers.
;;;
