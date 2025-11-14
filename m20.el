;; -*- lexical-binding: nil -*-
;; ===============================================================
;; [EGG / TAMAGO Integrated Input Tracker with ITS Report]
;; Canonical base: 2025-11-02T12:00 JST / rev. FINAL
;; ===============================================================
;; Preserves full historical logic with additive ITS-REPORT subsystem
;; ===============================================================

;; === EGG / TAMAGO Debugging Rules ===
;;
;; 1. Never remove functions that log tri-state, previous states, or
;;    pre/post hooks.  - Functions like `egg-trace-position-recorder`,
;;    `egg-trace-position-pre-checker`, etc., must be preserved.
;;
;; 2. Preserve the original hook and mouse binding environment.
;;    - Do not overwrite global maps or hooks; wrap them instead.
;;    - Always restore previous bindings on unload.
;;
;; 3. Keep all comments and historical debug logic intact.
;;    - Comments explaining intricate processing, edge cases, or debugging rationale
;;      must not be deleted.
;;
;; 4. Do not convert dynamically scoped code to lexical-binding or
;;    lambdas if the code relies on dynamic binding.  - Named
;;    functions are preferred for advice or hook
;;
;; 5. Go for verbose debug messages with lots of information until
;;    I need to ask you to limit the log to special cases.
;;
;; RULE 6 : During debugging or investigative Emacs Lisp work,
;; Preserve all comments verbatim — including block, inline,
;; and historical context comments — unless the user explicitly requests
;; their removal or summarization.
;;
;; Comments should be treated as part of the executable knowledge base.
;; If a reorganization requires moving code, the related comment block
;; must move with it, unchanged.

;; NEVER shorten or "simplify" explanatory comments about
;; debugging strategy, historical findings, or unexpected Emacs behavior.
;;;
;;; Rule 7 : No code merging or simplifying HEURISTICS. Uless you can
;;; logically prove that the change is identical and produce the same
;;; reuslt, don't merge code.  Rule 8: Don't rename existing functions
;;; randomly.
;;;
;;; When you try to modify the code:
;;;
;;; Rule 8: Don't rename existing functions randomly.
;;;
;;; rule 9. Don't use "cleanup" heuristics
;;;
;;;rule 10. Check why the code takes the form as it does.
;;;   It possibly has serious reason to be complicated
;;;   to avoid rare error conditions.
;;;
;;; rule 11. Check the semantics.
;;;   We are dealing with nasty real-world error conditions and such.
;;;   Code cannot be "clean" when we try to handle them.
;;;
;;; rule 12. Don't ASSUME anything.  Always VERIFY using the available
;;;   code.  If you need to assume somthing to create a much shorter
;;;   code, or much cleaner code, etc. to benefit the maintenance,
;;;   then ASSERT the conditions in the first part of the code block
;;;   so that we can fail if the assummption is incorrect.
;;;
;;; rule 13. Preserve every live function and code path unless
;;; explicitly requested to remove it
;;;
;;;
;;; Rule 14. Always trace conditional calls during debugging.
;;;
;;; Rule 15. Never simplify code with historical debugging logic
;;;
;;; End of rules block.
;;;

;;; TODO/FIXME:
;;; 1. mouse-1 button is handled. But other buttons are not handled.
;;; 2. Checking for mouse or wheel related events/commands are
;;;    done by very loose string match. We may want to make exact comparison, but
;;;    in order to do that, we have to enumarate all the mouse or wheel events.
;;;    This is something I have not done. Currect code "works" for my situation.

;;; --- Global variables ---
(defvar its-debug-enabled nil
  "Set this to non-nil to enable logging for debugging")
(defvar its-debug-log-buffer "*ITS-DEBUG*")
;;; The *ITS-DEBUG* buffer is trimmed when we go over the following #
;;; of lines.
(defvar its-debug-log-max-lines 3000)
(defvar its-debug-running-counter 0)
(defvar its-debug-start-timestamp nil
  "Timestamp marking the start of current ITS debug session.")

;;; IMPROVEMENT: Make magic numbers configurable
(defvar egg-dump-buffer-max-chars 100
  "Maximum characters to dump in buffer snapshots.")

;;; --- Logging core ---
(defun its-current-timestamp ()
  "Return timestamp string (for markers and headers)."
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

(defun its-debug-log (fmt &rest args)
  "Log formatted message to *ITS-DEBUG* buffer, timestamped and numbered.
Logging is done only when its-debug-enabled is non-nil.
Suppress logging when current buffer is *ITS-DEBUG* itself or minibuffer is active."
  ;; FIXED: Previously had inverted logic. Now correctly checks:
  ;; - its-debug-enabled must be non-nil
  ;; - current buffer must NOT be minibuffer
  ;; - current buffer must NOT be the debug buffer itself
  ;; - The last condition ensures we can look at the debug buffer
  ;;   and scroll through it without getting new lines added while we do this.
  
  (when (and  its-debug-enabled
             (not  (minibufferp))
             (not (string= (buffer-name (current-buffer)) its-debug-log-buffer)))
    (setq its-debug-running-counter (1+ its-debug-running-counter))
    (let* ((timestamp (its-current-timestamp))
           (msg (apply #'format fmt args))
           (log-line (format "[%s] [#%04d] %s\n"
                             timestamp
                             its-debug-running-counter
                             msg)))
      (with-current-buffer (get-buffer-create its-debug-log-buffer)
        (goto-char (point-max))
        (insert log-line)
        (when (> (count-lines (point-min) (point-max))
                 its-debug-log-max-lines)
          (goto-char (point-min))
          (forward-line (/ its-debug-log-max-lines 3))
          (delete-region (point-min) (point)))))))

;;; --- Utility functions ---
(defun egg--truncate-content (str &optional maxlen)
  "Return STR truncated to MAXLEN characters with […] if needed."
  (let ((limit (or maxlen 100)))
    (if (> (length str) limit)
        (concat (substring str 0 limit) "[…]")
      str)))

(defun egg--dump-buffer-snapshot (&optional label)
  "Dump buffer content (truncated) with text properties."
  (let ((label (or label "snapshot"))
        (end-pos (min (+ (point-min) egg-dump-buffer-max-chars) (point-max)))
        (i (point-min)))
    (while (< i end-pos)
      (let ((ch (char-after i))
            (props (text-properties-at i)))
        (its-debug-log "[EGG-DUMP] %s POS=%d Char='%c' Props=%s"
                       label i (if ch ch ?\0) props))
      (setq i (1+ i)))))

;;; --- Internal state tracking (explicit variables, no plists) ---
;;;
;;; Why three? To make a long story short, there are very complex
;;; event sequences when mouse is involved, especially when egg/tamago
;;; input is used minibuffer .  I could only figure out what was
;;; happening only after recording two generations of old data and
;;; compare it to the next state.
;;;
;;; Case in point: One of my mouse seems to broken and generates mouse
;;; DRAG event when I simply click the left button to move a cursor to
;;; new position, for no apparent reason.  example.  OS mouse driver
;;; and even the Emacs's mouse handler seems to cope with such barrage
;;; of strange mouse events that arrive in short successon, but some
;;; strange mouse events do seep to user applicaton.
;;;
;;; BEWARE: So the logically correct handling here may not work with
;;; unexpected arrival order of events created by a faulty mouse
;;; and/or driver.

(defvar egg--prev-pos-pos nil)   ;; second-last POS
(defvar egg--prev-pos-state nil) ;; second-last state symbol
(defvar egg--prev-pos-cmd nil)   ;; second-last cmd
(defvar egg--prev-pos-key nil)   ;; second-last key desc

(defvar egg--last-pos-pos nil)   ;; last POS
(defvar egg--last-pos-state nil) ;; last state symbol
(defvar egg--last-pos-cmd nil)   ;; last cmd
(defvar egg--last-pos-key nil)   ;; last key desc

(defvar egg--now-pos-pos nil)    ;; now POS (transient)
(defvar egg--now-pos-state nil)  ;; now state (transient)
(defvar egg--now-pos-cmd nil)    ;; now cmd (transient)
(defvar egg--now-pos-key nil)    ;; now key (transient)

(defvar egg--mouse-original-binding nil)

;;; --- Input-state helper ---
(defun egg-current-input-state ()
  "Return symbol describing current Tamago input state.

Possible values:
  'ascii       -- ordinary input
  'fence       -- Japanese input active, before conversion
  'conversion  -- inside Egg conversion mode
  'unknown     -- fallback if state cannot be determined"
  (cond
   ((not current-input-method) 'ascii)
   ((and (boundp 'its-fence-mode) ;;; This must be checked first.
         its-fence-mode) 'fence)
   (current-input-method 'conversion)
   ((and (boundp 'egg-in-conversion)
         egg-in-conversion) 'conversion)
   (t 'unknown)))

;; [20251108-004]  Add type-safe mouse detection for byte-compiled commands
;; Classification: Validated Patch (Rule 26 workflow)
;; Timestamp: 2025-11-08T23:59 JST
;; Validation mode: Multi-Form (2 forms)

;;----------------------------------------------------------------
;;  EGG-TRACE utilities
;;----------------------------------------------------------------

;; --- [20251108-004-A]  New helper  ----------------------------------------
;; Now we handle wheel-related events to some extent.
(defun egg--mouse-related-command-p (cmd)
  "Return non-nil if CMD (symbol or byte-code function) looks mouse-related or wheel-related.
Preserves dynamic scope and never signals type errors.
Used in pre/post trace diagnostics where `this-command' may be a
byte-compiled lambda in Emacs 30+."
  (cond
   ;; Case 0 — nil/null command (IMPROVEMENT: Added defensive check)
   ((null cmd) nil)
   ;; Case 1 — ordinary symbol command
   ((symbolp cmd)
    (or (string-match-p "mouse" (symbol-name cmd))
        (string-match-p "wheel-down" (symbol-name cmd))
        (string-match-p "wheel-scroll" (symbol-name cmd))
        ))
   ;; Case 2 — byte-code closure (Emacs 30)
   ((byte-code-function-p cmd)
    (let ((desc (prin1-to-string cmd)))
      (string-match-p
       "\\(mouse-\\|posn-\\|event-end\\|mouse-movement\\|wheel-scroll\\|wheel-down\\)"
       desc)))
   ;; Case 3 — anything else
   (t nil)
   )
  )

;;; --- Pre-command hook: detect unexpected movement and optionally exit conversion ---
(defun egg-trace-position-pre-checker ()
  "Pre-command hook: detect unexpected movement and exit conversion if needed."
  (condition-case top-err
      (let ((now-pos (point))
            (now-state (egg-current-input-state))
            (cmd this-command)
            (key (key-description (this-command-keys-vector))))
        ;; --- Trace logging ---
        (its-debug-log
         "[EGG-PRE] Trace snapshot:\n  prev-pos=%s prev-state=%s prev-cmd=%s prev-key=%s\n  last-pos=%s last-state=%s last-cmd=%s last-key=%s\n  now-pos=%s now-state=%s now-cmd=%s now-key=%s"
         egg--prev-pos-pos egg--prev-pos-state egg--prev-pos-cmd egg--prev-pos-key
         egg--last-pos-pos egg--last-pos-state egg--last-pos-cmd egg--last-pos-key
         now-pos now-state cmd key)
        ;; Update transient now variables (visible to outer logic if needed)
        (setq egg--now-pos-pos now-pos
              egg--now-pos-state now-state
              egg--now-pos-cmd cmd
              egg--now-pos-key key)
        ;; === Detect abrupt mouse movement in conversion/fence ===
        (when (and egg--prev-pos-pos egg--last-pos-pos)
          (let* ((cond1 (and (member egg--prev-pos-state '(conversion fence))
                             (member egg--last-pos-state '(conversion fence))
                             (member now-state '(conversion fence))))
                 (cond2 (and (/= egg--prev-pos-pos egg--last-pos-pos)
                             (= egg--last-pos-pos now-pos)))
                 (cond3 (and egg--last-pos-cmd
                             (egg--mouse-related-command-p egg--last-pos-cmd)))
                 (cond4 (and cmd
                             (egg--mouse-related-command-p cmd))))
            ;; Conditions 1 & 2 true but 3 or 4 false -> warn
            (when (and cond1 cond2 (not (and cond3 cond4)))
              (its-debug-log
               "[EGG-WARN] Conditions 1 & 2 satisfied, but mouse keys not detected (last-key=%s, now-key=%s); not invoking egg-exit-conversion"
               egg--last-pos-key key))
            ;; All 4 conditions satisfied -> trigger exit
            (when (and cond1 cond2 cond3 cond4)
              (its-debug-log "[EGG-PRE] Abrupt mouse movement detected in conversion/fence. Invoking egg-exit-conversion")
              (condition-case inner-err
                  (save-excursion
                    (let ((saved-marker (point-marker)))
                      (goto-char egg--prev-pos-pos)
                      (when (and (eq now-state 'conversion) (fboundp 'egg-exit-conversion))
                        (egg-exit-conversion))
                      (when (and (eq now-state 'fence) (fboundp 'its-exit-mode))
                        (its-exit-mode))
                      ;; === Post-exit variable reset (per your request) ===
                      (setq egg--last-pos-state nil
                            egg--last-pos-cmd 'egg-exit-conversion
                            egg--last-pos-key '\n
                            egg--now-pos-pos now-pos
                            egg--now-pos-state now-state)
                      (set-marker saved-marker nil)))
                (error
                 (its-debug-log "[EGG-PRE] ERROR during egg-exit-conversion: %s" inner-err))))))
        ;; === New mechanism: handle down-mouse-1 when last state is conversion/fence ===
        ;; This is similar to 3 generation detection above, but the position
        ;; to go to before invoking tamago exit function is different.
        ;;           (string-match-p "<down-mouse-1>" (symbol-name cmd))
        (when (and (egg--mouse-related-command-p cmd)
                   (member egg--last-pos-state '(conversion fence))
                   (/= egg--prev-pos-pos egg--last-pos-pos))
          ;; Trigger exit at the last known position before mouse click/drag
          (its-debug-log "[EGG-PRE] Mouse down detected; invoking exit at last state/position")
          (condition-case inner-err
              (save-excursion
                (let ((saved-marker (point-marker)))
                  (goto-char egg--last-pos-pos)
                  (when (and (eq egg--last-pos-state 'conversion) (fboundp 'egg-exit-conversion))
                    (egg-exit-conversion))
                  (when (and (eq egg--last-pos-state 'fence) (fboundp 'its-exit-mode))
                    (its-exit-mode))
                  ;; Reset last-pos after forced exit
                  (setq egg--last-pos-state nil
                        egg--last-pos-cmd 'egg-exit-conversion
                        egg--last-pos-key '\n)
                  (set-marker saved-marker nil)))
            (error
             (its-debug-log "[EGG-PRE] ERROR during forced exit for down-mouse-1: %s" inner-err)))))
    (error
     (its-debug-log "[EGG-PRE] ERROR: %s" top-err))))


;;; --- Post-command hook: record and log cursor state, and check abrupt movement ---
;;; We repeat the invocation of exit from tamago here, too.
;;; A buggy mouse made it easier to handle the situation here.
(defun egg-trace-position-recorder ()
  "Post-command hook: record and log cursor state."
  (condition-case top-err
      (let ((pos (point))
            (state (egg-current-input-state))
            (cmd this-command)
            (key (key-description (this-command-keys-vector))))
        ;; --- Logging in the same verbose trace format as PRE
        (its-debug-log
         "[EGG-POST] Trace snapshot:\n  prev-pos=%s prev-state=%s prev-cmd=%s prev-key=%s\n  last-pos=%s last-state=%s last-cmd=%s last-key=%s\n  now-pos=%s now-state=%s now-cmd=%s now-key=%s"
         egg--prev-pos-pos egg--prev-pos-state egg--prev-pos-cmd egg--prev-pos-key
         egg--last-pos-pos egg--last-pos-state egg--last-pos-cmd egg--last-pos-key
         pos state cmd key)
        ;; === Detect abrupt mouse movement in conversion/fence ===
        (when (and egg--prev-pos-pos egg--last-pos-pos)
          (let* ((cond1 (and (member egg--prev-pos-state '(conversion fence))
                             (member egg--last-pos-state '(conversion fence))
                             (member state '(conversion fence))))
                 (cond2 (and (/= egg--prev-pos-pos egg--last-pos-pos)
                             (= egg--last-pos-pos pos)))
                 (cond3 (and egg--last-pos-cmd
                             (egg--mouse-related-command-p egg--last-pos-cmd)))
                 (cond4 (and cmd
                             (egg--mouse-related-command-p cmd))))
            ;; Conditions 1 & 2 true but 3 or 4 false -> warn
            (when (and cond1 cond2 (not (and cond3 cond4)))
              (its-debug-log
               "[EGG-WARN] Conditions 1 & 2 satisfied, but mouse keys not detected (last-key=%s, now-key=%s); not invoking egg-exit-conversion"
               egg--last-pos-key key))
            ;; All 4 conditions satisfied -> trigger exit
            (when (and cond1 cond2 cond3 cond4)
              (its-debug-log "[EGG-POST] Abrupt mouse movement detected in conversion/fence. Invoking egg-exit-conversion")
              (condition-case inner-err
                  (save-excursion
                    (let ((saved-marker (point-marker)))
                      (goto-char egg--prev-pos-pos)
                      (when (and (eq state 'conversion) (fboundp 'egg-exit-conversion))
                        (egg-exit-conversion))
                      (when (and (eq state 'fence) (fboundp 'its-exit-mode))
                        (its-exit-mode))
                      ;; === Post-exit variable reset ===
                      (setq egg--last-pos-state nil
                            egg--last-pos-cmd 'egg-exit-conversion
                            egg--last-pos-key '\n
                            egg--now-pos-pos pos
                            egg--now-pos-state state)
                      (set-marker saved-marker nil)))
                (error
                 (its-debug-log "[EGG-POST] ERROR during egg-exit-conversion: %s" inner-err))))))
        ;; === New mechanism: handle down-mouse-1 when last state is conversion/fence ===
        ;; This is similar to 3 generation detection above, but the position
        ;; to go to before invoking tamago exit function is different.
        ;; (string-match-p "<down-mouse-1>" (symbol-name cmd))
        (when (and (egg--mouse-related-command-p cmd)
                   (member egg--last-pos-state '(conversion fence))
                   (/= egg--prev-pos-pos egg--last-pos-pos))
          ;; Trigger exit at the last known position before mouse click/drag
          (its-debug-log "[EGG-POST] Mouse down detected; invoking exit at last state/position")
          (condition-case inner-err
              (save-excursion
                (let ((saved-marker (point-marker)))
                  (goto-char egg--last-pos-pos)
                  (when (and (eq egg--last-pos-state 'conversion) (fboundp 'egg-exit-conversion))
                    (egg-exit-conversion))
                  (when (and (eq egg--last-pos-state 'fence) (fboundp 'its-exit-mode))
                    (its-exit-mode))
                  ;; Reset last-pos after forced exit
                  (setq egg--last-pos-state nil
                        egg--last-pos-cmd 'egg-exit-conversion
                        egg--last-pos-key '\n)
                  (set-marker saved-marker nil)))
            (error
             (its-debug-log "[EGG-POST] ERROR during forced exit for down-mouse-1: %s" inner-err))))
        ;; --- Shift last->prev, now->last updates (explicit variables) ---
        (setq egg--prev-pos-pos egg--last-pos-pos
              egg--prev-pos-state egg--last-pos-state
              egg--prev-pos-cmd egg--last-pos-cmd
              egg--prev-pos-key egg--last-pos-key)
        (setq egg--last-pos-pos pos
              egg--last-pos-state state
              egg--last-pos-cmd cmd
              egg--last-pos-key key))
    (error
     (its-debug-log "[EGG-POST] ERROR: %s" top-err)
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (backtrace)
         (its-debug-log "%s" (buffer-string)))))))

;;; --- Mouse wrapper ---
;;;
;;; Dispatch of the original mouse handler in minibuffer, etc. are
;;; deferred, but I think exiting from Japanese input using Egg/Tamago
;;; in minibuffer is handled in pre/post handler.  To be honest, I
;;; almost forgot why minibuffer, etc. are ignored. I recall vaguely
;;; that if the dispatch is done here, minibuffer can be cluttered
;;; with overlaid input prompt, etc. This special handling is done
;;; only when egg/tamago is done, so should not be a problem (well,
;;; that is what I think). Someone got to prove it. :-)
;;;
(defun egg-mouse-1-wrapper (event)
  "Wrapper for mouse-1; call original binding safely without recursion.
- If original binding is a symbol & command, call it interactively.
- If original binding is a function, apply with EVENT.
- If no original binding, fall back to `mouse-set-point` if available."
  (interactive "e")
  (condition-case err
      (let* ((ev-start (event-start event))
             (win (posn-window ev-start))
             (posn (posn-point ev-start))
             (buf (and (windowp win) (window-buffer win)))
             (orig egg--mouse-original-binding))
        (its-debug-log "[EGG-MOUSE] egg-mouse-1-wrapper invoked; args=%s" (list event))
        ;; If buffer is special (minibuffer, names starting with * or minibuffer or read-only), don't dispatch
        (if (or (not buf)
                (minibufferp buf)
                (string-prefix-p "*" (buffer-name buf))
                (with-current-buffer buf buffer-read-only))
            (its-debug-log "[EGG-MOUSE] Click is deffered in special/read-only buffer: %s"
                           (if buf (buffer-name buf) "<no-buffer>"))
          ;; Dispatch original command safely without recursive blowup (!)
          (cond
           ((and (symbolp orig) (commandp orig) (not (eq orig 'egg-mouse-1-wrapper)))
            (its-debug-log "[EGG-MOUSE] dispatching original symbol command: %s" orig)
            (call-interactively orig))
           ((and (functionp orig) (not (eq orig 'egg-mouse-1-wrapper)))
            (its-debug-log "[EGG-MOUSE] applying original function binding")
            (apply orig (list event)))
           (t
            (its-debug-log "[EGG-MOUSE] fallback to mouse-set-point if available")
            (if (fboundp 'mouse-set-point)
                (apply #'mouse-set-point (list event))
              (its-debug-log "[EGG-MOUSE] fallback mouse-set-point not available"))))))
    (error
     (its-debug-log "[EGG-MOUSE] ERROR: %S" err)
     (with-temp-buffer
       (let ((standard-output (current-buffer)))
         (backtrace)
         (its-debug-log "%s" (buffer-string)))))))


;;; --- Hooks installation ---
(defun egg-load-monitor-hooks ()
  "Install monitoring hooks and mouse wrapper safely."
  (interactive)
  (its-report-start);;; this always leaves a timestamped marker in *Messages*, too.
  (its-debug-log "[EGG] Loading monitor hooks")
  (egg--dump-buffer-snapshot "AFTER-LOAD")
  ;; Install hooks
  (add-hook 'pre-command-hook 'egg-trace-position-pre-checker)
  (add-hook 'post-command-hook 'egg-trace-position-recorder)
  ;; Capture original mouse-1 binding **before** installing wrapper
  (unless (and (boundp 'egg--mouse-original-binding)
               egg--mouse-original-binding)
    (setq egg--mouse-original-binding (key-binding [mouse-1])))
  ;; Install wrapper
  (global-set-key [mouse-1] 'egg-mouse-1-wrapper)
  (its-debug-log "[EGG] Hooks added (pre, post, mouse)")
  (its-debug-log "[EGG] Conversion mode state and symbol dump:")
  (its-debug-log "  pre-hook: %s" 'egg-trace-position-pre-checker)
  (its-debug-log "  post-hook: %s" 'egg-trace-position-recorder)
  (its-debug-log "  mouse wrapper: %s" 'egg-mouse-1-wrapper))



(defun egg-unload-monitor-hooks ()
  "Remove hooks, restore mouse binding, and finalize ITS report safely."
  (interactive)
  ;; Remove hooks
  (remove-hook 'pre-command-hook 'egg-trace-position-pre-checker)
  (remove-hook 'post-command-hook 'egg-trace-position-recorder)

  ;; Restore original mouse binding
  (when egg--mouse-original-binding
    (global-set-key [mouse-1] egg--mouse-original-binding))

  ;; Log final buffer snapshot
  (its-debug-log "[EGG] Hooks removed. Final buffer snapshot:")
  (egg--dump-buffer-snapshot "BEFORE-UNLOAD")

  ;; Ensure *ITS-REPORT* buffer is fresh and writable
  (let ((buf (get-buffer-create "*ITS-REPORT*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Insert report header
        (insert (format-time-string "[%Y-%m-%dT%H:%M:%S]  ITS REPORT BEGIN\n")))))

  ;; Generate ITS debug report safely
  (egg-generate-full-its-report)

  ;; Reset mouse-original-binding
  (setq egg--mouse-original-binding nil))

;;; --- ITS Report subsystem ---
(defun its-report-start ()
  "Insert marker line in *Messages* and *ITS-DEBUG* buffers."
  (setq its-debug-start-timestamp (its-current-timestamp))
  (let ((marker (format "ITS-REPORT START MARKER: %s" its-debug-start-timestamp)))
    (its-debug-log "%s" marker)
    (message "%s" marker)))

(defun its--collect-core-info ()
  "Return a formatted string containing the core ITS debug log slice.

The slice begins at the most recent \"ITS-REPORT START MARKER:\" line
in the *ITS-DEBUG* buffer and continues to the buffer end.
If the buffer or marker is not found, return a descriptive message.

This function does not modify any buffer; it only returns text."
  (let ((out ""))
    (condition-case err
        (if (not (get-buffer its-debug-log-buffer))
            (setq out "[ITS-DEBUG] No debug buffer found.\n")
          (with-current-buffer (get-buffer its-debug-log-buffer)
            (goto-char (point-max))
            (let* ((marker (and its-debug-start-timestamp
                                (format "ITS-REPORT START MARKER: %s"
                                        its-debug-start-timestamp)))
                   (start (if (and marker (search-backward marker nil t))
                              (progn (forward-line 1) (point))
                            (point-min)))
                   (slice (buffer-substring-no-properties start (point-max))))
              (setq out
                    (concat "[ITS-DEBUG CORE]\n"
                            slice
                            "\n[END ITS-DEBUG CORE]\n")))))
      (error
       (setq out (format "[ITS-DEBUG] ERROR collecting core info: %s\n" err))))
    out))


;; Validated: PASS (Rule 25 workflow)
;; Timestamp: 2025-11-09T03:15-J1
(defun egg-generate-full-its-report ()
  "Generate full ITS debug report in *ITS-REPORT* buffer atomically.
Preserves existing header, appends *Messages* excerpt starting from
the last ITS-REPORT START MARKER, adds ITS-DEBUG slice and backtrace
if available, and makes the buffer visible."
  (interactive)
  (let ((buf (get-buffer-create "*ITS-REPORT*"))
        (marker "ITS-REPORT START MARKER:"))
    ;; Ensure buffer is writable and preserve content (Atomic Adherence)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            messages-collected)
        ;; Move point to end for additive insertion
        (goto-char (point-max))
        ;; Extract *Messages* content from last marker backward
        (when (get-buffer "*Messages*")
          (with-current-buffer "*Messages*"
            (goto-char (point-max))
            (if (search-backward marker nil t)
                ;; Include the marker line itself
                (setq messages-collected
                      (split-string
                       (buffer-substring (line-beginning-position)
                                         (point-max))
                       "\n" t))
              (setq messages-collected
                    '("[ITS-REPORT] No recent start marker found.")))))
        ;; Insert Messages excerpt after report header
        (when messages-collected
          (goto-char (point-max))
          (insert "[*Messages*]\n")
          (dolist (line messages-collected)
            (insert line "\n"))
          (insert "\n[END *Messages*]\n\n"))
        ;; Insert ITS-DEBUG slice if available
        (when (get-buffer "*ITS-DEBUG*")
          (insert "[ITS-DEBUG CORE]\n")
          (insert-buffer-substring "*ITS-DEBUG*")
          (insert "\n[END ITS-DEBUG CORE]\n\n"))
        ;; Insert Backtrace if available
        (when (get-buffer "*Backtrace*")
          (insert "[BACKTRACE]\n")
          (insert-buffer-substring "*Backtrace*")
          (insert "\n[END OF REPORT]\n")))
      ;; Make buffer visible at the end
      (display-buffer buf))))

;;; ---
(provide 'egg-its-mouse-tracker)
;;; End of canonical file [2025-11-02T12:00 JST / rev. FINAL]
