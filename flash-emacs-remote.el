;;; flash-emacs-remote.el --- Remote operator support like flash.nvim -*- lexical-binding: t; -*-


;;; Commentary:
;;
;; This implements a "remote" operator flow similar to flash.nvim.
;; Usage (Evil): type an operator (e.g. y/d/c), then press 'r'.
;; - Cancels the pending operator
;; - Use flash to jump to a remote location/window
;; - Re-triggers the same operator to read a motion at the remote location
;; - The operator is performed on that range
;; - Cursor and windows are restored to the original position
;;
;;; Code:

(require 'cl-lib)
(require 'evil)
(require 'flash-emacs)

(defgroup flash-emacs-remote nil
  "Remote operator integration for flash-emacs."
  :group 'convenience
  :prefix "flash-emacs-remote-")

(defcustom flash-emacs-remote-restore t
  "When non-nil, restore original window configuration and cursor after remote op."
  :type 'boolean
  :group 'flash-emacs-remote)


;; Saved state
(defvar flash-emacs-remote--saved-operator nil)
(defvar flash-emacs-remote--saved-register nil)
(defvar flash-emacs-remote--saved-window-config nil)
(defvar flash-emacs-remote--saved-window nil)
(defvar flash-emacs-remote--saved-point nil)
(defvar flash-emacs-remote--saved-operator-keys nil)

(defun flash-emacs-remote--save-state ()
  "Save operator/window state for restoration."
  (setq flash-emacs-remote--saved-operator evil-this-operator
        flash-emacs-remote--saved-register evil-this-register
        flash-emacs-remote--saved-window-config (current-window-configuration)
        flash-emacs-remote--saved-window (selected-window)
        flash-emacs-remote--saved-point (point-marker)))

(defun flash-emacs-remote--restore-state ()
  "Restore window configuration and point (if enabled)."
  (when (and flash-emacs-remote-restore
             (not flash-emacs-remote--operator-active))
    (when (window-configuration-p flash-emacs-remote--saved-window-config)
      (set-window-configuration flash-emacs-remote--saved-window-config))
    (when (window-live-p flash-emacs-remote--saved-window)
      (select-window flash-emacs-remote--saved-window))
    (when (markerp flash-emacs-remote--saved-point)
      (goto-char flash-emacs-remote--saved-point))))

(defun flash-emacs-remote--operator-keys-minus-r ()
  "Return the key vector used to start the operator, without the trailing 'r'."
  (let* ((vec (this-command-keys-vector))
         (len (length vec)))
    (when (> len 0)
      (cl-subseq vec 0 (1- len)))))

(defun flash-emacs-remote--capture-operator-keys ()
  "Capture the operator key sequence (including register/count), minus trailing 'r'."
  (let* ((vec (this-command-keys-vector))
         (len (length vec)))
    (when (> len 0)
      (cl-subseq vec 0 (1- len)))))

(defun flash-emacs-remote--restore-when-op-done ()
  "Restore original window/point when operator is finished.
Waits until Evil leaves operator state; if in insert state (after change),
restores after exiting insert."
  (cond
   ;; still waiting for motion
   ((eq evil-state 'operator)
    (run-at-time 0.03 nil #'flash-emacs-remote--restore-when-op-done))
   ;; editing after change -- defer to insert leave
   ((eq evil-state 'insert)
    (add-hook 'evil-insert-state-exit-hook
              (lambda ()
                (remove-hook 'evil-insert-state-exit-hook #'ignore)
                (flash-emacs-remote--restore-state))
              :append :local))
   ;; otherwise done, restore now
   (t (flash-emacs-remote--restore-state))))

(defvar flash-emacs-remote--invoking nil)
(defvar flash-emacs-remote--op-called-once nil)
(defvar flash-emacs-remote--operator-active nil)

(defun flash-emacs-remote--with-single-op (fn &rest args)
  "Allow only the first operator call while remote is invoking."
  (if (and flash-emacs-remote--invoking flash-emacs-remote--op-called-once)
      ;; swallow duplicate immediate invocation
      (message "[flash-remote] suppress duplicate operator")
    (let ((res (apply fn args)))
      (when flash-emacs-remote--invoking
        (setq flash-emacs-remote--op-called-once t))
      res)))

(defun flash-emacs-remote--install-op-guards ()
  (advice-add 'evil-yank :around #'flash-emacs-remote--with-single-op)
  (advice-add 'evil-delete :around #'flash-emacs-remote--with-single-op)
  (advice-add 'evil-change :around #'flash-emacs-remote--with-single-op))

(defun flash-emacs-remote--remove-op-guards ()
  (advice-remove 'evil-yank #'flash-emacs-remote--with-single-op)
  (advice-remove 'evil-delete #'flash-emacs-remote--with-single-op)
  (advice-remove 'evil-change #'flash-emacs-remote--with-single-op))


(defun flash-emacs-remote--schedule-restore ()
  "Schedule restoration based on the current Evil state.
For change operator, wait until insert mode is exited."
  (cond
   ;; If in insert state after change operation, wait for insert-leave
   ((eq evil-state 'insert)
    (add-hook 'evil-insert-state-exit-hook
              (lambda ()
                (remove-hook 'evil-insert-state-exit-hook #'flash-emacs-remote--delayed-restore)
                (flash-emacs-remote--restore-state))
              nil t))
   ;; For other operators, restore immediately
   (t (flash-emacs-remote--restore-state))))

;;;###autoload
(defun flash-emacs-remote ()
  "Start a remote operation (press during Evil operator-pending)."
  (interactive)
  (unless (eq evil-state 'operator)
    (user-error "flash-remote: must be called from Evil operator-pending state"))
  ;; Save operator/register/env
  (setq flash-emacs-remote--saved-operator evil-this-operator
        flash-emacs-remote--saved-register evil-this-register)
  (flash-emacs-remote--save-state)

  ;; Schedule the remote operation to happen AFTER the current command completes
  ;; This prevents Evil from seeing anything as the motion for the current operator
  (run-at-time 0 nil
               (lambda ()
                 ;; Jump remotely 
                 (flash-emacs-jump)
                 
                 ;; Restart the same operator at the remote point
                 (evil-repeat-abort)
                 (setq flash-emacs-remote--invoking t
                       flash-emacs-remote--op-called-once nil
                       flash-emacs-remote--operator-active t)
                 (flash-emacs-remote--install-op-guards)
                 (unwind-protect
                     (when (functionp flash-emacs-remote--saved-operator)
                       (let ((evil-this-register flash-emacs-remote--saved-register))
                         (evil-without-repeat
                           (call-interactively flash-emacs-remote--saved-operator))))
                   (setq flash-emacs-remote--invoking nil
                         flash-emacs-remote--operator-active nil)
                   ;; cleanup guards shortly after
                   (run-at-time 0.05 nil #'flash-emacs-remote--remove-op-guards)
                   ;; restore original position after operator completes
                   (when flash-emacs-remote-restore
                     (flash-emacs-remote--schedule-restore)))))

  ;; For the current operator, provide a dummy motion that does nothing
  (evil-motion-range (point) (point)))

;;;###autoload
(defun flash-emacs-remote-setup ()
  "Bind 'r' in Evil operator state to remote operation."
  (interactive)
  (define-key evil-operator-state-map (kbd "r") #'flash-emacs-remote))

;; Enable by default
(flash-emacs-remote-setup)

(provide 'flash-emacs-remote)

;;; flash-emacs-remote.el ends here
