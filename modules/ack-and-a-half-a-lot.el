;;; ack-and-a-half-a-lot --- Compatibility layer

;; Setup:

;; (require 'ack-and-a-half)
;; ;; ack-and-a-half setup...
;; (require 'grep-a-lot)
;; ;; grep-a-lot setup...
;; (require 'ack-and-a-half-a-lot)

(grep-a-lot-advise ack-and-a-half-run)

;; grep-a-lot needs the following to work:
;; 1) grep-a-lot mode-name needs to be "^i?grep$".
;     The buffer name (*grep*<N>) is only set when the mode name is "^i?grep$", as
;;    defined by grep-a-lot-buffer-name-function .
;; 2) grep-a-lot-grep-setup-hook needs to run when the grep is started. grep-a-lot hooks into 'grep-setup-hook.

;; for ack-and-a-half, we fix it like this:

;; 1) grep-a-lot uses defadvice to set compilation-buffer-name-function, but it is overwritten by
;;    ack-and-a-half with its own dynamically bound variable in ack-and-a-half-run.
;;    => Solve this and point 1) at once by overwriting ack-buffer-name.
(defun ack-buffer-name (name)
  (grep-a-lot-buffer-name-function "grep"))

;; 2) Run the setup hook.
(defadvice ack-and-a-half-mode-setup (around grep-a-lot-ack-and-a-half-mode-setup activate)
  ad-do-it
  (grep-a-lot-grep-setup-hook)
  ad-return-value)

(provide 'ack-and-a-half-a-lot)
