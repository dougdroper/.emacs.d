;;; editing --- Tools that make editing files generally easier
;;; Commentary:
;;; Code:
(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d"         . smart-hungry-delete-forward-char)
         ("C-S-d"       . smart-hungry-delete-backward-char))
  :config (smart-hungry-delete-add-default-hooks))

(use-package smart-newline
  :bind ("RET" . smart-newline))

(use-package move-text
  :bind (("s-P" . move-text-up)
         ("s-N" . move-text-down)))

(use-package smartparens-config
  :ensure nil :after smartparens)

(use-package smartparens
  :commands (smartparens)
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . show-smartparens-mode))
  :bind (("s-e" . sp-end-of-sexp)
         ("s-a" . sp-beginning-of-sexp)

         ("s-f" . sp-forward-sexp)
         ("s-b" . sp-backward-sexp)

         ("s-p" . sp-backward-up-sexp)
         ("s-n" . sp-down-sexp)

         ("<s-backspace>"   . sp-splice-sexp-killing-around)
         ("<S-s-backspace>" . sp-backward-kill-sexp)
         ("s--"             . sp-forward-slurp-sexp)

         :map emacs-lisp-mode-map
         ("C-k" . sp-kill-whole-line)))

(use-package embrace
  :bind (("C-," . embrace-add)
         ("C-<" . embrace-change)))

(use-package expand-region
  :bind ("M-q" . er/expand-region))

(use-package electric-operator
  :commands (electric-operator-get-rules-for-mode
             electric-operator-add-rules-for-mode
             )
  :hook (prog-mode . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode
   'emacs-lisp-mode
   (cons "-" nil)
   (cons "." " . ")))

(use-package visual-regexp-steroids
  :bind (("s-r" . vr/replace)
         ("s-R" . vr/query-replace)))

(delete-selection-mode 1)

(bind-keys
 ("C-K" . kill-whole-line)
 ("M-D" . backward-kill-word)
 ("C-j" . join-line))

(provide 'editing)
;;; editing.el ends here
