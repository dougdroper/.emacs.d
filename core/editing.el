;;; editing --- Tools that make editing files generally easier
;;; Commentary:
;;; Code:
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
         ("s-n" . sp-backward-down-sexp)

         ("<s-backspace>"   . sp-splice-sexp)
         ("<S-s-backspace>" . sp-backward-kill-sexp)
         ("s--"             . sp-forward-slurp-sexp)
         ("s-l"             . sp-forward-slurp-sexp)
         ("H-{"             . sp-forward-slurp-sexp)
         ("H-("             . sp-forward-slurp-sexp)
         ("H-["             . sp-forward-slurp-sexp)

         :map emacs-lisp-mode-map
         ("C-S-k" . sp-kill-whole-line)))

(use-package embrace
  :bind (("C-," . embrace-add)
         ("C-<" . embrace-change)))

(use-package expand-region
  :demand t
  :bind (("M-q" . er/expand-region)
         :map core-mode-map
         ("q" . er/copy-string)
         ("s" . er/copy-symbol)
         ("p" . er/copy-inside-pairs))
  :defines (defcopy er/copy-string er/copy-symbol er/copy-inside-pairs)
  :commands (er/mark-symbol er/mark-inside-quotes)
  :init
  (defmacro defcopy (name f)
    `(defun ,(intern (format "er/copy-%s" name)) ()
       (interactive)
       (save-excursion
         (call-interactively ',f)
         (kill-ring-save (region-beginning) (region-end)))))

  :config
  (defcopy "inside-pairs" er/mark-inside-pairs)
  (defcopy "string" er/mark-inside-quotes)
  (defcopy "symbol" er/mark-symbol))

(use-package visual-regexp
  :bind (("s-r" . vr/replace)
         ("s-R" . vr/query-replace)
         :map vr/minibuffer-keymap
         ("M-c" . (lambda () (interactive) (insert "\\(.*?\\)") (backward-char 5)))
         ("M-w" . (lambda () (interactive) (insert "\\(\\w+\\)") (backward-char 5)))))

(use-package duplicate-thing
  :config
  (defun duplicate-thing-replace ()
    (interactive)
    (call-interactively 'duplicate-thing)
    (call-interactively 'vr/query-replace))

  (defun duplicate-thing-quit ()
    (interactive)
    (save-excursion
      (call-interactively 'duplicate-thing)
      (keyboard-quit)))

  :bind (("s-d" . duplicate-thing-quit)
         ("s-D" . duplicate-thing-replace)))

(use-package multiple-cursors
  :hook (multiple-cursors-mode
         . (lambda ()
             (setq mc/always-run-for-all t)
             (bind-keys
              :map mc/keymap
              ("<backspace>" . delete-backward-char)
              ("<return>"    . smart-newline)
              ("C-|"         . mc/vertical-align-with-space)
              ("C-1"         . mc/insert-numbers))))
  :bind (("<down>"      . mc/mark-next-like-this)
         ("<S-down>"    . mc/skip-to-next-like-this)
         ("<up>"        . mc/mark-previous-like-this)
         ("<S-up>"      . mc/skip-to-previous-like-this)
         ("<right>"     . mc/mark-all-like-this)
         ("<left>"      . mc/edit-lines)
         ))

(use-package multi-line
  :bind (("C-c [" . multi-line-single-line)
         ("C-c ]" . multi-line)))

(use-package kmacro
  :ensure nil
  :bind (("H-["   . kmacro-start-macro)
         ("H-r"   . kmacro-start-macro)
         ("H-]"   . kmacro-end-macro)
         ("H-SPC" . kmacro-end-or-call-macro)
         ("H-e"   . kmacro-end-or-call-macro)))

(use-package sudo-edit
  :commands (sudo-edit))

(defun show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(delete-selection-mode 1)

(bind-keys
 ("C-x a a" . align-current)
 ("M-u"     . upcase-char)
 ("M-l"     . downcase-word)
 ("C-k"     . kill-line)
 ("C-S-k"   . kill-whole-line)
 ("C-x C-p" . show-buffer-file-name)
 ("M-D"     . backward-kill-word)
 ("s-/"     . comment-or-uncomment-region)
 ("C-j"     . join-line))

(provide 'editing)
;;; editing.el ends here
