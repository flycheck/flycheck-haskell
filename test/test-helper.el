(require 'f)

(defvar flycheck-haskell-test-path
  (f-dirname (f-this-file)))

(defvar flycheck-haskell-code-path
  (f-parent flycheck-haskell-test-path))

(require 'flycheck-haskell (f-expand "flycheck-haskell.el" flycheck-haskell-code-path))
