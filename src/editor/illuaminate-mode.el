(defcustom illuaminate-illuaminate-lsp-executable "illuaminate-lsp"
  "The executable of the Illuaminate Language Server.

This should be a string containing the name or path of the
executable. This defaults to \"illuaminate-lsp\"."
  :type '(choice (const :tag "Default executable" "illuaminate-lsp")
                  (file :tag "Name or path"))
  :tag "illuaminate-lsp executable"
  :group 'illuaminate
  :risky t)

(defun illuaminate--lsp-command ()
  "The command to start the LSP server. This can be taken as a
  function reference in order to defer evaluation of
  `illuaminate-illuaminate-lsp-executable'"
  `(,illuaminate-illuaminate-lsp-executable))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'illuaminate--lsp-command)
    :major-modes '(lua-mode)
    :server-id 'illuaminate-lsp)))
