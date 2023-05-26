(require 'ox-html)

(with-temp-buffer
  (insert-file-contents-literally "./README.org")
  (org-mode)
  (org-html-export-as-html)
  (write-file "./README.html"))
