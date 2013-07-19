;;; -*- lexical-binding: t -*-

(require 'http-post-simple)
(require 'json)
(require 'popup)
(require 'cl)

(defvar em-translate-mode nil)

;;;###autoload
(defcustom em-translate-google-apikey ""
  "The Google API key for the translation service"
  :type 'string
  :group 'em-translate)

;;;###autoload
(defcustom em-translate-lang "en"
  "The language that the service should translate text to"
  :type 'string
  :group 'em-translate)

(defface em-translate-text-face
  '((default :foreground "red")) "Face used to display translated text")

(defvar em-translate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'em-translate-kill-buffer)
    map)
  "Keymap for translation buffers")

(define-derived-mode em-translate-mode text-mode "Translate"
  "Major mode for viewing translation results"
  (use-local-map em-translate-mode-map))

(defun em-translate--trim-space (str)
  "Trim leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun* em-translate--http-post-simple (url fields &key charset extra-headers)
  "Like `http-post-simple' but use &key parameters and adds `extra-headers' parameter"
  (http-post-simple-internal url (http-post-encode-fields fields charset) charset
                             (append `(("Content-Type" . ,(http-post-content-type
                                                           "application/x-www-form-urlencoded"
                                                           charset)))
                                     extra-headers)))

(defun em-translate-paragraph-popup ()
  "Translate the paragraph at point nad display the translation in a popup"
  (interactive)
  (let ((translation (em-translate-string (save-excursion
                                                   (backward-paragraph)
                                                   (let ((start (point)))
                                                     (forward-paragraph)
                                                     (buffer-substring start (point)))))))
    (message "Language detected as: %s" (cadr translation))
    (popup-tip (em-translate--trim-space (car translation)))))

(defun em-translate-string (text &optional target)
  "Translate the given string and return it as a string. An optional parameter
`target' indicates the langiage code to translate to (defaults to the value of
`em-translate-lang'."
  (let ((url-result (em-translate--http-post-simple "https://www.googleapis.com/language/translate/v2"
                                         `((key    . ,em-translate-google-apikey)
                                           (target . ,(or target em-translate-lang))
                                           (q      . ,text)
                                           (format . "text"))
                                         :extra-headers '(("X-HTTP-Method-Override" . "GET")))))
    (unless (= (caddr url-result) 200)
      (error "Error performing HTTP request"))
    (let* ((decoded (json-read-from-string (car url-result)))
           (translations-list (cdr (assoc 'translations
                                          (cdr (assoc 'data decoded))))))
      (when (or (null translations-list) (zerop (length translations-list)))
        (error "Empty translations list"))
      (when (> (length translations-list) 1)
        (error "Expected a single translation, got %d results" (length translations-list)))
      (let* ((translation-entry (aref translations-list 0))
             (detected-language (cdr (assoc 'detectedSourceLanguage translation-entry)))
             (text (cdr (assoc 'translatedText translation-entry))))
        (list text detected-language)))))

(defun em-translate--insert-to-new (text)
  (let ((translated (em-translate-string text)))
    (switch-to-buffer (get-buffer-create "*Translate Result*"))
    (setq buffer-read-only nil)
    (em-translate-mode)
    (delete-region (point-min) (point-max))
    (insert (car translated))
    (setq buffer-read-only t)
    (message "Detected source language: %s" (cadr translated))))

(defun em-translate-region ()
  "Translate the content of the current region and display the result in a new buffer."
  (interactive)
  (em-translate--insert-to-new (buffer-substring (point) (mark))))

(defun em-translate-buffer ()
  "Translate the content of the buffer and display the result in a new buffer."
  (interactive)
  (em-translate--insert-to-new (buffer-string)))

(defun em-translate-markup-region ()
  (interactive)
  (save-restriction
    (narrow-to-region
     (goto-char (point-min))
     (loop while (< (point) (point-max))
           for previous = (point)
           do (forward-paragraph)
           do (let* ((current (point))
                     (text (buffer-substring previous current))
                     (translated-text (em-translate-string text)))
                (when (not (string= (cadr translated-text) "en"))
                  (put-text-property previous current
                                     'display (car (em-translate-string text)))
                  (put-text-property previous current
                                     'face 'em-translate-text-face)))))))

(defun em-translate-kill-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*Translate Result*")))
    (when buffer
      (kill-buffer buffer))))

(provide 'em-translate)