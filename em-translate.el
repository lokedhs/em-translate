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
  (list url (http-post-encode-fields fields charset) charset
        (append `(("Content-Type" . ,(http-post-content-type
                                      "application/x-www-form-urlencoded"
                                      charset)))
                extra-headers))
  (http-post-simple-internal url (http-post-encode-fields fields charset) charset
                             (append `(("Content-Type" . ,(http-post-content-type
                                                           "application/x-www-form-urlencoded"
                                                           charset)))
                                     extra-headers)))

(defvar em-translate-languages-map '(("af" "Afrikaans")
                                     ("sq" "Albanian")
                                     ("ar" "Arabic")
                                     ("az" "Azerbaijani")
                                     ("eu" "Basque")
                                     ("bn" "Bengali")
                                     ("be" "Belarusian")
                                     ("bg" "Bulgarian")
                                     ("ca" "Catalan")
                                     ("zh-CN" "Chinese Simplified")
                                     ("zh-TW" "Chinese Traditional")
                                     ("hr" "Croatian")
                                     ("cs" "Czech")
                                     ("da" "Danish")
                                     ("nl" "Dutch")
                                     ("en" "English")
                                     ("eo" "Esperanto")
                                     ("et" "Estonian")
                                     ("tl" "Filipino")
                                     ("fi" "Finnish")
                                     ("fr" "French")
                                     ("gl" "Galician")
                                     ("ka" "Georgian")
                                     ("de" "German")
                                     ("el" "Greek")
                                     ("gu" "Gujarati")
                                     ("ht" "Haitian Creole")
                                     ("iw" "Hebrew")
                                     ("hi" "Hindi")
                                     ("hu" "Hungarian")
                                     ("is" "Icelandic")
                                     ("id" "Indonesian")
                                     ("ga" "Irish")
                                     ("it" "Italian")
                                     ("ja" "Japanese")
                                     ("kn" "Kannada")
                                     ("ko" "Korean")
                                     ("la" "Latin")
                                     ("lv" "Latvian")
                                     ("lt" "Lithuanian")
                                     ("mk" "Macedonian")
                                     ("ms" "Malay")
                                     ("mt" "Maltese")
                                     ("no" "Norwegian")
                                     ("fa" "Persian")
                                     ("pl" "Polish")
                                     ("pt" "Portuguese")
                                     ("ro" "Romanian")
                                     ("ru" "Russian")
                                     ("sr" "Serbian")
                                     ("sk" "Slovak")
                                     ("sl" "Slovenian")
                                     ("es" "Spanish")
                                     ("sw" "Swahili")
                                     ("sv" "Swedish")
                                     ("ta" "Tamil")
                                     ("te" "Telugu")
                                     ("th" "Thai")
                                     ("tr" "Turkish")
                                     ("uk" "Ukrainian")
                                     ("ur" "Urdu")
                                     ("vi" "Vietnamese")
                                     ("cy" "Welsh")
                                     ("yi" "Yiddish")))

(defun em-translate--select-source-language ()
  (let ((completion-ignore-case t))
    (let ((s (completing-read "Language: " (mapcar #'cadr em-translate-languages-map) nil t)))
      (let ((found (cl-find s em-translate-languages-map :test #'equal :key #'cadr)))
        (unless found
          (error "Can't find language"))
        (car found)))))

;;;###autoload
(defun em-translate-paragraph-popup (&optional source)
  "Translate the paragraph at point and display the translation in a popup.
With prefix arg, ask for the source language."
  (interactive)
  (when current-prefix-arg
    (setq source (em-translate--select-source-language)))
  (let ((translation (em-translate-string (save-excursion
                                            (backward-paragraph)
                                            (let ((start (point)))
                                              (forward-paragraph)
                                              (buffer-substring start (point))))
                                          source)))
    (unless source
      (em-translate--display-detected (cadr translation)))
    (popup-tip (em-translate--trim-space (car translation)))))

(defun em-translate-string (text &optional source target)
  "Translate the given string and return it as a string. An optional parameter
`target' indicates the language code to translate to (defaults to the value of
`em-translate-lang')."
  (when (equal em-translate-google-apikey "")
    (error "em-translate-google-apikey has not been configured"))
  (let ((url-result (em-translate--http-post-simple "https://www.googleapis.com/language/translate/v2"
                                                    `((key    . ,em-translate-google-apikey)
                                                      (target . ,(or target em-translate-lang))
                                                      (q      . ,text)
                                                      (format . "text")
                                                      ,@(if source (list (cons 'source source))))
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
             (text (decode-coding-string (cdr (assoc 'translatedText translation-entry)) 'utf-8)))
        (list text detected-language)))))

(defun em-translate--display-detected (lang)
  (let ((v (cl-find lang em-translate-languages-map :test #'equal :key #'car)))
    (message "Detected source language: %s" (or (cadr v) lang))))

(defun em-translate--insert-to-new (text &optional source)
  (let ((translated (em-translate-string text source)))
    (switch-to-buffer (get-buffer-create "*Translate Result*"))
    (setq buffer-read-only nil)
    (em-translate-mode)
    (delete-region (point-min) (point-max))
    (insert (car translated))
    (fill-region (point-min) (point-max) nil t)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (unless source
      (em-translate--display-detected (cadr translated)))))

;;;###autoload
(defun em-translate-region (start end &optional source)
  "Translate the content of the current region and display the result in a new buffer.
With prefix arg, ask for the source language."
  (interactive "r")
  (when current-prefix-arg
    (setq source (em-translate--select-source-language)))
  (em-translate--insert-to-new (buffer-substring start end) source))

;;;###autoload
(defun em-translate-buffer (&optional source)
  "Translate the content of the buffer and display the result in a new buffer.
With prefix arg, ask for the source language."
  (interactive)
  (when current-prefix-arg
    (setq source (em-translate--select-source-language)))
  (em-translate--insert-to-new (buffer-string)) source)

;;;###autoload
(defun em-translate-kill-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*Translate Result*")))
    (when buffer
      (kill-buffer buffer))))

(provide 'em-translate)
