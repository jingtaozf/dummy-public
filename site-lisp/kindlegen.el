;;; -*- encoding:utf-8 -*-  --- 
;; 
;; Filename: kindlegen.el
;; Description: generate .mobi file from utf-8 encoded txt file based on kindlegen
;; Author: Xu Jingtao <jingtaozf@gmail.com>
;; Created: 2011.12.14 13:33:49(+0800)
;; Last-Updated: 2012.07.25 20:59:43(+0800)
;;     Update #: 190
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; The originally code are copied from here: http://bbs.mydoo.cn/thread-24568-1-1.html
;;
;;; Usage:
;; 1. switch to file buffer you want to convert
;; 2. adjust the variable *kindlegen-regex* for current buffer
;; 3. add following lines to end of buffer and re-open it.
;;    % Local Variables:
;;    % *kindlegen-regex*: "\s*\\(第[一二三四五六七八九十]+章　.*\\)"
;;    % End:
;; 4. M-x: kindlegen-toc-preview
;; 5. M-x: kindlegen-txt-to-mobi
;;

(provide 'kindlegen)

(defvar *kindlegen-command* "kindlegen")
(defvar *kindlegen-creator* "jingtao")
(defvar *kindlegen-publisher* "jingtao.net")
(defvar *kindlegen-tmpdir* "/tmp/xjt/kindlegen")
(defvar *kindlegen-regex* "^##  \\(.*\\)")

(defvar *kindlegen-cover-path* "/home/jingtao/Dropbox/book")

(defvar *kindlegen-html-begin*
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>special-english-study</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /> 
<link rel=\"stylesheet\" href=\"KUG.css\" type=\"text/css\" />
</head>
<body>")

(defvar *kindlegen-html-end*
"</body>
</html>")

(defvar *kindlegen-css* "
  /* Style Definitions */

p
{

        text-align: justify;
        text-indent: 1em;
        line-height:130%;
        margin-bottom:-0.8em;
}

.pagebreak
{
        page-break-before: always;
}

.centered
{
        text-align: center;
}

.bottom
{
        vertical-align: text-bottom;
}

.tablehead
{
        text-align: center;
        font-weight: bold
}

H1 {margin-top: 1em}
H2 {margin-top: 1em}
H3 {margin-top: 1em}
H4 {margin-top: 1em}

h2 {
        
        text-align: center;
        }


.cover {
        width:100%;
        padding:0px;
}")

(defvar *kindlegen-preface-begin* "
  <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>special-english-study</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /> 
<link rel=\"stylesheet\" href=\"KUG.css\" type=\"text/css\" />
</head>
<body>
")

(defvar *kindlegen-preface-end* "
</body>
</html>")

(defvar *kindlegen-ncx-begin* "
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE ncx PUBLIC \"-//NISO//DTD ncx 2005-1//EN\"
        \"http://www.daisy.org/z3986/2005/ncx-2005-1.dtd\">

<!--
        For a detailed description of NCX usage please refer to:
        http://www.idpf.org/2007/opf/OPF_2.0_final_spec.html#Section2.4.1
-->

<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\" xml:lang=\"en-US\">
<head>
<meta name=\"dtb:uid\" content=\"BookId\"/>
<meta name=\"dtb:depth\" content=\"2\"/>
<meta name=\"dtb:totalPageCount\" content=\"0\"/>
<meta name=\"dtb:maxPageNumber\" content=\"0\"/>
</head>
")

(defvar *kindlegen-ncx-end* "
</ncx>
")

(defvar *kindlegen-opf-begin* "
<?xml version=\"1.0\" encoding=\"utf-8\"?>

<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"2.0\" unique-identifier=\"BookId\">        
<metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:opf=\"http://www.idpf.org/2007/opf\">
          <dc:title>%s</dc:title>  
        <dc:language>en-US</dc:language>
  <meta name=\"cover\" content=\"My_Cover\" />
        <dc:identifier id=\"BookId\" opf:scheme=\"ISBN\"></dc:identifier>
        <dc:creator>%s</dc:creator>
        <dc:publisher>%s</dc:publisher>
        <dc:subject>%s</dc:subject>
          <dc:date>%s</dc:date>
  <dc:description></dc:description>
        
</metadata>

")

(defvar *kindlegen-opf-end* "
<guide>
        <reference type=\"toc\" title=\"Table of Contents\" href=\"toc.html\"></reference>
        </guide>

</package>
")

(defvar *kindlegen-toc-begin* "
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head><title>Table of Contents</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /> 
<link rel=\"stylesheet\" href=\"KUG.css\" type=\"text/css\" />
</head>
<body>

<div>
<h1><b>目录</b></h1>
<div><ul>
<li><a href=\"0.html\">前 言</a></li>
")

(defvar *kindlegen-toc-end* "
</ul></div>
<hr />
</div>
</body>
</html>
")

;;;###autoload
(defun kindlegen-opf-begin (title creator publisher subject date)
  (format *kindlegen-opf-begin* title creator publisher subject date))

(defvar *kindlegen-toc* '())

;;;###autoload
(defun kindlegen-build-toc ()
  (interactive)
  (message "build kindle toc...")
  (let ((toc))
    (save-excursion
        (goto-char (point-min))
      (while (re-search-forward *kindlegen-regex* nil t)
        (push (cons (match-string 1) (line-number-at-pos)) toc)))
    (setq *kindlegen-toc* (reverse toc))))

;;;###autoload
(defun kindlegen-toc-preview ()
  (interactive)
  (let ((toc))
    (save-excursion
        (goto-char (point-min))
      (while (re-search-forward *kindlegen-regex* nil t)
        (push (cons (match-string 1) (line-number-at-pos)) toc)))
    (setq *kindlegen-toc* (reverse toc))
    (switch-to-buffer (get-buffer-create "*Kindlegen toc*"))
    (erase-buffer)
    (insert (org-add-props "Kindlegen table of content\n" nil 'face 'bold))
    (loop for (s . l) in (reverse toc)
          do (insert (format "[%d] %s\n" l s)))))

;;;###autoload
(defun kindlegen-current-line ()
  (dired-replace-in-string
   " " "&nbsp; "
   (buffer-substring-no-properties (line-beginning-position)
                                   (line-end-position))))

;;;###autoload
(defun kindlegen-txt-to-html ()
  (interactive)
  (mkdir *kindlegen-tmpdir* t)
  (shell-command (format "rm -fr %s/*" *kindlegen-tmpdir*));clean history files.
  (message "convert txt to html...")
  (let* (preface
         htmls
         (index 0)
         (toc *kindlegen-toc*)
         (title (or (caar toc) ""))
         (line (or (cdar toc) (count-lines (point-min) (point-max)))))
    (save-excursion
      (goto-char (point-min))
      ;; add preface page if exist.
      (while (< (line-number-at-pos) line)
        (push (kindlegen-current-line) preface)
        (forward-line))
      (loop for current-title = (caar toc)
            for current-line = (cdar toc)
            for next-line = (cdadr toc)
            while toc
            do 
            (incf index)
            (setf toc (cdr toc))
            (unless next-line
              (setq next-line
                    (1+ (count-lines (point-min) (point-max)))))
            (message "collect section %s..." current-title)
            (forward-line)
            (loop with content = (list current-title)
                  for i from (1+ current-line) to (1- next-line)
                  do
                  (push (kindlegen-current-line) content)
                  (forward-line)
                  finally (push (reverse content) htmls)))
      (message "generate KUG.css file...")
      (with-current-buffer (find-file-noselect
                            (format "%s/KUG.css" *kindlegen-tmpdir*))
        (insert *kindlegen-css*)
        (save-some-buffers t)
        (kill-buffer))
      
      (message "generate preface html file...")
      (with-current-buffer (find-file-noselect
                            (format "%s/0.html" *kindlegen-tmpdir*))
        (insert *kindlegen-preface-begin*)
        (if preface
            (loop for line in (reverse preface)
                  do (insert (format "<p>%s</p>" line)) (insert "\n"))
          (insert "<h2>This book has no preface page<h2>") (insert "\n"))
        (insert *kindlegen-preface-end*)
        (save-some-buffers t)
        (kill-buffer))

      (loop for i from 1 to index
            for content in (reverse htmls)
            do
            (with-current-buffer (find-file-noselect
                                  (format "%s/%d.html" *kindlegen-tmpdir* i))
              (insert *kindlegen-html-begin*)
              (insert (format "<h2>%s</h2>\n" (car content)))
              (loop for line in (cdr content)
                    do (insert (format "<p>%s</p>\n" line)))
              (insert *kindlegen-html-end*)
              (save-some-buffers t)
              (kill-buffer))))))

;;;###autoload
(defun kindlegen-build-ncx (title author)
  (interactive)
  (ignore-errors (delete-file (format "%s/KUG.ncx" *kindlegen-tmpdir*)))
  (with-current-buffer (find-file-noselect
                        (format "%s/KUG.ncx" *kindlegen-tmpdir*))
    (insert *kindlegen-ncx-begin*)
    (insert (format "<docTitle><text>%s</text></docTitle>\n" title))
    (insert (format "<docAuthor><text>%s</text></docAuthor>\n" author))
    (insert "
  <navMap>
    <navPoint class=\"toc\" id=\"toc\" playOrder=\"0\">
      <navLabel>
        <text>目录</text>
      </navLabel>
      <content src=\"toc.html\"/>
    </navPoint>
    <navPoint class=\"chapter\" id=\"chapter_0\" playOrder=\"\">
      <navLabel>
        <text>前 言</text>
      </navLabel>
      <content src=\"0.html\"/>
   </navPoint>\n")
    (loop for i from 1
          for (title . nil) in *kindlegen-toc*
          do (insert (format 
                      "<navPoint class=\"chapter\" id=\"chapter_%d\" playOrder=\"1\">
      <navLabel>
        <text>%s</text>
      </navLabel>
      <content src=\"%d.html\"/>
   </navPoint>\n" i title i)))
    (insert *kindlegen-ncx-end*)
    (save-some-buffers t)
    (kill-buffer)))

;;;###autoload
(defun kindlegen-build-opf (title creator publisher subject cover-picture date)
  (interactive)
  (copy-file cover-picture (concat *kindlegen-tmpdir* "/" (file-name-nondirectory cover-picture)))
  (ignore-errors (delete-file (format "%s/target.opf" *kindlegen-tmpdir*)))
  (with-current-buffer (find-file-noselect
                        (format "%s/target.opf" *kindlegen-tmpdir*))
    (insert (kindlegen-opf-begin title creator publisher subject date))
    (insert "
<manifest>
  <!-- HTML content files [mandatory] -->
  <item id=\"My_Cover\" media-type=\"image/gif\" href=\"cover.png\"/>
  <item id=\"My_Table_of_Contents\" media-type=\"application/x-dtbncx+xml\" href=\"KUG.ncx\"/>
  <item id=\"toc\" media-type=\"application/xhtml+xml\" href=\"toc.html\"></item>\n
")
    (loop for i from 0 to (length *kindlegen-toc*)
          do (insert (format "
        <item id=\"item%d\" media-type=\"application/xhtml+xml\" href=\"%d.html\"></item>\n" i i)))
  
    (insert "
</manifest>
        
<spine toc=\"My_Table_of_Contents\">
  <!-- the spine defines the linear reading order of the book -->
     
             <itemref idref=\"My_Cover\"/>
        <itemref idref=\"My_Table_of_Contents\"/>
        <itemref idref=\"toc\"/>\n")
    (loop for i from 0 to (length *kindlegen-toc*)
          do (insert (format "<itemref idref=\"item%d\"/>\n" i)))
    (insert "</spine>\n")
    (insert *kindlegen-opf-end*)
    (save-some-buffers t)
    (kill-buffer)))

;;;###autoload
(defun kindlegen-build-toc-html ()
  (interactive)
  (ignore-errors (delete-file (format "%s/toc.html" *kindlegen-tmpdir*)))
  (with-current-buffer (find-file-noselect
                        (format "%s/toc.html" *kindlegen-tmpdir*))
    (insert *kindlegen-toc-begin*)
    (loop for i from 1
          for (title . nil) in *kindlegen-toc*
          do (insert (format "<li><a href=\"%d.html\">%s</a></li>\n"  i title)))
    (insert *kindlegen-toc-end*)
    (save-some-buffers t)
    (kill-buffer)))

;;;###autoload
(defun kindlegen-txt-to-mobi (&optional author title cover-picture)
  (interactive)
  (let ((author (or author (read-from-minibuffer "author: " "jingtao")))
        (title (or title (read-from-minibuffer "title: " x-last-selected-text-clipboard)))
        (cover-picture (or cover-picture
                           (expand-file-name
                            (read-file-name "Cover Picture: " *kindlegen-cover-path*)))))
    (kindlegen-build-toc)
    (kindlegen-txt-to-html)
    (kindlegen-build-ncx title author)
    (kindlegen-build-opf title author *kindlegen-publisher* title cover-picture
                         (format-time-string "%Y.%m.%d"))
    (kindlegen-build-toc-html)
    (let* ((target (concat (subseq buffer-file-name 0
                                   (position ?. buffer-file-name :from-end t))
                           ".mobi"))
           (default-directory *kindlegen-tmpdir*))
      (shell-command (format "kindlegen %s/target.opf" *kindlegen-tmpdir*))
      (when (file-exists-p (format "%s/target.mobi" *kindlegen-tmpdir*))
        (copy-file (format "%s/target.mobi" *kindlegen-tmpdir*) target t)
        (message "%s generated" target)))))
