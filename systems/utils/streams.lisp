(in-package :utils)

(export '(stream->list))

(defun stream->list (stream &key number-of-lines)
  "Returns a list of the next number-of-lines lines of the stream.
   If number-of-lines is unspecified, the entire stream is processed
   untill an empty line is encountered." 
  (let (lines)
    (do ((line (read-line stream nil nil)
               (read-line stream nil nil))
         (n 1 (1+ n)))
        ((if number-of-lines
           (or (> n number-of-lines) (null line))
           (null line))
         (reverse lines))
      (push line lines))))