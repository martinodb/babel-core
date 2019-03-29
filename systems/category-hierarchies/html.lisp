(in-package :type-hierarchies)

(export '(type-hierarchy->image))

(defun type-hierarchy->image (type-hierarchy &key (render-program "dot") weights? colored-edges-0-1 path file-name format (open nil))
  "makes an image from a type hierarchy"
  ;; Let's specify a paths and filenames ;;
  (let* ((path (or path wi::*graphviz-output-directory*))
         (format (or format "png"))
         (file-name (or file-name (downcase (gensym "TYPE-HIERARCHY-"))))
         (dot-file (merge-pathnames path (make-pathname :name file-name
                                                        :type "dot")))
         (image-file (merge-pathnames path (make-pathname :name file-name
                                                          :type format))))
    (ensure-directories-exist path)
    ;; Let's make the dot file ;;    
    (visualize-type-hierarchy (graph type-hierarchy) :file dot-file :weights? weights? :colored-edges-0-1 colored-edges-0-1)
    ;; Let's make the image file ;;
    (run-prog render-program :args (list (mkstr "-T" format)
                                (format nil "~a" dot-file)
                                (mkstr "-o" (format nil "~a" image-file)))
     :wait t)
    ;; Let's open it! ;;
    ;; although we run dot with :wait t, on some machines it might
    ;; happen that the resulting image is not accessible yet, so we
    ;; wait. Although we also don't wait for more than 5 seconds
    ;; because the file might, for some erroneous reason, never be
    ;; written (e.g. dot is not found on the system).
    (loop for i from 1 to 100
          until (probe-file image-file)
          do (sleep 0.05))
    ;; try to open it
    (when open 
      (cond 
       ((equal (software-type) "Darwin")
        (run-prog "open" :args (list (format nil "~a" image-file))))
       ((equal (software-type) "Linux")
        (run-prog "see" :args (list (format nil "~a" image-file))))
       ((equal (software-type) "Microsoft Windows")
        (run-prog "cmd" 
                  :args (list "/C"
                              (string-replace 
                               (format nil "\"c:~a\"" image-file) "/" "\\"))))))
    image-file))

(defmethod make-html ((type-hierarchy type-hierarchy) &key (weights? nil) (colored-edges-0-1 t) (render-program "dot") &allow-other-keys)
  "generates html code for a type-hierarchy"
  (let* ((path (type-hierarchy->image type-hierarchy :open nil :format "svg" :weights? weights? :colored-edges-0-1 colored-edges-0-1 :render-program render-program))
         (svg (with-open-file (stream path :direction :input) 
                (let ((lines nil) (line nil))
                  (loop do (setf line (read-line stream nil))
                        ;; skip stuff before <svg  ...
                        until (and (> (length line) 3) (equal (subseq line 0 4) "<svg")))
                  (loop ;; skip comments
                        do (unless (and (> (length line) 3) (equal (subseq line 0 4) "<!--"))
                             (push line lines))
                        (setf line (if (equal line "</svg>")
                                     nil
                                     (read-line stream nil)))
                        while line)
                  (close stream)
                  (reduce #'string-append  (reverse lines))))))
    `((div) ,svg)))
