;;                                                              
;; JSON Corpus Processing                                       
;;                                                              
;; System developed for parallel processing of large JSON files.
;; Uses the 'jq' command line tool and cl-json for encoding and
;; decoding.
;; 
;; The corpus processing already takes care of encoding/decoding.
;; The 'function' will receive decoded JSON data (i.e. an alist)
;; and should send back an alist that will than be encoded to
;; string before written to output.
;;
;; Regarding the input:
;; By default, we assume the JSON file has an array at the top
;; level that contains JSON objects.
;; Alternatively, if a symbol is passed to 'object-key', the
;; JSON file has an object at the top level and an array as
;; value for that key.
;; Example: if the object-key is 'data', we read .data in the
;; JSON file
;; Finally, if 'object-key' is a list, the array we will iterate
;; over is situated multiple levels deep in the JSON file.
;; Example: if the object-key is '(foo bar), we read .foo.bar
;; in the JSON file.
;;
;; Regarding the output:
;; The output will always consist of a JSON array on the top level
;; with JSON objects inside. These objects are the ones returned
;; by 'function'.
;;
;; It is possible to pass keyword arguments to 'function', by
;; providing them with 'function-kwargs'. Pass them as a plist
;; e.g. '(:key-1 value-1 :key-2 value-2)
;;                                                            
;; Jens - June 2018                                             
;;                                                              

;; Load with:
;; (ql:quickload :corpus-processing)

(in-package :utils)

(export '(json-process-corpus))

(defun empty-directory-p (dirpath)
  "Check if the given directory is empty"
  (null (cl-fad::list-directory dirpath)))

(defun delete-all-files-in-folder (dirpath)
  "Remove all files in the given directory"
  (let ((all-files (cl-fad::list-directory dirpath)))
    (mapcar #'delete-file all-files)))

(defun all-nonempty-json-files-in-folder (dirpath)
  "Return all files in the given directory that are
   non-empty JSON files. The non-emptiness is checked
   using the 'jq' tool"
  (let ((all-files (cl-fad::list-directory dirpath)))
    (remove-if-not #'(lambda (pname)
                       (and (string= (pathname-type pname) "json")
                            (> (jq-number-of-objects pname) 0)))
                   all-files)))

(defun jq-installed-p ()
  "Checks if the jq tool is installed"
  (exec-and-return "which" "jq"))

(defun quote-string (string)
  "Adds extra quotes in to a string"
  (format nil "\"~a\"" string))

(defun unquote-string (string)
  "Removes extra quotes from a string"
  (subseq string 1 (1- (length string))))

(defun jq-json-array-p (inputfile)
  "Checks if the json data is an array"
  (string= (unquote-string
            (first
             (exec-and-return "jq" "type" (namestring inputfile))))
           "array"))
    
(defun jq-number-of-objects (inputfile)
  "Returns the number of objects in the json file."
  (parse-integer
   (first
    (exec-and-return "jq" "length" (namestring inputfile)))))

(defun alistp (value)
  (or (null value)
      (and (consp value)
           (mapl (lambda (tree)
                   (unless (and (consp (first tree))
                                (not (consp (first (first tree))))
                                (listp (rest tree)))
                     (return-from alistp nil)))
                 value)
           t)))

#+:LISPWORKS7+
(defun all-threads-dead (thread-list)
  "Returns t if all processes in thread-list are of status dead."
  (let ((all-dead t)
        (statuses (mapcar #'mp:process-whostate thread-list)))
    (dolist (status statuses)
      (unless (equalp "dead" status)
        (setf all-dead nil)))
    all-dead))

#+:CCL
(defun all-threads-dead (thread-list)
  "Returns t if all processes in thread-list are of status dead."
  (let ((all-dead t)
        (statuses (mapcar #'ccl:process-whostate thread-list)))
    (dolist (status statuses)
      (unless (equalp "reset" status)
        (setf all-dead nil)))
    all-dead))

(defun read-json-entries (inputfile start end)
  "Reads the json entries between index start and end from the inputfile"
  (let* ((value (exec-and-return "jq"
                                 (format nil ".[~a:~a]" start end)
                                 (namestring inputfile))))
    (cl-json:decode-json-from-string
     (list-of-strings->string value :separator ""))))

(defun write-batch-output (list-of-alists batch-start batch-end dir)
  (let* ((filename (format nil "json-batch-~a-~a" batch-start batch-end))
         (outpath (merge-pathnames dir (make-pathname :name filename :type "json"))))
    (with-open-file (stream outpath :direction :output)
      (write-string (cl-json:encode-json-to-string list-of-alists) stream))))

#+:LISPWORKS7+
(defun process-list-of-json-objects (function function-kwargs list-of-alists mailbox)
  "Applies function to list-of-json-objects and returns the processed list-of-objects"
  (mailbox-send mailbox
                (cons (car list-of-alists)
                      (mapcar #'(lambda (alist)
                                  (if function-kwargs
                                    (apply function alist function-kwargs)
                                    (funcall function alist)))
                              (cdr list-of-alists)))))

#+:CCL
(defun process-list-of-json-objects (function function-kwargs list-of-alists)
  "Applies function to list-of-json-objects and returns the processed list-of-objects"
  (cons (car list-of-alists)
        (mapcar #'(lambda (alist)
                    (if function-kwargs
                      (apply function alist function-kwargs)
                      (funcall function alist)))
                (cdr list-of-alists))))

(defun json-process-corpus (&key function
                                 function-kwargs
                                 inputfile
                                 outputfile
                                 (tmpdir (babel-pathname :directory '(".tmp" "json-process-corpus-tmp")))
                                 (remove-tmp-data t)
                                 (process-batch-fn #'identity)
                                 (number-of-threads 4)
                                 (number-of-objects-per-thread 2000))
  "Applies 'function' with 'function-kwargs' to every JSON object in the inputfile.
   This is split over batches, where each batch is the number of threads times the number of objects per thread.
   The function 'process-batch-fn' is applied to each batch result. Whatever this function returns is encoded
   as json and written to the outputfile. This function can be useful for side-effects (keeping accuracy measures).
   A higher number-of-threads results in a higher speed (if these threads are available).
   A higher number-of-entries-per-thread results in a higher speed, but also in a higher memory use."
  
  ;; Detect if the jq command line tool is installed
  (unless (jq-installed-p)
    (error (format nil "Could not detect the 'jq' command line tool.~%Install it with homebrew:~%~% > brew install jq~%")))

  ;; Validate the input
  (assert (string= (pathname-type inputfile) "json"))
  (assert (string= (pathname-type outputfile) "json"))
  (assert (jq-json-array-p inputfile))

  ;; Printing some information
  (format t "~%~%****************** Started Corpus Processing ******************")
  (format t "~%~%Inputfile: ~a" inputfile)
  (format t "~%Outputfile: ~a" outputfile)
  (format t "~%Applying function: ~a" function)
  (when function-kwargs (format t "~%Passing function arguments: ~a" function-kwargs))
  (format t "~%Storing temporary data in: ~a" tmpdir)
  (when process-batch-fn (format t "~%Applying function ~a to every batch result" process-batch-fn))
  (format t "~%~%Threads: ~a" number-of-threads)
  (format t "~%Objects per thread: ~a" number-of-objects-per-thread)
  (format t "~%Objects per batch: ~a" (* number-of-threads number-of-objects-per-thread))

  ;; Count number of objects in file
  (format t "~%Total number of objects: ")
  (let ((start-time (get-universal-time))
        (number-of-objects (jq-number-of-objects inputfile))
        (number-of-objects-per-batch (* number-of-objects-per-thread number-of-threads)))
    (format t "~a" number-of-objects)

    ;; Clear the outputfile
    ;; (and for CCL: add the open square bracket)
    (with-open-file (stream outputfile :direction :output :if-exists :supersede :if-does-not-exist :create)
      (declare (ignore stream))
      #+CCL (write-string "[" stream))
    
    ;; Check if the tmpdir exist and if it is empty
    (ensure-directories-exist tmpdir)
    (unless (empty-directory-p tmpdir)
      (delete-all-files-in-folder tmpdir))

    ;; Divide inputfile in batches, that are processed sequentially (avoids reading whole corpus in memory)
    (multiple-value-bind (number-of-complete-batches number-of-objects-in-last-batch)
        (floor number-of-objects number-of-objects-per-batch)
      (format t "~%Number of batches: ~a complete + ~a objects in extra batch.~%" number-of-complete-batches number-of-objects-in-last-batch)
      (let ((start 0)
            (end number-of-objects-per-batch))
        (do ((n 0 (1+ n)))
            ((= n number-of-complete-batches) (format t "~%(Finished)"))
          (format t "~%Started complete batch ~a/~a..." (+ 1 n) number-of-complete-batches)
          (process-batch-multi-threaded function function-kwargs inputfile outputfile tmpdir process-batch-fn
                                        number-of-threads number-of-objects-per-batch
                                        start end)
          (incf start number-of-objects-per-batch)
          (incf end number-of-objects-per-batch))
        (when (> number-of-objects-in-last-batch 0)
          (format t "~%Started extra batch...")
          (process-batch-multi-threaded function function-kwargs inputfile outputfile tmpdir process-batch-fn
                                        number-of-threads number-of-objects-in-last-batch
                                        start (+ start number-of-objects-in-last-batch))
          (format t "~%(Finished)"))))

    ;; Merge the JSON files written by every batch into outputfile
    ;; When specified, also remove the tmp data
    ;; Only for LW!
    #+LISPWORKS7+ (progn
                    (format t "~%Merging temporary data files into ~a" outputfile)
                    (let ((all-tmp-files (all-nonempty-json-files-in-folder tmpdir)))
                      (exec-and-return "jq" "-s" (format nil "add ~{~a ~}" (mapcar #'namestring all-tmp-files))
                                       ">" (namestring outputfile)))
                    (when remove-tmp-data
                      (format t "~%Removing temporary data files")
                      (delete-all-files-in-folder tmpdir)))

    ;; Only for CCL; add the closing square bracket to the output file
    #+CCL (with-open-file (stream outputfile :direction :output :if-exists :append)
            (write-string "]" stream))

    ;; Finish
    (let ((finish-time (get-universal-time)))
      (multiple-value-bind (h m s)
          (seconds-to-hours-minutes-seconds (- finish-time start-time))
        (format t "~%~%Processing took ~a hours, ~a minutes and ~a seconds." h m s)))
    (format t "~%~%***************** Finished Corpus Processing *****************")))

#+:LISPWORKS7+
(defun process-batch-multi-threaded (function
                                     function-kwargs
                                     inputfile
                                     outputfile
                                     tmpdir
                                     process-batch-fn
                                     number-of-threads
                                     number-of-objects
                                     start
                                     end)
  "reads a batch of objects from file,
   divides them over threads,
   applies function on each object,
   collects batch results,
   applies process-batch-fn on this,
   and writes the result to a file in tmpdir"
  (declare (ignorable outputfile))
  (let ((list-of-thread-batches nil)
        (batch-data (read-json-entries inputfile start end)))
    ;; Divide objects over threads
    (multiple-value-bind (entries-per-thread entries-last-thread)
        (floor number-of-objects number-of-threads)
      (format t "~%      Launching ~a threads with ~a entries and 1 thread with ~a entries "
              (- number-of-threads 1) entries-per-thread (+ entries-per-thread entries-last-thread))
      (let ((thread-start 0)
            (thread-end entries-per-thread))
        (dotimes (n (- number-of-threads 1)) ;; For each thread, except last
          (push (cons (+ 1 n)
                      (subseq batch-data thread-start thread-end))
                list-of-thread-batches)
          (incf thread-start entries-per-thread)
          (incf thread-end entries-per-thread))
        ;; For last thread
        (push (cons number-of-threads
                    (subseq batch-data thread-start (+ thread-start entries-per-thread entries-last-thread)))
              list-of-thread-batches)))
       
    ;; process each thread-batch
    (let* ((mailbox (make-mailbox :name "batch-mailbox"))
           (thread-list (mapcar #'(lambda (thread-batch)
                                    (process-run-function "json-processing" '()
                                                          #'process-list-of-json-objects
                                                          function function-kwargs thread-batch mailbox))
                                list-of-thread-batches))
           (mailbox-messages nil))
      ;; Wait for messages
      (mp:process-wait "waiting for threads to finish..." 'all-threads-dead thread-list)
      ;; Read batches from mailbox
      (while (not (mp:mailbox-empty-p mailbox))
        (push (mp:mailbox-read mailbox) mailbox-messages))
      ;; Process batches and write to tmpdir
      (let ((flattened-batch-results
             (loop for message in (sort mailbox-messages #'< :key #'car)
                   for message-content = (rest message)
                   if (alistp (first message-content))
                   append message-content
                   else
                   append (first message-content))))
        (write-batch-output
         (funcall process-batch-fn flattened-batch-results)
         start
         end
         tmpdir)))))

(defun append-to-output (batch-data outputfile start)
  "Append the batch data to the output file"
  (with-open-file (stream outputfile :direction :output :if-exists :append)
    (let ((string-data (format nil "~{~a~^, ~}"
                               (loop for obj in batch-data
                                     collect (cl-json:encode-json-to-string obj)))))
      (unless (= start 0)
        (write-string "," stream))
      (write-string string-data stream))))

#+CCL
(defun process-batch-multi-threaded (function
                                     function-kwargs
                                     inputfile
                                     outputfile
                                     tmpdir
                                     process-batch-fn
                                     number-of-threads
                                     number-of-objects
                                     start
                                     end)
  "reads a batch of objects from file,
   divides them over threads,
   applies function on each object and writes
   the output to a file in tmpdir"
  (declare (ignorable tmpdir))
  (let ((list-of-thread-batches nil)
        (batch-data (read-json-entries inputfile start end)))
    ;; Divide objects over threads
    (multiple-value-bind (entries-per-thread entries-last-thread)
        (floor number-of-objects number-of-threads)
      ;; Read from input
      (format t "~%      Launching ~a threads with ~a entries and 1 thread with ~a entries "
              (- number-of-threads 1) entries-per-thread (+ entries-per-thread entries-last-thread))
      (let ((thread-start 0)
            (thread-end entries-per-thread))
        (dotimes (n (- number-of-threads 1)) ;; For each thread, except last
          (push (cons (+ 1 n)
                      (subseq batch-data thread-start thread-end))
                list-of-thread-batches)
          (incf thread-start entries-per-thread)
          (incf thread-end entries-per-thread))
        ;; For last thread
        (push (cons number-of-threads
                    (subseq batch-data thread-start (+ thread-start entries-per-thread entries-last-thread)))
              list-of-thread-batches)))
       
    ;; process each thread-batch
    (let ((thread-list (loop repeat number-of-threads
                             collect (ccl:make-process "json-processing")))
          (mailbox-messages nil))
      ;; preset the threads
      (loop for thread in thread-list
            for thread-batch in list-of-thread-batches
            do (ccl:process-preset thread #'process-list-of-json-objects
                                   function function-kwargs thread-batch))
      ;; enable the process
      (loop for thread in thread-list
            do (ccl:process-enable thread))
      ;; Wait for threads to finish
      (ccl:process-wait "waiting for threads to finish..." 'all-threads-dead thread-list)
      ;; Join the threads
      (loop for thread in thread-list
            do (push (ccl:join-process thread) mailbox-messages))
      ;; Filter out empty messages
      (setf mailbox-messages
            (remove nil mailbox-messages))
      ;; Collect batch data and append to outputfile
      (let ((batch-results
             (loop for message in (sort mailbox-messages #'< :key #'car)
                   for message-content = (cdr message)
                   collect message-content)))
        (append-to-output
         (funcall process-batch-fn batch-results)
         outputfile
         start)))))