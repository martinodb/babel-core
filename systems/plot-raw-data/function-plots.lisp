(in-package :plot-raw-data)

(export '(create-function-plot))

(defun create-function-plot (equations
                             &key
                             (function-definitions nil)
                             (title nil) (captions nil)
                             (plot-file-name nil)
                             (plot-directory nil)
                             (graphic-type "pdf")
                             (key-location "below")
                             (x-min 0) (x-max 1)
                             (y-min -1) (y-max 1)
                             (colors *great-gnuplot-colors*)
                             (draw-y1-grid t)
                             (x-label "Number of games played")
                             (y1-label nil)
                             (grid-line-width 0.5)
                             (open t) (dashed nil)
                             (fsize 10)
                             (typeface "Helvetica"))
  "Plot several functions using gnuplot. This function expects a list
   of function definitions (in a list of strings) specified with x as the variable
   and in a format that can be handled by gnuplot (e.g. f(x)=4*x instead of f(x)=4x).
   See examples in the comments below."
  (let ((colors (loop for color in colors
                      for other-color in *great-gnuplot-colors*
                      collect (or color other-color)))
        (captions (loop for f in equations
                        for i from 0
                        collect (or (nth i captions) f)))
        (file-path (babel-pathname :name (or plot-file-name "function-plot")
                                   :type (if (equal graphic-type "postscript") "ps" graphic-type)
                                   :directory (or plot-directory '(".tmp")))))
    (ensure-directories-exist file-path)
    (with-open-stream
      (stream (monitors::pipe-to-gnuplot))
      (set-gnuplot-parameters stream
                              :output file-path :terminal graphic-type :title (or title "")
                              :draw-y1-grid draw-y1-grid :grid-line-width grid-line-width
                              :key-location key-location :x-label x-label :y1-label y1-label
                              :y2-label nil :y1-min y-min :y1-max y-max
                              :y2-min nil :y2-max nil
                              :dashed dashed :fsize fsize :typeface typeface)
      (set-range stream "x" x-min x-max)
      (format stream "~cset grid back noxtics" #\linefeed)
    
      (format stream "~cset ytics nomirror" #\linefeed)
      (format stream "~cset style fill transparent solid 0.20 border" #\linefeed)

      (when function-definitions
        (loop for def in function-definitions
              do (format stream "~c~a" #\linefeed def)))
      
      (loop for f in equations
            do (format stream "~c~a" #\linefeed f))

      (format stream "~cplot " #\linefeed)
      (loop for f in equations
            for source-number from 0
            for function-name = (when (find #\= f)
                                  (remove-spurious-spaces (first (split f #\=))))
            for color = (nth (mod source-number (length colors)) colors)
            do (format stream "~a title ~s dt ~a lc rgb ~s ~:[~;, ~]"
                       (or function-name f)
                       (nth source-number captions) ;; caption
                       (+ 2 (mod source-number 8)) ;; dash type
                       color ;; color
                       (< source-number (- (length equations) 1)) ;; adding , to the end or not
                       ))

      (format stream "~cexit~c"  #\linefeed #\linefeed)
      (finish-output stream)
      ;(close-pipe stream)
      (when open
        (sleep 0.5)
        (open-file-in-os file-path)))))

;; creating two simple functions
;(create-function-plot '("f1(x)=2*x" "f2(x)=3*x"))

;; creating two functions using an additional function definition
;(create-function-plot '("normal(x,0,1)") :function-definitions '("normal(x, mu, sd) = (1/(sd*sqrt(2*pi)))*exp(-(x-mu)**2/(2*sd**2))") :x-min -3 :x-max 3 :y-min 0 :y-max nil)
;(create-function-plot '("f(x)=normal(x,0,1)") :function-definitions '("normal(x, mu, sd) = (1/(sd*sqrt(2*pi)))*exp(-(x-mu)**2/(2*sd**2))") :x-min -3 :x-max 3 :y-min 0 :y-max nil)