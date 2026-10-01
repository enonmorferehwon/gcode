#! /usr/bin/env -S sbcl --script ;; if needed, change the shebang

(defun read-edges-from-file (filename)
"Reads a list of edges from a file and returns it as a list of lists"
  (with-open-file (stream filename :direction :input)
    (read stream)))

(defun remove-edge-and-symmetric (edges edge)
   "Removes an edge and its reverse edge, if present, from the edge list" 
  (let* ((reversed-edge (reverse edge)))
    (remove-if (lambda (e)
                 (or (equal e edge)
                     (equal e reversed-edge)))
               edges)))

(defun generate-edge-sets (edges k)
   "Generates all possible edge lists by removing k edges and their reverse edges"
  (if (= k 0)
      (list edges)
      (let ((result '()))
        (dolist (edge edges)
          (let ((remaining (remove-edge-and-symmetric edges edge)))
            (dolist (subset (generate-edge-sets remaining (1- k)))
              (push subset result))))
        (remove-duplicates result :test #'equal))))

(defun write-edge-sets-to-file (filename edge-sets)
  "Writes the edge lists to the output file, one list per line"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dolist (edges edge-sets)
      (format stream "~a~%" edges))))

(defun remove-duplicate-lines-from-file (filename)
  "Removes duplicate lines from a file and returns their number"
  (let ((lines (with-open-file (stream filename :direction :input)
                 (loop for line = (read stream nil)
                       while line
                       collect line))))
    (let ((unique-lines (remove-duplicates lines :test #'equal)))
      (with-open-file (stream filename :direction :output :if-exists :supersede)
        (dolist (line unique-lines)
          (format stream "~a~%~%" line)))
      (length unique-lines))))

(defun main ()
  "Driver"
  (let* ((args (rest sb-ext:*posix-argv*)) ;; Number (k) of edges to be removed in order to find spanning-trees 
         (k (if args
                (parse-integer (first args) :junk-allowed t)
                2)) ;; Hardcoded default (without k indications) => 2 edges removed
         (input-file "graphD_in")
         (output-file "sink_out-1")
         (directed-edges (read-edges-from-file input-file))
         (edge-sets (generate-edge-sets directed-edges k)))
    ;; Writes the m edge lists to the output file
    (write-edge-sets-to-file output-file edge-sets)
    ;; Removes duplicate lines from the output file and returns the number of isolated sequences
    (let ((num-sequenze (remove-duplicate-lines-from-file output-file)))
      (format t "Processing completed and duplicate lines removed~%")
      (format t "Number of edges removed from each sequence (k): ~d~%" k)
      (format t "Total number of isolated sequences: ~d~%" num-sequenze))))

(main)

