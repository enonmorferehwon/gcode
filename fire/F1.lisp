#! sbcl --script

(defun read-graphs-from-file (file-path)
  "Legge una lista di grafi (ogni grafo è una lista di tuple) da un file."
  (with-open-file (in file-path :direction :input)
    (loop for graph = (read in nil)
          while graph
          collect graph)))

(defun find-edge-weight (edge graph)
  "Trova il peso di un arco dato nel grafo."
  (let ((src (first edge))
        (dst (second edge)))
    (third (find-if (lambda (e) (and (equal (first e) src)
                                     (equal (second e) dst)))
                    graph))))

(defun calculate-spanning-tree-weight (spanning-tree graph)
  "Calcola la somma dei pesi per lo spanning tree dato."
  (reduce #'+ (mapcar (lambda (edge)
                        (find-edge-weight edge graph))
                      spanning-tree)))

(defun process-spanning-trees-from-file (file-path graph graph-index output-file)
  "Legge gli spanning trees da un file e calcola il peso per ciascuno. Scrive i risultati nel file."
  (with-open-file (in file-path :direction :input)
    (loop for line = (read in nil)
          while line
          for tree-index from 1
          do (let ((spanning-tree line)
                   (total-weight (calculate-spanning-tree-weight line graph)))
               (format output-file "Grafo ~d, Spanning Tree ~d:~%~a~%" graph-index tree-index spanning-tree)
               (format output-file "Il peso totale dello Spanning Tree ~d è: ~a~%~%" tree-index total-weight)))))

(defun write-results-to-file (file-path)
  "Scrive i risultati dell'elaborazione su un file."
  (with-open-file (out file-path :direction :output :if-exists :supersede)
    ;; Lettura dei grafi dal file T
    (let ((graphs (read-graphs-from-file "T298")))
      (loop for graph in graphs
            for graph-index from 1
            do (format out "Processando il Grafo ~d: ~a~%" graph-index graph)
               ;; Processa il grafo con gli spanning trees dal file W2
               (process-spanning-trees-from-file "W2_0" graph graph-index out)))))

(defun write-results-to-files (input-files output-files)
  "Scrive i risultati dell'elaborazione su una sequenza di file."
  ;; Lettura dei grafi dal file T
  (let ((graphs (read-graphs-from-file "T298")))
    (loop for input-file in input-files
          for output-file in output-files
          do (with-open-file (out output-file :direction :output :if-exists :supersede)
               (loop for graph in graphs
                     for graph-index from 1
                     do (format out "Processando il Grafo ~d: ~a~%" graph-index graph)
                        ;; Processa il grafo con gli spanning trees dal file corrente
                        (process-spanning-trees-from-file input-file graph graph-index out))))))

;; Specifica i file di input e output
(write-results-to-files '("W2_0" "W2_1" "W2_2" "W2_3" "W2_4" "W2_5" "W2_6" "W2_7" "W2_8" "W2_9"
			  "W2_10" "W2_11" "W2_12" "C2" "C3") ;; input
                        '("END_0" "END_1" "END_2" "END_3" "END_4" "END_5" "END_6" "END_7"
			  "END_8" "END_9" "END_10" "END_11"  "END_12"  "END_C2"  "END_C3") ;; output
			)
