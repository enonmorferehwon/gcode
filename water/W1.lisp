#!sbcl --script

(defun read-edges-from-file (filename)
  "Legge una lista di archi da un file e la restituisce come una lista di liste."
  (with-open-file (stream filename :direction :input)
    (read stream)))

(defun remove-edge-and-symmetric (edges edge)
  "Rimuove un arco e il suo simmetrico, se presente, dalla lista degli archi."
  (let* ((reversed-edge (reverse edge)))
    (remove-if (lambda (e)
                 (or (equal e edge)
                     (equal e reversed-edge)))
               edges)))

(defun generate-edge-sets (edges)
  "Genera tutte le possibili liste di archi rimuovendo ogni arco e i suoi simmetrici iterativamente."
  (let ((result '()))
    (dolist (initial-edge edges)
      (let ((remaining-edges (remove-edge-and-symmetric edges initial-edge)))
        (dolist (edge remaining-edges)
          (let* ((new-edges (remove-edge-and-symmetric remaining-edges edge)))
            (push new-edges result)))))
    (reverse result)))

(defun write-edge-sets-to-file (filename edge-sets)
  "Scrive le liste di archi nel file di output, una lista per linea."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (dolist (edges edge-sets)
      (format stream "~a~%" edges))))

(defun remove-duplicate-lines-from-file (filename)
  "Rimuove le righe duplicate da un file e restituisce il numero di righe uniche."
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
  "Funzione principale."
  (let* ((input-file "graphD_in")
         (output-file "sink_out-1")
         (directed-edges (read-edges-from-file input-file))
         (edge-sets (generate-edge-sets directed-edges)))
    ;; Scrivi le m liste di archi nel file di output
    (write-edge-sets-to-file output-file edge-sets)
    ;; Rimuovi le righe duplicate dal file di output e ottieni il numero di sequenze isolate
    (let ((num-sequenze (remove-duplicate-lines-from-file output-file)))
      (format t "Processo completato e righe duplicate rimosse.~%")
      (format t "Numero totale di sequenze isolate: ~d~%" num-sequenze))))

(main)
