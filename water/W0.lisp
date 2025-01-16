#! sbcl --script

(defun filtra-grafi-branch-ammissibili (grafi)
  "Filtra i grafi che non contengono archi con lo stesso nodo in una posizione."
  (remove-if (lambda (grafo)
               (or (has-duplicate-nodes-in-position grafo 0) ; Duplica nel primo nodo
                   (has-duplicate-nodes-in-position grafo 1))) ; Duplica nel secondo nodo
             grafi))

(defun has-duplicate-nodes-in-position (grafo pos)
  "Verifica se ci sono nodi duplicati nella posizione specificata (0 o 1)."
  (let ((nodes (mapcar (lambda (arco) (nth pos arco)) grafo)))
    (/= (length nodes) (length (remove-duplicates nodes)))))

(defun genera-grafi-diretti (grafo)
  "Genera tutti i possibili grafi diretti rimuovendo archi simmetrici."
  (let* ((archi-simmetrici
          (remove-if-not (lambda (arco)
                           (find (reverse arco) grafo :test #'equal))
                         grafo))
         ;; Rimuove duplicati per evitare di contare due volte la stessa coppia
         (coppie-simmetriche
          (remove-duplicates
           (mapcar (lambda (arco) (list arco (reverse arco)))
                   archi-simmetrici)
           :test (lambda (a b)
                   (or (equal a b)
                       (equal a (reverse b))))))
         ;; Genera tutte le combinazioni di grafi diretti
         (combinazioni
          (reduce (lambda (acc coppia)
                    (let ((arco1 (first coppia))
                          (arco2 (second coppia)))
                      (loop for g in acc
                            append (list (remove arco2 g :test #'equal)
                                         (remove arco1 g :test #'equal)))))
                  coppie-simmetriche
                  :initial-value (list grafo))))
    ;; Rimuove duplicati e restituisce il risultato singolo
    (remove-duplicates combinazioni :test #'equal)))

(defun rimuovi-e-costruisci (grafo-modificato n1 n2)
  "Rimuove archi che iniziano con n1 o n2 e genera grafi con archi diretti da archi simmetrici."
  (let* ((n1 (first n1)) ; Estrae il valore dal nodo
         (n2 (first n2)) ; Estrae il valore dal nodo
         ;; Rimuove archi che iniziano con n1 o n2
         (grafo-senza-n1-n2 (remove-if (lambda (arco)
                                         (or (equal (first arco) n1)
                                             (equal (first arco) n2)))
                                       grafo-modificato))
         ;; Genera grafi diretti
         (grafi-diretti (genera-grafi-diretti grafo-senza-n1-n2)))
    ;; Stampa i grafi diretti generati
    (format t "Grafi diretti generati:~%")
    (dolist (g grafi-diretti)
      (format t "  ~a~%" g))
    ;; Filtra i grafi-branch ammissibili
    (let ((grafi-branch-ammissibili (filtra-grafi-branch-ammissibili grafi-diretti)))
      (if (null grafi-branch-ammissibili)
          (progn
            (format t "Nessun grafo-branch ammissibile trovato.~%")nil)
          (progn
            (format t "Grafi-branch ammissibili:~%")
            (dolist (g grafi-branch-ammissibili)
              (format t "  ~a~%" g)))))))

(defun rimuovi-arco (grafo arco)
  "Rimuove un arco dal grafo."
  (remove arco grafo :test #'equal))

(defun rimuovi-coppia-archi (grafo arco)
  "Rimuove un arco e il suo simmetrico dal grafo."
  (let ((simmetrico (reverse arco)))
    (remove simmetrico (rimuovi-arco grafo arco) :test #'equal)))

(defun genera-grafi (grafo)
  "Genera tutti i grafi rimuovendo uno alla volta un arco o una coppia di archi simmetrici,
   evitando duplicati per coppie simmetriche."
  (let ((grafi '())
        (visitati '())) ; Per tenere traccia degli archi già considerati
    (dolist (arco grafo)
      (unless (member arco visitati :test #'equal)
        ;; Aggiungi l'arco e il suo simmetrico alla lista dei visitati
        (push arco visitati)
        (push (reverse arco) visitati)
        ;; Rimuovi l'arco e il suo simmetrico (se presente)
        (if (member (reverse arco) grafo :test #'equal)
            (push (list (rimuovi-coppia-archi grafo arco) arco) grafi)
            ;; Rimuovi solo l'arco
            (push (list (rimuovi-arco grafo arco) arco) grafi))))
    (remove-duplicates grafi :test #'equal)))

(defun main ()
  "Legge il grafo, i nodi estremi e genera i grafi modificati."
  (let* ((grafo 
	  '((5 8) (8 5) (8 9) (9 8) (9 10) (10 9) (10 11) (11 12) (12 1))
         ;; '((5 6) (6 5) (6 7) (7 1))
	   )
         (nodo1 '(5)) ; Nodo estremo 1
         (nodo2 '(1))) ; Nodo estremo 2
    ;; Stampa il grafo originale e i nodi estremi
    (format t "~%Grafo originale: ~a~%" grafo)
    ;; Conta e rappresenta il numero dei nodi
    (let* ((archi (copy-tree grafo)))          ;; Copia locale di grafo
      ;; Genera e stampa i grafi modificati
      (let* ((grafi-modificati (genera-grafi grafo)))
        (dolist (g grafi-modificati)
          ;; Estrai i componenti del grafo modificato
          (let* ((grafo-modificato (first g))
                 (n1 nodo1)
                 (n2 nodo2))
            ;; Stampa informazioni sul grafo corrente
	    (format t "~%Arco estratto --> ~a ~%Grafo corrispondente:" (second g))
            (format t "~%~a~%" grafo-modificato)
            ;; Chiamata alla funzione rimuovi-e-costruisci
            (let ((grafi-derivati (rimuovi-e-costruisci grafo-modificato n1 n2)))
              ;; Stampa i grafi derivati
              (dolist (g-derivato grafi-derivati)
                (format t "  ~a~%" g-derivato)))))))))


(main)
