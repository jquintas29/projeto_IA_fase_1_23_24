;;; Ficheiro procura.lisp
;; Implementa��o dos algoritmos de procura BFS, DFS, A*

; Escrever depois num ficheiro a solu��o encontrada, n�mero de n�s gerados 
; n�mero de n�s expandidos, penetr�ncia, fator de ramifica��o m�dia e tempo de execu��o.

(defun abertos-bfs (lista-abertos lista-sucessores)
    (cond 
        ((null lista-sucessores) lista-abertos)
        ((no-existep (car lista-sucessores) lista-abertos) (abertos-bfs lista-abertos (cdr lista-sucessores)))
        (t (abertos-bfs (append lista-abertos (cons (car lista-sucessores) nil)) (cdr lista-sucessores)))
    )
)

(defun abertos-dfs (lista-abertos lista-sucessores)
    (cons (car lista-sucessores) (append (cdr lista-sucessores) lista-abertos))
)

(defun no-existep (no lista)
    (cond 
        ((null lista) nil)
        ((equal (no-operadores no) (no-operadores (car lista))) T)
        (t (no-existep no (cdr lista)))
    )
)

;;; Funcoes auxiliares da procura
(defun no-solucaop (no)
    (cond 
        ((>= (no-pontuacao no) (no-solucao no)) T)
        (t nil)
    )
)

;;; Algoritmos
;; procura na largura (bfs)
(defun bfs (no funcao-solucao funcao-sucessores lista-operadores &optional (abertos '()) (fechados '()))
    (cond 
        ((not (funcall funcao-solucao (car (abertos-bfs abertos (funcall funcao-sucessores no lista-operadores 'bfs))))) 
            (bfs 
                (car (abertos-bfs abertos (funcall funcao-sucessores no lista-operadores 'bfs))) 
                funcao-solucao 
                funcao-sucessores 
                lista-operadores 
                (cdr (abertos-bfs abertos (funcall funcao-sucessores no lista-operadores 'bfs))) 
                (append fechados (car (abertos-bfs abertos (funcall funcao-sucessores no lista-operadores 'bfs))))
            ))
        (t (car (abertos-bfs abertos (funcall funcao-sucessores no lista-operadores 'bfs))))
    ) 
)

;; procura na profundidade (dfs)
(defun dfs (no funcao-solucao funcao-sucessores lista-operadores profundidade &optional (abertos '()) (fechados '()))
    (cond 
        ((> (no-profundidade (car (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs)))) profundidade)
         (dfs 
            (car (cdr (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs))))
            funcao-solucao 
            funcao-sucessores 
            lista-operadores 
            profundidade 
            (cdr (cdr (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs)))) 
            fechados
         ))
        ((not (funcall funcao-solucao (car (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs)))))
         (dfs 
            (car (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs))) 
            funcao-solucao 
            funcao-sucessores 
            lista-operadores 
            profundidade 
            (cdr (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs))) 
            (append fechados (cons (car (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs))) nil))
         ))
        (t (car (abertos-dfs abertos (funcall funcao-sucessores no lista-operadores 'dfs))))
    )  
)