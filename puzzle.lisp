;;;Ficheiro puzzle.lisp
;;Ficheiro com operadores e heurísticas específicos do domínio da aplicação

(defun tabuleiro-a ()
"Tabuleiro para o problema A"
  '(
    (02 20 44 nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil 03 30 nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil 22 nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-b ()
"Tabuleiro para o problema B"
  '(
    (02 nil 04 nil 06 nil 08 nil 10 nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil 03 nil 05 nil 07 nil 09 nil 11) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-c ()
"Tabuleiro para o problema c"
  '(
    (01 12 03 23 nil 88 nil nil nil nil) 
    (21 45 43 nil nil nil nil nil nil nil) 
    (nil 56 nil 78 nil nil nil nil nil nil) 
    (89 nil 99 54 nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil nil nil nil nil nil nil)
    )
)

(defun tabuleiro-d ()
"Tabuleiro para o problema d"
  '(
    (98 97 96 95 94 93 92 91 90 89) 
    (01 02 03 04 05 55 06 07 08 09) 
    (nil 66 nil nil nil nil nil nil nil 11) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil 22 nil nil nil nil nil 33 nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil 88 nil nil nil 44 nil nil) 
    (nil nil nil nil nil nil nil nil nil nil) 
    (nil nil nil nil 77 nil nil nil nil nil) 
    (nil nil nil nil nil nil 99 nil nil nil)
    )
)

(defun tabuleiro-e ()
"Tabuleiro para o problema e"
  '(
    (nil 05 nil nil nil 15 nil nil nil 25) 
    (nil nil nil 06 nil nil nil 16 nil nil) 
    (nil 04 nil nil nil 14 nil nil nil 24) 
    (nil nil nil 07 nil nil nil 17 nil nil) 
    (nil 03 nil nil nil 13 nil nil nil 23) 
    (nil nil nil 08 nil nil nil 18 nil nil) 
    (nil 02 nil nil nil 12 nil nil nil 22) 
    (nil nil nil 09 nil nil nil 19 nil nil) 
    (nil 01 nil nil nil 11 nil nil nil 21) 
    (nil nil nil 10 nil nil nil 20 nil nil)
    )
)

;; TODO Aplicar a regra do duplo, caso calhe numa casa de número duplo (exemplo: 99) 
;;retirar do tabuleiro outro duplo à escolha do jogador (por default será o de maior valor).

;;; Métodos seletores
(defun linha (indice tabuleiro)
"Função que recebe um índice e o tabuleiro e retorna uma lista que representa essa linha do
tabuleiro."
    (cond ((and (integerp indice) (>= indice 0) (< indice (length tabuleiro))) 
            (nth indice tabuleiro))
          (t (error "Indice inválido"))
    )
)

(defun celula (indiceLinha indiceColuna tabuleiro)
"Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do
tabuleiro."
    (cond ((and (integerp indiceColuna) (>= indiceColuna 0) (< indiceColuna (length (linha indiceLinha tabuleiro)))) 
            (nth indiceColuna (linha indiceLinha tabuleiro)))
          (t (error "Indice inválido"))
    )
)

(defun alisa (lista)
"Função que devolve todos os elementos de uma lista que poderá conter sub-listas, 
com todos os elementos agregados numa única lista principal. "
    (cond ((null lista) nil)
          ((atom (car lista)) (cons (car lista) (alisa (cdr lista))))
          (t (append (alisa (car lista)) (alisa (cdr lista))))
    )
)

(defun lista-numeros (&optional (n 100))
"Função que recebe um número positivo n e cria uma lista com todos os números
entre 0 (inclusivé) e o número passado como argumento (exclusivé). Por default o n é 100"
    (cond ((= n 0) nil)
          (t (cons (- n 1) (lista-numeros (- n 1)))))
)

(defun remover-se(pred lista)
"Devolve a resconstrução da lista passada como argumento sem os elementos que verificam a
condição/predicado passado pelo argumento pred 
exemplo: (remover-se #'(lambda (x) (= x 0)) lista) -> devolve a lista sem os átomos 0"
    (cond ((null lista) NIL) 
          ((funcall pred (car lista)) (remover-se pred (cdr lista)))
          (T (cons (car lista) (remover-se pred (cdr lista))))
    )
)

(defun baralhar (lista)
"Recebe uma e irá mudar aleatoriamente os números de forma a criar uma lista baralhada."
    (cond ((null lista) nil)
        (t (let* ((index (random (length lista)))
                  (numberRandom (nth index lista))
                  (nova-lista (remover-se #'(lambda (x) (= x numberRandom)) lista)))
                (cons numberRandom (baralhar nova-lista))
            )
        )
    )
)

(defun tabuleiro-aleatorio (&optional (lista (baralhar (lista-numeros))) (n 10))
"Recebe uma lista (por default será chamada a função baralhar com argumento a função lista-numeros)
e vai devolver essa lista dividida em sublistas de n elementos recebido como parâmetro pelo argumento n
(por default terá o valor 10)"
    (cond ((null lista) nil)
          (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir-posicao (indice lista &optional (valor nil))
"Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e
substitui pelo valor pretendido nessa posição."
    (cond ((and (integerp indice) (>= indice 0) (< indice (length lista))) 
                (let ((nova-lista lista))
                        (setf (nth indice nova-lista) valor)
                    nova-lista))
          (t (error "Indice inválido"))
    )
)

(defun substituir (indice-linha indice-coluna tabuleiro &optional (valor 'nil))
"Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL). A
função retorna o tabuleiro com a célula substituída pelo valor pretendido."
    (let ((novo-tabuleiro tabuleiro) (numero (nth indice-coluna (nth indice-linha tabuleiro))))
        (substituir-posicao indice-coluna (linha indice-linha novo-tabuleiro) valor)
        (cond ((not (eql valor 'nil)) (substituir-simetrico numero tabuleiro))
        (novo-tabuleiro)
    )    
)
)

(defun substituir-simetrico (numero tabuleiro &optional (valor-linha 0))
"Função que recebe um número, um tabuleiro. A função vai retornar o tabuleiro 
com o simétrico do número da variável numero substituido por NIL"
    (cond 
        ((>= valor-linha (length tabuleiro)) nil)
        ((let ((lista-linha (linha valor-linha tabuleiro)) (simetrico (obter-simetrico numero)))
            (cond 
                ((find simetrico lista-linha) (substituir valor-linha (position simetrico lista-linha) tabuleiro))
                (t (substituir-simetrico numero tabuleiro (+ valor-linha 1)))
            )
        ))
    )
)

(defun obter-simetrico (numero)
"Função que recebe um número e devolve o simétrico desse número (exemplo: 56->65)"
  (parse-integer (coerce (reverse (coerce (princ-to-string numero) 'list)) 'string))
)

(defun posicao-cavalo (tabuleiro &optional (valor-linha 0))
"Função que recebe o tabuleiro e devolve a posição (i j) em que se encontra o
cavalo. Caso o cavalo não se encontre no tabuleiro retorna NIL."
    (cond 
        ((>= valor-linha (length tabuleiro)) nil)
        ((let ((lista-linha (linha valor-linha tabuleiro)))
            (cond 
                ((find T lista-linha) (cons valor-linha (cons (position T lista-linha) nil)))
                (t (posicao-cavalo tabuleiro (+ valor-linha 1)))
            )
        ))
    )
)

;;; Operadores do problema (ver lab7)
;;operador-1
;verificar primeiro se a ação do operador será feita dentro dos limites do tabuleiro


;;operador-2


;;operador-3


;;operador-4


;;operador-5


;;operador-6


;;operador-7


;;operador-8