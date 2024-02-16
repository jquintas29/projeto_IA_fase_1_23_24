;;;Ficheiro puzzle.lisp
;;Ficheiro com operadores e heur�sticas espec�ficos do dom�nio da aplica��o

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

;; TODO Aplicar a regra do duplo, caso calhe numa casa de n�mero duplo (exemplo: 99) 
;;retirar do tabuleiro outro duplo � escolha do jogador (por default ser� o de maior valor).

;;; M�todos seletores
(defun linha (indice tabuleiro)
"Fun��o que recebe um �ndice e o tabuleiro e retorna uma lista que representa essa linha do
tabuleiro."
    (cond ((and (integerp indice) (>= indice 0) (< indice (length tabuleiro))) 
            (nth indice tabuleiro))
          (t (error "Indice inv�lido"))
    )
)

(defun celula (indiceLinha indiceColuna tabuleiro)
"Fun��o que recebe dois �ndices e o tabuleiro e retorna o valor presente nessa c�lula do
tabuleiro."
    (cond ((and (integerp indiceColuna) (>= indiceColuna 0) (< indiceColuna (length (linha indiceLinha tabuleiro)))) 
            (nth indiceColuna (linha indiceLinha tabuleiro)))
          (t (error "Indice inv�lido"))
    )
)

(defun alisa (lista)
"Fun��o que devolve todos os elementos de uma lista que poder� conter sub-listas, 
com todos os elementos agregados numa �nica lista principal. "
    (cond ((null lista) nil)
          ((atom (car lista)) (cons (car lista) (alisa (cdr lista))))
          (t (append (alisa (car lista)) (alisa (cdr lista))))
    )
)

(defun lista-numeros (&optional (n 100))
"Fun��o que recebe um n�mero positivo n e cria uma lista com todos os n�meros
entre 0 (inclusiv�) e o n�mero passado como argumento (exclusiv�). Por default o n � 100"
    (cond ((= n 0) nil)
          (t (cons (- n 1) (lista-numeros (- n 1)))))
)

(defun remover-se(pred lista)
"Devolve a resconstru��o da lista passada como argumento sem os elementos que verificam a
condi��o/predicado passado pelo argumento pred 
exemplo: (remover-se #'(lambda (x) (= x 0)) lista) -> devolve a lista sem os �tomos 0"
    (cond ((null lista) NIL) 
          ((funcall pred (car lista)) (remover-se pred (cdr lista)))
          (T (cons (car lista) (remover-se pred (cdr lista))))
    )
)

(defun baralhar (lista)
"Recebe uma e ir� mudar aleatoriamente os n�meros de forma a criar uma lista baralhada."
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
"Recebe uma lista (por default ser� chamada a fun��o baralhar com argumento a fun��o lista-numeros)
e vai devolver essa lista dividida em sublistas de n elementos recebido como par�metro pelo argumento n
(por default ter� o valor 10)"
    (cond ((null lista) nil)
          (t (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))
    )
)

(defun substituir-posicao (indice lista &optional (valor nil))
"Fun��o que recebe um �ndice, uma lista e um valor (por default o valor � NIL) e
substitui pelo valor pretendido nessa posi��o."
    (cond ((and (integerp indice) (>= indice 0) (< indice (length lista))) 
                (let ((nova-lista lista))
                        (setf (nth indice nova-lista) valor)
                    nova-lista))
          (t (error "Indice inv�lido"))
    )
)

(defun substituir (indice-linha indice-coluna tabuleiro &optional (valor 'nil))
"Fun��o que recebe dois �ndices, o tabuleiro e um valor (por default o valor � NIL). A
fun��o retorna o tabuleiro com a c�lula substitu�da pelo valor pretendido."
    (let ((novo-tabuleiro tabuleiro) (numero (nth indice-coluna (nth indice-linha tabuleiro))))
        (substituir-posicao indice-coluna (linha indice-linha novo-tabuleiro) valor)
        (cond ((not (eql valor 'nil)) (substituir-simetrico numero tabuleiro))
        (novo-tabuleiro)
    )    
)
)

(defun substituir-simetrico (numero tabuleiro &optional (valor-linha 0))
"Fun��o que recebe um n�mero, um tabuleiro. A fun��o vai retornar o tabuleiro 
com o sim�trico do n�mero da vari�vel numero substituido por NIL"
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
"Fun��o que recebe um n�mero e devolve o sim�trico desse n�mero (exemplo: 56->65)"
  (parse-integer (coerce (reverse (coerce (princ-to-string numero) 'list)) 'string))
)

(defun posicao-cavalo (tabuleiro &optional (valor-linha 0))
"Fun��o que recebe o tabuleiro e devolve a posi��o (i j) em que se encontra o
cavalo. Caso o cavalo n�o se encontre no tabuleiro retorna NIL."
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
;verificar primeiro se a a��o do operador ser� feita dentro dos limites do tabuleiro


;;operador-2


;;operador-3


;;operador-4


;;operador-5


;;operador-6


;;operador-7


;;operador-8