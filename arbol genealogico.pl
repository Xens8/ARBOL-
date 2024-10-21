/* Árbol Genealógico */

mensaje :- nl,write('Ejemplo "Árbol Genealógico" cargado.'),nl,nl.

/*---- PROGRAMA PRINCIPAL ----*/

/*------ Hechos ------*/

/* padres(H,P,M,A) <- H tiene como padre a P y como madre a M, y nació el año A */
padres('rene mandujano',p1,m1,1930).
padres('lilia espinoza',p2,m2,1930).
padres('lolis mandujano','rene mandujano','lilia espinoza',1950).
padres('lourdes mandujano','rene mandujano','lilia espinoza',1949).
padres('don patricio',p3,m3,1954).
padres('aminta amndujano','rene mandujano','lilia espinoza',1951).
padres('hernan lopez',p4,m4,1955).
padres('patricia lopez','don patricio','lolis mandujano',1998).
padres('rene lopez','don patricio','lolis mandujano',1995).
padres('daniela rammirez',p5,m5,1984).
padres('fara lopez','hernan lopez','lourdes mandujano',1982).
padres('amaury lopez','hernan lopez','lourdes mandujano',1986).
padres('Marti Lopez','fara lopez','daniela rammirez',2003).
/* casados(H,M) <- El hombre H está casado con la mujer M */
casados('rene mandujano','lilia espinoza').
casados('don patricio','lolis mandujano').
casados('hernan lopez','lourdes mandujano').
casados('fara lopez','daniela rammirez').
/* hombre(P) <- la persona P es del género masculino */
hombre('rene mandujano').
hombre('don patricio').
hombre('hernan lopez').
hombre('rene lopez').
hombre('amaury lopez').
hombre('fara lopez').
hombre('Marti Lopez').
/* mujer(P) <- la persona P es del género femenino */
mujer('lilia espinoza').
mujer('lolis mandujano').
mujer('lourdes mandujano').
mujer('aminta amndujano').
mujer('daniela rammirez').
mujer('patricia lopez').

/*------ Reglas ------*/
/* edad(P,E) <- la persona P tiene E años */
edad(P,E) :- padres(P,_,_,A), E is 2024-A.

/* mayor(P1,P2) <- la persona P1 es mayor que P2 */
mayor(P1,P2) :- padres(P1,_,_,A1), padres(P2,_,_,A2), A1<A2.

/* niño(P1) <- P1 es un niño (menos de 14 años) */
ninyo(P) :- edad(P,E), E=<14.

/* joven(P1) <- P1 es una persona joven (entre 14 y 25 años) */
joven(P) :- edad(P,E), 14<E,E=<25. 

/* adulto(P1) <- P1 es un adulto (entre 25 y 50 años) */
adulto(P) :- edad(P,E), 25<E,E=<50.

/* viejo(P1) <- P1 es una persona vieja (más de 50 años) */
viejo(P) :- edad(P,E), E>50.

/* hermanos(H1,H2) <- H1 es hermano/a de H2 */
hermanos(H1,H2) :- padres(H1,P,M,_), padres(H2,P,M,_), H1\=H2.

/* tio(T,S) <- T es el tio de S */
tio(T,S) :- hombre(T),
            padres(S,P,_,_),
	    hermanos(T,P).
tio(T,S) :- hombre(T),
	    padres(S,_,M,_),
	    hermanos(T,M).
tio(T,S) :- hombre(T),
 	    padres(S,P,_,_),
 	    hermanos(T1,P),
 	    casados(T,T1).
tio(T,S) :- hombre(T),
	    padres(S,_,M,_),
	    hermanos(T1,M),
	    casados(T,T1). 

/* tia(T,S) <- T es la tia de S */
tia(T,S) :- mujer(T),
	    padres(S,P,_,_),
	    hermanos(T,P).
tia(T,S) :- mujer(T),
	    padres(S,_,M,_),
	    hermanos(T,M).
tia(T,S) :- mujer(T),
	    padres(S,P,_,_),
	    hermanos(T1,P),
	    casados(T1,T).
tia(T,S) :- mujer(T),
	    padres(S,_,M,_),
	    hermanos(T1,M),
   	    casados(T1,T).

/* primos(P1,P2) <- P1 es primo/a de P2 */
primos(P1,P2) :- padres(P1,PA1,MA1,_),
		 padres(P2,PA2,MA2,_),
	         (hermanos(PA1,PA2);
		  hermanos(PA1,MA2);
		  hermanos(MA1,PA2);
		  hermanos(MA1,MA2)).

/* abuelo(A,N) <- A es el abuelo de N */
abuelo(A,N) :- padres(N,P,M,_),
	       (padres(P,A,_,_);
	        padres(M,A,_,_)).

/* abuela(A,N) <- A es la abuela de N */
abuela(A,N) :- padres(N,P,M,_),
 	       (padres(P,_,A,_);
	        padres(M,_,A,_)).
/* antepasado(A,P) <- A es antepasado de P */
antepasado(A,P) :- padres(P,A,_,_).
antepasado(A,P) :- padres(P,_,A,_).
antepasado(A,P) :- padres(P,PA,_,_), antepasado(A,PA).
antepasado(A,P) :- padres(P,_,MA,_), antepasado(A,MA).
:- mensaje.     
