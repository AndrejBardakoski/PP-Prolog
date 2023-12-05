%ZAD - 1 neparen palindrom
%neparen_palindrom(L) true ako L ima neparen broj na elementi i e palindrom
neparen_palindrom([_]).
neparen_palindrom([Glava|Ostatok]):-
    izvadi_posleden(Ostatok,Posleden,ListOut),
    Glava = Posleden,
    neparen_palindrom(ListOut),!.

%izvadi_posleden(L,Posleden,Rest) Poosleden e posledniot element vo listata L a Rest e ostatokot od L
izvadi_posleden([X],X,[]).
izvadi_posleden([Glava|Ostatok],Posleden,ListOut):-
    izvadi_posleden(Ostatok,Posleden,ListOutRec),
    ListOut=[Glava|ListOutRec].


%ZAD - 2
%nаj_podniza(L,N,X).  X e podlista so dolzina N koja sto se pojavuva najmnogu pati vo L
nаj_podniza(InputL,Dolzina,Maks):-
	findall(L1,(podniza(InputL,L1),dolzina(L1,Dolzina)),L),
    naj_pojavuvana(L,Maks),!.

%podniza(L1,L2). L2 e podnizna na L1 
podniza([X|L1],[X|L2]):-podnizaPocetok(L1,L2).
podniza([_|L1],L2):-podniza(L1,L2).
%podnizaPocetok(L1,L2). L2 e podniza koja sto se naoga na pocetok na L1, 
podnizaPocetok(_,[]).
podnizaPocetok([X|L1],[X|L2]):-podnizaPocetok(L1,L2).

%naj_pojavuvana(L,Maks) Maks e eleemntot koj sto najmnogu se pojavuva vo L
naj_pojavuvana([X|L],Maks):-izbroiPojavuvana(X,[X|L],N),naj_pojavuvana(L,[X|L],X,N,Maks).
%naj_pojavuvana(L,L,tempX,tempN,X).  X e element koj sto se pojavuva najmnogu pati vo L.
% tempX i tempN se pomosnini promenlvi koj go cuvaat tekovno najdeniot element so najmnogu pojavuvana 
naj_pojavuvana([],_,MaksX,_,MaksX).
naj_pojavuvana([X|O],L,_,MaksN,Out):-
    izbroiPojavuvana(X,L,N),N>MaksN,
    naj_pojavuvana(O,L,X,N,Out).
naj_pojavuvana([X|O],L,MaksX,MaksN,Out):-
    izbroiPojavuvana(X,L,N),N=<MaksN, 
    naj_pojavuvana(O,L,MaksX,MaksN,Out).

%izbroiPojavuvana(X,L,N).  N e broj na pojavuvana na X vo listata L.
izbroiPojavuvana(_,[],0).
izbroiPojavuvana(X,[X|L],N):-izbroiPojavuvana(X,L,M), N is M+1.
izbroiPojavuvana(X,[_|L],N):-izbroiPojavuvana(X,L,N).

%dolzina(L,N) N e brojot na elementi vo L
dolzina([],0).
dolzina([_|L],N):-dolzina(L,M), N is M+1.


%ZAD 3
proveri([A,B]):-B>A.
proveri([A,B,C|Ostatok]):-proveri_uslov3([A,B,C|Ostatok]).
    
proveri_uslov3([_]).
proveri_uslov3([A,B]):-B>A.
proveri_uslov3([A,B,C|Ostatok]):-B>A, C<B, proveri_uslov3([C|Ostatok]).


%ZAD 4
%permutacii(L1,L2). L2 e lista od site permutacii na L1
permutacii(L1,L2):-setof(P,permutacija(P,L1),L2).

%permutacija(L1,L2) True ako L1 e permutacija na L2
permutacija([],[]).
permutacija([X|L1],L2):-izvadiElem(L2,X,L3), permutacija(L1,L3).

%izvadiElem(L1,X,L2). od listata L1 isvadi go elementot X a ostatokot zapisi go vo L2
izvadiElem([X|L],X,L).
izvadiElem([X|L1],Y,[X|L2]):-izvadiElem(L1,Y,L2).


%ZAD 5
%convert(L,X). X e decimalniot zapis na brojot L koj e vo binaren zapis vo format na lista
%convert([X],X).
convert(L,Out) :- convert(L,0,Out).
convert([],Tmp,Tmp).
convert([X|Rest],Tmp,Out) :-
    NewTmp is (Tmp * 2) + X,
    convert(Rest,NewTmp,Out).

%convertR(X,L). L e binaren zapis vo format na lista na decimalniot broj X
convertR(X,L):-convertR(X,[],L).
convertR(0,L,L):-!.
convertR(X,L,Out):-
    Ostatok is X mod 2,
    Kolicnik is X // 2,
    convertR(Kolicnik,[Ostatok|L],Out).

%sobiranje(L1,L2,R):- R e zbir od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
sobiranje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 + X2,
    convertR(R_dec,R).

%odzemanje(L1,L2,R):- R e razlika od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
odzemanje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 - X2, 
    (R_dec>0, convertR(R_dec,R),!);
	R=[0].

%mnozenje(L1,L2,R):- R e proizvod od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
mnozenje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 * X2,
    convertR(R_dec,R).

%delenje(L1,L2,R):- R e celobroen kolicnik od L1 i L2 kade i L1 i L2 i R se vo binaren zapis
delenje(L1,L2,R):-
    convert(L1,X1),
    convert(L2,X2),
    R_dec is X1 // X2,
    convertR(R_dec,R).


%ZAD 6










