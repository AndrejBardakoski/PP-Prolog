%ZAD 1
%Data
lice(1,petko,petkovski,m,datum(1,3,1950),kratovo,skopje).
lice(2,marija,petkovska,z,datum(30,5,1954),kumanovo,skopje).
lice(3,ljubica,petkovska,z,datum(29,11,1965),skopje,skopje).
lice(4,vasil,vasilev,m,datum(8,4,1954),bitola,bitola).
lice(5,elena,vasileva,z,datum(19,6,1958),resen,bitola).
lice(6,krste,krstev,m,datum(9,8,1948),veles,veles).
lice(7,biljana,krsteva,z,datum(13,8,1949),veles,veles).
lice(8,igor,krstev,m,datum(26,10,1971),veles,skopje).
lice(9,kristina,krsteva,z,datum(30,5,1974),kumanovo,skopje).
lice(10,julija,petrova,z,datum(30,5,1978),skopje,skopje).
lice(11,bosko,petkovski,m,datum(13,11,1981),skopje,skopje).
lice(12,gjorgji,vasilev,m,datum(15,7,1978),bitola,bitola).
lice(13,katerina,petkovska,z,datum(11,12,1979),bitola,skopje).
lice(14,petar,vasilev,m,datum(21,2,1982),skopje,skopje).
lice(15,andrej,krstev,m,datum(3,8,1998),skopje,skopje).
lice(16,martina,petkovska,z,datum(5,12,2005),skopje,skopje).
familija(1,2,[9,10]).
familija(1,3,[11]).
familija(4,5,[12,13,14]).
familija(6,7,[8]).
familija(8,9,[15]).
familija(11,13,[16]).

%lice(Sifra,Ime,Prezime,Pol,Data_raganje,Mesto_raganje,Mesto_ziveenje),
%familija(Sifra_tatko,Sifra_majka,Lista_sifri_deca) 

%Task 1
%rodeni_razlicen_grad(Kolku). 
rodeni_razlicen_grad(Kolku):-
	findall(X,rodenRazlicenGrad(X),L),
    dolzina(L,Kolku).

%rodenRazlicenGrad(L) True ako liceto so sifra L e rodeno vo razlicen grad od gradot na roditelite
rodenRazlicenGrad(L):-
    lice(L,_,_,_,_,GLice,_),
    familija(T,M,D), clen(L,D),
    rodenGrad(T,GTatko), rodenGrad(M,GMajka),
	GLice\==GTatko, GLice\==GMajka.

%rodenGrad(L,G) G e gradot vo koj e rodeno liceto so sifra L
rodenGrad(L,G):-lice(L,_,_,_,_,G,_).

%clen(X,L) True ako X e element na L.
clen(X,[X|_]).
clen(X,[_|L]):-clen(X,L).

%dolzina(L,N) N e dolzina na listata L
dolzina([],0).
dolzina([_|L],N):-dolzina(L,M), N is M+1.

%Task 2
%predci(Sifra,L)
predci(Sifra,L):-findall(P,predok_uslov(Sifra,P),L).

%roditel(L,R) True ako R e roditel na L
roditel(L,R):-familija(R,_,D),clen(L,D).
roditel(L,R):-familija(_,R,D),clen(L,D).

%predok(L,P) P e predok na L
predok(L,P):-roditel(L,P).
predok(L,P):-roditel(L,R),predok(R,P).

%predok_uslov(L,P) P e predok na L, L i P se od ist pol i imaat blizok rodenden.
predok_uslov(L,P):-predok(L,P),
    lice(L,_,_,Pol,LData,_,_),
    lice(P,_,_,Pol,PData,_,_),
    blizokRodenden(LData,PData).

%blizokRodenden(Data1,Data2) Data1 i Data2 se razlikuvaat za edna nedela godinata e nebitna.
blizokRodenden(datum(D1,M,_),datum(D2,M,_)):- D1>=D2,Raz=D1-D2, Raz=<7.
blizokRodenden(datum(D1,M,_),datum(D2,M,_)):- D1<D2,Raz=D2-D1, Raz=<7.
blizokRodenden(datum(D1,M1,_),datum(D2,M2,_)):- MRaz is M1 - M2, MRaz=1,
    D1_new is D1+30, Raz is D1_new - D2, Raz=<7.
blizokRodenden(datum(D1,M1,_),datum(D2,M2,_)):- MRaz is M2 - M1, MRaz=1,
    D2_new is D2+30, Raz is D2_new - D1, Raz=<7.


%ZAD2
%Data
telefon(111111,petko,petkovski,[povik(222222,250),povik(101010,125)]).
telefon(222222,marija,petkovska,[povik(111111,350),povik(151515,113),povik(171717,122)]).
telefon(333333,ljubica,petkovska,[povik(555555,150),povik(101010,105)]).
telefon(444444,vasil,vasilev,[povik(171717,750)]).
telefon(555555,elena,vasileva,[povik(333333,250),povik(101010,225)]).
telefon(666666,krste,krstev,[povik(888888,75),povik(111111,65),povik(141414,50),povik(161616,111)]).
telefon(777777,biljana,krsteva,[povik(141414,235)]).
telefon(888888,igor,krstev,[povik(121212,160),povik(101010,225)]).
telefon(999999,kristina,krsteva,[povik(666666,110),povik(111111,112),povik(222222,55)]).
telefon(101010,julija,petrova,[]).
telefon(121212,bosko,petkovski,[povik(444444,235)]).
telefon(131313,gjorgji,vasilev,[povik(141414,125),povik(777777,165)]).
telefon(141414,katerina,petkovska,[povik(777777,315),povik(131313,112)]).
telefon(151515,petar,vasilev,[]).
telefon(161616,andrej,krstev,[povik(666666,350),povik(111111,175),povik(222222,65),povik(101010,215)]).
telefon(171717,martina,petkovska,[povik(222222,150)]).
sms(111111,[222222,999999,101010]).
sms(444444,[333333,121212,161616]).
sms(111111,[777777]).
sms(666666,[888888]).
sms(444444,[555555,121212,131313,141414]).
sms(666666,[777777,888888]).
sms(888888,[999999,151515]).
sms(171717,[131313,161616]).

%telefon(Broj,Ime,Prezime,Lista_na_pojdovni_povici)
%povik(Povikan_broj,Traenje)
%SMS(Broj_koj_ja_ispraka_porakata,Lista_broevi_koi_ja_dobivaat_porakata)

%Task 1
%najbroj(X,Y)  X ime, Y prezime na brojot koj ostvaril najmnogu povici

najbroj(X,Y):- maxRazgovori(Tel), getImePrez(Tel,X,Y).

maxRazgovori(Tel):-findall((T,N),brojPovici(T,N),[M|L]), maxRazgovori(L,M,Tel).

maxRazgovori([],(Tel,_),Tel):-!.
maxRazgovori([(X,N)|L],(_,TempMax),Tel):- N>TempMax, maxRazgovori(L,(X,N),Tel).
maxRazgovori([(_,N)|L],(TempX,TempMax),Tel):- N=<TempMax, maxRazgovori(L,(TempX,TempMax),Tel).

%razgovor(T1,T2) True ako T1 i T2 imale telefonski razgovor
razgovor(T1,T2):-povikan(T1,T2).
razgovor(T1,T2):-povikan(T2,T1).

%povikan(Tel1,Tel2) True ako Tel2 go povikal Tel1
povikan(T1,T2):- telefon(T2,_,_,L),clen(povik(T1,_),L).
    
%brojPovici(T,N) N e brojot na povici kon i od T.
brojPovici(T,N):- setof(T2,razgovor(T,T2),L),dolzina(L,N).

getImePrez(Tel,Ime,Prezime):-telefon(Tel,Ime,Prezime,_).


%Task 2




    
    


