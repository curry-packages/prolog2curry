% to read this file with standard Prolog, uncomment the next two lines:
:- op(1150,fx,function).
function(_).

% Compute a list of prime numbers not larger than the first argument
% via the sieve of Eratosthenes

:- function all_primes/2.
all_primes(N,Primes) :-
        range(2,N,Numlist),
        sieve(Numlist,Primes).

sieve([],[]).
sieve([N|Ns],[N|Primes]):-
        remove_mults(N,Ns,MPrimes),
        sieve(MPrimes,Primes).

remove_mults(_,[],[]).
remove_mults(N,[Y|Ys],Zs) :-
        Y mod N > 0 -> (remove_mults(N,Ys,Xs), Zs=[Y|Xs])
                     ; remove_mults(N,Ys,Zs).

:- function range/3.
range(M,N,R) :- M=<N -> (M1 is M+1, range(M1,N,R1), R=[M|R1])
                      ; R=[].

mylength([],0).
mylength([_|Xs],L) :- mylength(Xs,L1), L is L1+1.

% compute the number of primes smaller than 50000:
main(N) :- all_primes(50000,Ps), mylength(Ps,N).
