Problem: NS;
Principals: Alice, Bob;
Functions: aenc/2, adec/2, pk/1, m1/0, m2/0, m3/0;
Equations: adec(aenc(x,pk(y)),y) = x;

Protocol:

let Proto(A @ Alice, B @ Bob) =

Bob -> Alice: pkB = pk(B)
Alice { new nA; }
Alice -> Bob: x = aenc(<m1(), pk(A), nA>, pkB)
Bob { new nB; let pkA = fst(snd(adec(x, B))); let nA = snd(snd(adec(x, B))); }
Bob -> Alice: y = aenc(<m2(), nA, nB, pk(B)>, pkA)
Alice { let (%nA) = fst(snd(adec(y, A))); let nB = fst(snd(snd(adec(y, A)))); let (%pkB) = snd(snd(snd(adec(y, A)))); }
Alice -> Bob: z = aenc(<m3(), nB>, pkB)
Bob { let (%nB) = adec(z, B); }
end

in

Alice { new A; }
Bob { new B; }
Proto(A, B)
