Problem: NS;
Principals: Alice, Bob;
Functions: aenc/2, adec/2, pk/1;
Equations: adec(aenc(x,pk(y)),y) = x;

Protocol:

let Proto(A @ Alice, B @ Bob) =

Bob -> Alice: pkB = pk(B)
Alice { new nA; }
Alice -> Bob: x = aenc(<pk(A), nA>, pkB)
Bob { new nB; let pkA = fst(adec(x, B)); let nA = snd(adec(x, B)); }
Bob -> Alice: y = aenc(<nA, nB>, pkA)
Alice { let (%nA) = fst(adec(y, A)); let nB = snd(adec(y, A)); }
Alice -> Bob: z = aenc(nB, pkB)
Bob { let (%nB) = adec(z, B); }
end

in

Alice { new A; }
Bob { new B; }
Proto(A, B)