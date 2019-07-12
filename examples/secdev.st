Problem: SecureDevice;
Principals: Alice, SD;
Functions: left/0, right/0, projl/1, projr/1;
Equations: ;

Protocol:

let SD() =
    Alice {new x;}
    Alice -> SD: match x with {
    left():
	let SD_left() =
	    Alice {new y;}
	    Alice -> SD: y' = y
	    SD -> Alice: z = projl(y')
	    SD_left() in
	SD_left()
    right():
	let SD_right() =
	    Alice {new y;}
	    Alice -> SD: y' = y
	    SD -> Alice: z = projr(y')
	    SD_right() in
	SD_right()
    } in
SD()