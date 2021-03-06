(* For use in CSC540 Project -- String 2015  *)



Control.Print.printDepth  := 200;        (* set the depth of an object (list) to print *) 
Control.Print.printLength := 200;        (* set the length of an object (list) to print *) 



(* DEFINE THE LOGICAL CONNECTIVES *)
infix -->;  (* NB: "->" is reserved for tycon statements involving functions*) 
infix v;
infix &;
infix <->;

(* DATA TYPE FOR A SENTENCE *) 
datatype sentence =  P | Q | R | S | T             (* allowable sent. vars    *) 
                   | ~ of sentence                 (* negation:      ~P       *) 
                   | v of sentence * sentence      (* disjunction:   P v Q    *) 
                   | & of sentence * sentence      (* conjunction:   P & Q    *) 
                   | --> of sentence * sentence    (* conditional:   P --> Q  *) 
                   | <-> of sentence * sentence;   (* biconditional: P <-> Q  *) 


(* REMOVE ARROWS -- removeArows *) 
fun removeArows(~f)      = ~(removeArows (f))  
  | removeArows(f & g)   = removeArows(f) & removeArows(g)
  | removeArows(f v g)   = removeArows(f) v removeArows(g)
  | removeArows(f --> g) = ((~(f)) v (g))
  | removeArows(f <-> g) = removeArows(f) v removeArows(f) & removeArows(g) v removeArows(f) & removeArows(f) v removeArows(g) & removeArows(g) v removeArows(g)
  | removeArows(f) = (f);

(* BRING IN NEGATION, REMOVING DOUBLE NEGATIONS AS WE GO  *) 
fun bringInNegation(~(~ f))     = bringInNegation(f)
  | bringInNegation(~(f & g))   = bringInNegation(f) & bringInNegation(g)
  | bringInNegation(~(f v g))   = bringInNegation(f) v bringInNegation(g)
  | bringInNegation(~(f --> g)) = bringInNegation(f) --> bringInNegation(g)
  | bringInNegation(~(f <-> g)) = bringInNegation(f) <-> bringInNegation(g)
  | bringInNegation(f) = (f);

(* DISTRIBUTE THE DISJUNCTION IN THE CONJUNCTIONS *) 
fun distributeDisjInConj(f v (g & h)) = distributeDisjInConj f v distributeDisjInConj g & distributeDisjInConj h 
(**)
  | distributeDisjInConj(f v g)   = distributeDisjInConj f v distributeDisjInConj g
  | distributeDisjInConj(f) = (f);
  (**)
  
fun show P = (print "P")
| show Q = (print "Q")
| show R = (print "R")
| show S = (print "S")
| show T = (print "T")
| show (~f) = (print "-";
			 show f)
| show (f v g) = (show f;
				  print "v";
				  show g)
				  
| show (f & g) = (show f;
  				  print "&";
  				  show g)
				  
| show (f <-> g) = (show f;
  				  print "<->";
  				  show g)	 				 

| show(f --> g) = (show (f);
  					 print "->";
					 show (g))
		
  					

fun cnf (~(f)) = bringInNegation(~(f))
| cnf (f --> g) = removeArows(f --> g)
| cnf (f <-> g) = removeArows(f <-> g)
| cnf (f v g)   = distributeDisjInConj(f v g)
| cnf(f) = (f);

(* TOP LEVEL FUNCTIONS *)
fun run s  =  (print "\nSentence is: "; 
               show s; 
               print "\n Its CNF is: ";
               show(cnf s); 
               print "\n\n");

fun printNStr(s,0) = ()
  | printNStr(s,n) = (print s; printNStr(s,n-1));

fun go1(_,_,nil) = print "\n"
  | go1(i,n,s::ss) = if i>n 
                         then () 
                     else (print "\n";
                           if i>=10 then printNStr(" ",69) else printNStr(" ",70);
                           print "Example F";
                           print(Int.toString i);
                           run s; 
                           printNStr("=", 80);
                           go1(i+1,n,ss));

(* TOP LEVEL DRIVING FUNCTION *)
fun go s =  let 
                val count = length s
            in
                (printNStr("=",80);
                 go1(1,count,s) )
            end;
