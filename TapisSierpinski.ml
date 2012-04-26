#open "graphics";;
open_graph "";;
clear_graph ();;
 
let carre x1 y1 x2 y2 x3 y3 x4 y4 couleur =
set_color couleur;
fill_poly [|(x1,y1);(x2,y2);(x3,y3);(x4,y4)|];;
 
let tiers x1 x2 =
((abs (x2-x1))/3);;
 
let rec tapis_rec x1 y1 x2 y2 x3 y3 x4 y4 n couleurblanc = match n with
| 0 -&gt; ()
| _ -&gt;
let d = ((abs (y2-y1))/3) in
 
carre (x1+d) (y1-d) (x2+d) (y2+d) (x3-d) (y3+d) (x4-d) (y4-d) couleurblanc;
tapis_rec x1 y1 x1 (y1-d) (x1+d) (y1-d) (x1+d) y1 (n-1) couleurblanc;
tapis_rec x1 (y1-d) x1 (y1-2*d) (x1+d) (y1-2*d) (x1+d) (y1-d) (n-1) couleurblanc;
tapis_rec x1 (y1-2*d) x1 (y1-3*d) (x1+d) (y1-3*d) (x1+d) (y1-2*d) (n-1) couleurblanc;
tapis_rec (x1+d) (y1-2*d) (x1+d) (y1-3*d) (x1+2*d) (y1-3*d) (x1+2*d) (y1-2*d) (n-1) couleurblanc;
tapis_rec (x1+2*d) (y1-2*d) (x1+2*d) (y1-3*d) (x1+3*d) (y1-3*d) (x1+3*d) (y1-2*d) (n-1) couleurblanc;
tapis_rec (x1+2*d) (y1-d) (x1+2*d) (y1-2*d) (x1+3*d) (y1-2*d) (x1+3*d) (y1-d) (n-1) couleurblanc;
tapis_rec (x1+2*d) y1 (x1+2*d) (y1-d) (x1+3*d) (y1-d) (x1+3*d) y1 (n-1) couleurblanc;
tapis_rec (x1+d) y1 (x1+d) (y1-d) (x1+2*d) (y1-d) (x1+2*d) y1 (n-1) couleurblanc;;
 
let tapis_sierpinski x1 y1 x2 y2 x3 y3 x4 y4 n couleurnoir couleurblanc=
clear_graph();
carre x1 y1 x2 y2 x3 y3 x4 y4 couleurnoir;
tapis_rec x1 y1 x2 y2 x3 y3 x4 y4 n couleurblanc;;
 
tapis_sierpinski 20 749 20 20 749 20 749 749 6 white black;;
(* les huit premiers arguments correspondent aux quatre sommets du grand carré - il est recommandé que le carré ait un côté égal à un multiple de (3 puissance le nombre d'itérations) *)
(* le neuvième argument correspond au nombre d'itérations - ne dépassez pas six itérations ou on ne voit plus rien *)
(* le dixième argument correspond à la couleur initiale du grand carré - ici le blanc - et le dixième à la couleur des carrés que l'on retranche - ici le noir *)
