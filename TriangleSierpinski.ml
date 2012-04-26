#open "graphics";;
open_graph "";;
clear_graph ();;
 
let triangle x1 y1 x2 y2 x3 y3 couleur =
set_color couleur;
fill_poly [|(x1,y1);(x2,y2);(x3,y3)|];;
 
let milieu x1 y1 x2 y2 =
((x1+x2)/2,(y1+y2)/2);;
 
let rec sierpinski_rec x1 y1 x2 y2 x3 y3 n couleur = match n with
| 0 -&gt; ()
| _ -&gt;
let (xm12,ym12) = milieu x1 y1 x2 y2 in
let (xm13,ym13) = milieu x1 y1 x3 y3 in
let (xm23,ym23) = milieu x2 y2 x3 y3 in
triangle xm12 ym12 xm13 ym13 xm23 ym23 couleur;
sierpinski_rec x1 y1 xm12 ym12 xm13 ym13 (n-1) couleur;
sierpinski_rec x2 y2 xm12 ym12 xm23 ym23 (n-1) couleur;
sierpinski_rec x3 y3 xm13 ym13 xm23 ym23 (n-1) couleur;;
 
let sierpinski x1 y1 x2 y2 x3 y3 n couleurnoir couleurblanc=
clear_graph();
triangle x1 y1 x2 y2 x3 y3 couleurnoir;
sierpinski_rec x1 y1 x2 y2 x3 y3 n couleurblanc;;
 
sierpinski 212 84 812 84 512 700 7 black white;;
(* les six premiers arguments (x1 y1 x2 y2 x3 y3) correspondent aux coordonnées des 3 points du grand triangle*)
(* le septième argument correspond au nombre d'itérations - si on veut y voir quelque chose, il est recommandé de ne pas dépasser 10 *)
(* le huitième argument correspond à la couleur initiale du triangle et le neuvième à celle des triangles itérés *)
