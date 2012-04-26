#open "graphics";;
open_graph "";;
clear_graph ();;
 
let diverge_mandelbrot a b n =
let x = ref a in
let y = ref b in
let xtemp=ref 0. in
let ytemp=ref 0. in
let k = ref 0 in
 
while ((!x *. !x+. !y *. !y) &lt; 4.) && (!k&lt;n) do
xtemp:=(!x)*. (!x)-. (!y)*. (!y)+. a;
ytemp:=2. *. (!x) *.(!y)+. b;
x:= !xtemp;
y:= !ytemp;
k:= !k+1;
done;
(!k);;
 
let mandelbrot n d =
clear_graph ();
let k = ref 0 in
for x=(-511) to (511) do
for y=(-383) to (383) do
k:=((diverge_mandelbrot ((float_of_int x) /. d) ((float_of_int y) /. d) n)*10);
set_color (rgb (0) ((!k)/2) (!k));
plot (512+x) (384+y);
done;
done;;
 
mandelbrot 20 200.;;
(* le premier nombre en argument correspond au nombre d'itérations et le deuxième à un coefficient de zoom *)
(* le premier nombre est de type int mais le deuxième de type float *)
