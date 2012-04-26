#open "graphics";;
open_graph "";;
clear_graph ();;
 
let diverge_julia a b cr ci n =
let x = ref a in
let y = ref b in
let xtemp=ref 0. in
let ytemp=ref 0. in
let k = ref 0 in
while ((!x *. !x+. !y *. !y) &lt;4.)&&(!k&lt;n) do
xtemp:=(!x)*. (!x)-. (!y)*. (!y)+. cr;
ytemp:=2. *. (!x) *.(!y)+. ci;
x:= !xtemp;
y:= !ytemp;
k:= !k+1;
done;
(!k);;
 
let julia cr ci n d =
clear_graph ();
let k = ref 0 in
for x=(-511) to (511) do
for y=(-383) to (383) do
k:=((diverge_julia ((float_of_int x) /. d) ((float_of_int y) /. d) cr ci n)*10);
set_color (rgb (255-(!k)/2) (255-(!k)/2) (255-(!k)));
plot (512+x) (384+y);
done;
done;;
 
julia (-0.181) (-0.667) 100 200.;;
(* les deux premiers arguments correspondent respectivement à la partie réelle et la partie imaginaire de c et ils ont le type float *)
(* le troisième argument correspond au nombre d'itérations et il a le type int*)
(* le dernier argument correspond à un coefficient de zoom *)
