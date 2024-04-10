b:
	dune build

bi:
	dune test --instrument-with bisect_ppx --force

se:
	bisect-ppx-report html

ct:
	open _coverage/index.html

u:
	dune utop

t: 
	dune test

c: 
	dune clean

e:
	dune exec _build/default/bin/main.exe 