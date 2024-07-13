:- use_module(library(gsl)).

% Calculate the eigenvalues/eigenvectors of a matrix...

main :-
	mat_from_list(M,[
		[1/1,1/2,1/3,1/4],
		[1/2,1/3,1/4,1/5],
		[1/3,1/4,1/5,1/6],
		[1/4,1/5,1/6,1/7]
		]),

	mat_eigen(M, Vals, Vecs),
	format("eigenvalues = ~w~neigenvectors = ~w~n", [Vals,Vecs]),
	gsl_matrix_free(M).

/*
Compare the output from above:

	$ tpl -g main,halt -f samples/test_matrix_eigen.pl
	eigenvalues = [9.6702304022603313e-05,0.00673827360576064,0.16914122022145,1.50021428005924]
	eigenvectors = [[-0.029193323164787,0.328712055763193,-0.791411145833126,0.51455274999715],[-0.179186290535455,0.741917790628452,-0.100228136947188,-0.638282528193617],[0.582075699497237,-0.3705021850670927,-0.5095786345018,-0.514048272222165],[0.792608291163764,0.4519231209016,0.322416398581825,0.252161169688242]]

 to:

	$ octave -q
	QSocketNotifier: Can only be used with threads started with QThread
	octave:1> [v,d] = eig(hilb(4));
	octave:2> diag(d)
	ans =

	   9.6702e-05
	   6.7383e-03
	   1.6914e-01
	   1.5002e+00

	octave:3> v
	v =

	  -0.029193   0.179186   0.582076   0.792608
	   0.328712  -0.741918  -0.370502   0.451923
	  -0.791411   0.100228  -0.509579   0.322416
	   0.514553   0.638283  -0.514048   0.252161

	octave:4>
	$
*/
