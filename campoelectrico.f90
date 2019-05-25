SUBROUTINE campoelectrico(a,b)
USE charged_objects
USE vector_functions

  IMPLICIT NONE

	TYPE(charged_particles),INTENT(INOUT)::a
  	REAL(d),INTENT(IN)::b

  	!!Calculando la interacción eléctrica.
  	a%Eelec=(k*a%q/b**2)*runit
 	a%Felec = a%q*a%Eelec

END SUBROUTINE campoelectrico
