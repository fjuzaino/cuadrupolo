SUBROUTINE campoelectrico(a,b,c)
USE charged_particles
USE vector_functions
  IMPLICIT NONE
  REAL,DIMENSION(3)::a,b,c
  !!Calculando la interacción eléctrica.
  a%Eelec=(k*a%q/mag(c)**2)*runit
  a%Felec = a%Felec+a%q*a%Eelec
END SUBROUTINE campoelectrico
