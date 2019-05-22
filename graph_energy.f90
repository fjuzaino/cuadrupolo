SUBROUTINE eKU(a,b,r)
  USE charged_objects
  USE vector_functions
  IMPLICIT NONE
  !K: Energía Cinética
  !U: Energía Potencial
  !proton3%mass: Masa
  !ac: Aceleración
  !r: Radio
  !v: Velocidad
  !i: Contador
  !a: Variable dummy para el protonmovil enviado del programa principal
  !-------------------------------------------
  REAL(d)::r,Ki,U !radio, energía cinetica y potencial respectivamente
  REAL(d),DIMENSION(3)::ac
  TYPE(charged_particles),INTENT(IN)::a !partícula cargada
  REAL(d),INTENT(IN)::b !!tiempo
	
	!Se abre el archivo donde se tabulará
  OPEN(UNIT=132,FILE="Graph",STATUS="UNKNOWN",ACTION="WRITE",ACCESS="APPEND")
 !! a%vel=a%mom/a%mass !velocidad de la partícula
  ac=a%Felec/a%mass
  Ki=(1.0/2.0)*a%mass*mag(a%mom/a%mass)**2 !calculo de la energía cinética
  U=a%mass*mag(ac)*r !!cambiar esto, por esta formula https://en.wikipedia.org/wiki/Electric_potential_energy
                !!usar la formula para n-cargas
  WRITE(132,*)b,U,Ki,U+Ki  !tabular la gráfica
  CLOSE(132)
END SUBROUTINE eKU
