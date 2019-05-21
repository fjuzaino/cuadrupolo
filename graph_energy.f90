SUBROUTINE eKU(a,b)
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
  REAL::ac,r,K,U !aceleración, radio, energía cinetica y potencial respectivamente
  TYPE(charged_particle),INTENT(IN)::a !partícula cargada
  REAL,INTENT::b !!tiempo
	
	!Se abre el archivo donde se tabulará
  OPEN(UNIT=132,FILE="Graph",STATUS="UNKNOWN",ACTION="WRITE")
  a%vel=a%mom/a%mass !velocidad de la partícula
  ac=a%Felec/a%mass
  K=(1/2)*a%mass*a%vel**2 !calculo de la energía cinética
  U=a%mass*ac*r !!cambiar esto, por esta formula https://en.wikipedia.org/wiki/Electric_potential_energy
                !!usar la formula para n-cargas
  WRITE(132,*)b,U,K,U+K  !tabular la gráfica
  CLOSE(132)
END SUBROUTINE eKU
