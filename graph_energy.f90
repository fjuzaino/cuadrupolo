SUBROUTINE graph_energy(a,b,r,C)
  USE charged_objects
  USE vector_functions
  IMPLICIT NONE
  !Ki: Energía Cinética
  !U: Energía Potencial
  !proton3%mass: Masa
  !ac: Aceleración
  !r: Radio
  !v: Velocidad
  !i: Contador
  !a: Variable dummy para el protonmovil enviado del programa principal
  !C: Variable dummy para la carga del antiproton/proton que actúa
  !r: Radio (distancia entre las partículas que interactúan)
  !B: Tiempo
  !-------------------------------------------
  REAL(d)::r,Ki,U !radio, energía cinetica y potencial respectivamente
  TYPE(charged_particles),INTENT(IN)::a !partícula cargada
  REAL(d),INTENT(IN)::b !!tiempo
	
	!Se abre el archivo donde se tabulará
  OPEN(UNIT=132,FILE="Graph",STATUS="UNKNOWN",ACTION="WRITE",ACCESS="APPEND")
 !! a%vel=a%mom/a%mass !velocidad de la partícula
  Ki=(1.0/2.0)*a%mass*mag(a%mom/a%mass)**2 !calculo de la energía cinética
  U= k*a%q*(C/(MAG(r)))            !energía potencial con n cargas puntuales
                !!usar la formula para n-cargas
  WRITE(132,*)b,U,Ki,U+Ki  !tabular la gráfica
  CLOSE(132)
END SUBROUTINE graph_energy
