SUBROUTINE graph_energy(a,b,c)
  USE charged_objects
  USE vector_functions
  IMPLICIT NONE
  !Ki: Energía Cinética
  !U: Energía Potencial
  !a%mass: Masa
  !a: Variable dummy para el protonmovil enviado del programa principal
  !r: Radio (distancia entre las partículas que interactúan)
  !b: Tiempo
  !-------------------------------------------
  REAL(d)::Ki,U !radio, energía cinetica y potencial respectivamente
  TYPE(charged_particles),INTENT(IN)::a !partícula cargada
  REAL(d),INTENT(IN)::b !!tiempo
  TYPE(data),INTENT(IN)::c
  
	



  Ki=(1.0/2.0)*a%mass*mag(a%mom/a%mass)**2 !calculo de la energía cinética
  !  U= r*mag(c%Fenet)            !energía potencial con n cargas puntuales
  
                !!usar la formula para n-cargas
  WRITE(unit1,*) b, Ki, c%potencial_total, c%potencial_total + Ki  !tabular la gráfica
  
  END SUBROUTINE graph_energy
