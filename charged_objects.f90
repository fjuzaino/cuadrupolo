MODULE charged_objects
IMPLICIT NONE
SAVE

!!Constantes importantes para el programa
!!Kind con una precisión de 16 para mayor resolución en el sistema.
INTEGER , PARAMETER :: d = SELECTED_REAL_KIND ( 16 )

!!Introduciendo las unidades naturales del sistema
REAL(d),PARAMETER::pi=3.14159265358979_d !!Pi con este Kind
REAL(d),PARAMETER::k=9E9_d !!K con este Kind
REAL(d),DIMENSION(3)::runit
INTEGER::unit1

!!Tipo publico para producir objetos del tipo planeta
TYPE,PUBLIC::charged_particles
REAL(d),DIMENSION(3)::pos !!Posici´on en el tiempo t
REAL(d),DIMENSION(3)::mom !!Momento Lineal en el tiempo t
REAL(d),DIMENSION(3)::vel !!Velocidad en el tiempo t
REAL(d),DIMENSION(3)::Felec !!Fuerza el´ectrica en el tiempo t
REAL(d),DIMENSION(3)::Eelec !!Campo el´ectrico en el tiempo t
REAL(d),DIMENSION(3)::Fmagn !!Fuerza magnetica sobre la partícula movil
REAL(d),DIMENSION(3)::Felma !!Fuerza electromagnética (Fuerza de Lorentz)
REAL ( d ) :: mass !!Masa de la particula cargada
REAL ( d ) :: pos0 !!Posici´on inicial de la particula
REAL ( d ) :: q !!Carga de la particula
REAL ( d ) :: v0 !!Rapidez inicial de la particula
END TYPE charged_particles

!!Tipo publico para producir objetos del tipo estrella
TYPE, PUBLIC :: data
REAL ( d ), DIMENSION ( 3 ) :: Bmagne
REAL ( d ) :: ttot !!Tiempo total de la simulacion
REAL ( d ) :: dt !!Tama~no de paso
REAL ( d ) :: N_step !!Cantidad de pasos
REAL ( d ), DIMENSION ( 3 ) :: Fenet !!Fuerza el´ectrica neta
REAL ( d ), DIMENSION ( 3 ) :: pos2 !!Vector donde se guarda la
REAL ( d ):: potencial_total !!energía potencial total en un instante t
!!diferencia de posici´on entre los objetos
!INTEGER::unit1 !!unidad donde se escribirá la energía potenacial y cinética
END TYPE data

END MODULE charged_objects
