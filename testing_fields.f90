PROGRAM testing_fields
USE charged_objects
USE vector_functions
IMPLICIT NONE

!!Definiendo mis objetos masivos
TYPE(charged_particles) :: antiproton1,antiproton2 !!Objeto tipo partícula cargada: antiprotón
TYPE(charged_particles) :: proton1,proton2,protonmovil !!Objeto tipo partícula
!!cargada: proton estacionario
TYPE(data) :: sim !!Objeto con informaci´on de la simulación

!!Definiendo variables locales
REAL(d) :: t !!Instante de tiempo en cada aproximaci´on
REAL(d) :: scale=1E9 !!Escalar longitudes en los plots
!!REAL ( d ), DIMENSION ( 3 ) :: runit !!Vector Unitario
INTEGER :: unit0 !!Unidad para archivo de salida
INTEGER::i,j
TYPE(charged_particles),DIMENSION(4)::particulas !Arreglo contenedor de las 4 partículas.
particulas(1)=proton1
particulas(2)=proton2
particulas(3)=antiproton1
particulas(4)=antiproton2
DO i=1,4
IF(i==1 .OR. i==2)THEN
particulas(i)%mass= 1.7E-27_d
particulas(i)%q= 1.6E-19_d
particulas(i)%vel= vector(0.0_d,0.0_d,0.0_d)
particulas(i)%pos= vector(0.0_d,0.0_d,0.0_d)
ELSE IF(i==3 .OR. i==4)THEN
particulas(i)%mass= 1.7E-27_d
particulas(i)%q= -1.6E-19_d
particulas(i)%vel= vector(0.0_d,0.0_d,0.0_d)
IF(i==3)THEN
particulas(i)%pos= vector(0.0_d,0.5E-8_d,0.0_d)
ELSE IF(i==4)THEN
particulas(i)%pos= vector(0.0_d,-0.5E-8_d,0.0_d)
END IF
END IF
END DO

!Asignación del campo magnetico
sim%Bmagne = vector(0.0_d,0.0_d,0.0_d)

!!Asignando valores físicos a la particula en movimiento (MKS)
protonmovil%mass = 1.7E-27_d !!Masa del antiproton
protonmovil%q = -1.6E-19_d !!carga del antiproton


!Construyendo el vector de velocidad inicial del antiproton.
protonmovil%vel = vector(1E4_d,1E4_d,0.0_d)
!Construyendo el vector de posición inicial del antiproton.
protonmovil%pos = vector(0.7E-9_d,0.0_d,0.0_d)


!!Construyendo el vector de Momento lineal inicial del antiproton.
protonmovil%mom = protonmovil%mass*protonmovil%vel
protonmovil%Eelec = 0.0_d

!!Ajustando el tama~no de paso (Cantidad de veces en las que se
!!actualizará la información del sistema).
sim%N_step = 100000 !!AQUÍ EL LIMITE ES CUANDO EL PROGRAMA HACE KAPUTT
sim%ttot = 1E-12_d !!En este ejemplo se realizará la
!!evolución durante unos segundos
!sim%ttot = sim%ttot/t0 !!Traduciendo a unidades naturales.
sim%dt = sim%ttot/sim%n_step !!Calculando el ancho de paso

!!Abriendo el archivo de texto
OPEN(NEWUNIT=unit0,FILE="Sim_ejemplo",STATUS="UNKNOWN",ACTION="WRITE")
!!Archivo donde se escirbirán la energía cinética y potencial
OPEN(NEWUNIT=unit1,FILE="Energies",STATUS="UNKNOWN",ACCESS="APPEND")

t=0 !!La simulación inicia al tiempo t=0
sim%potencial_total = 0.0_d
sim%Fenet = 0.0_d
!Repetir el proceso hasta que se cumpla la condición
DO
   IF(t > sim%ttot ) EXIT
   !protonmovil%Felec = 0.0_d !!Se reinicia la fuerza neta para este instante
   sim%potencial_total = 0.0_d
   sim%Fenet = 0.0_d
   

   DO j=1,4
      ! protonmovil%Eelect=0.0_d !!No hace falta
      sim%pos2 = protonmovil%pos-particulas(j)%pos !!Calculando el vector r=r2-r1
      runit=sim%pos2/mag(sim%pos2)
      protonmovil%Eelec=(k*particulas(j)%q/mag(sim%pos2)**2)*runit
      protonmovil%Felec = protonmovil%q*protonmovil%Eelec
      
      !CALL campoelectrico(protonmovil,mag(sim%pos2))
      sim%Fenet = sim%Fenet + protonmovil%Felec
      sim%potencial_total = sim%potencial_total + k*protonmovil%q*particulas(j)%q/mag(sim%pos2)
   END DO

!!Calculando la interacción eléctrica.

protonmovil%Fmagn = protonmovil%q*cross_product(protonmovil%mom/protonmovil%mass, sim%Bmagne)
protonmovil%Felma = sim%Fenet + protonmovil%Fmagn
CALL graph_energy(protonmovil, t, sim)

!!Actualizando el momento lineal
protonmovil%mom = protonmovil%mom + protonmovil%Felma * sim%dt
!!Actualizando la posición
protonmovil%pos = protonmovil%pos +( protonmovil%mom / protonmovil%mass) * sim%dt
!RINT*, mag(antiproton%mom/antiproton%mass)
!CALL graph_energy(antiproton, t, mag(sim%pos2), proton%q)
WRITE(unit0,*)t,  protonmovil%pos*scale !!Escribiendo en el archivo
t = t + sim%dt !!Actualizando el tiempo
ENDDO
CLOSE ( unit1 )
CLOSE ( unit0 )
!!Este archivo sirve para la informaci´on del sol
OPEN ( NEWUNIT = unit0 , FILE = "proton" , STATUS = "UNKNOWN" )
!WRITE ( unit0 , * ) proton%pos
CLOSE ( unit0 )

CALL SYSTEM("gnuplot -p data_plot.plt") !!Graficar los resultados

END PROGRAM testing_fields
