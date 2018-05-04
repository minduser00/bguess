#-------------------------------------------------------------------------------
# License
#-------------------------------------------------------------------------------
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# The GNU General Public License may be viewed here:
# http://www.gnu.org/copyleft/gpl.html
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#	Comprueba si colores.tcl está cargado y es la versión corecta
#-------------------------------------------------------------------------------

if {![info exists colores_lib_version]} {
	putlog "\[VGUESS\]: Error; no se pudo encontrar colores.tcl"
	return
} elseif { $colores_lib_version < 100 } {
	putlog "\[VGUESS\]: Error; se necesita la versión 100 o posterior\
			de colores.tcl"
	return
}

#-------------------------------------------------------------------------------
#	Comprueba si alltools esta cargado y es la versión corecta
#-------------------------------------------------------------------------------

if { !$alltools_loaded } {
	putlog "\[VGUESS\]: Error; no se pudo encontrar alltools.tcl"
	return
} elseif { $allt_version < 206 } {
	putlog "\[VGUESS\]: Error; se necesita la versión 206 o posterior de alltools.tcl"
	return
}

#===============================================================================
# USER SETTINGS
#===============================================================================

#-------------------------------------------------------------------------------
# Files
#-------------------------------------------------------------------------------

set bguess(file) "scripts/bguess.db"
set bgstats(file) "scripts/bgstats.db"
#set bgstats(web) "scripts/web/bgstats.db"
set bgtarget(file) "scripts/bgtarget.db"
#set bgtarget(web) "scripts/web/bgtarget.db"

#-------------------------------------------------------------------------------
# Game Channel
#-------------------------------------------------------------------------------

set bguess(chan) "#The.ColisevM"

#-------------------------------------------------------------------------------
# Banned Nicks
#-------------------------------------------------------------------------------

set bguess(banned_nicks) { [DJ] }

#-------------------------------------------------------------------------------
# Tiempo entre intentos (segundos)
#-------------------------------------------------------------------------------

set bguess(period) 180

#-------------------------------------------------------------------------------
# Selecciona los límites inferior y superior de los números del juego.
#-------------------------------------------------------------------------------

set bglow_num 0
set bghigh_num 99

#-------------------------------------------------------------------------------
# Está permitido jugar cuando solo quede un número?
# 		true = si   -   false = no
#-------------------------------------------------------------------------------

set bgduck_granted false

#-------------------------------------------------------------------------------
# Si no se permite jugar al último número, se guarga el punto en el bote?
# 		true = si   -   false = no
#-------------------------------------------------------------------------------

set bgcan_on true

#-------------------------------------------------------------------------------
# Número de aciertos consecutivos para llevarse el bote.
#-------------------------------------------------------------------------------

set bgrow_can 3

#------------------------------------------------------------------------------
# Puntos a sumar al bote
#------------------------------------------------------------------------------

set bgcan_increase 2

#-------------------------------------------------------------------------------
# Tiempo a conservar las estadísticas de un jugador desde su
# último intento en segundos ( segundos * minutos * horas * dias * meses ).
#-------------------------------------------------------------------------------

set bguess(preservetime) [expr {60 * 60 * 24 * 30 * 1}]

#-------------------------------------------------------------------------------
# Tiempo para borrar la puntuación máxima
# en segundos ( segundos * minutos * horas * dias ).
#-------------------------------------------------------------------------------

set bguess(cleartime) [expr {60 * 60 * 24 * 7}]

#-------------------------------------------------------------------------------
# Rango máximo de estadísticas a listar.
#-------------------------------------------------------------------------------

set bgmaxrange 10

#-------------------------------------------------------------------------------
# Mensajes cuando sólo queda un número por acertar.
#-------------------------------------------------------------------------------

set msgduck [list "Duck!!" "¿Será?" "¿Quizás sea?" "A lo mejor es el..." "¿O no?" "CUAK!!" "No puede ser" "Qué Melón xD!" "JaJaJa" "Casiiii!!!" "Al Palo xD!" "Uyyyyy!" "Por los pelos xD" "Otra vez será xD" "Anda que..." "Outch!!" "Buahh" "Yo creo que es..."]

#-------------------------------------------------------------------------------
# Set the triggers: guess, stats, score
#-------------------------------------------------------------------------------

set bguess(cmdplay) "!vguess"
set bguess(cmdrange) "!vgrange"
#set bguess(cmdweb) "!vguessweb"
set bguess(cmdstats) "!vgstats"

#===============================================================================
# DO NOT ALTER ANYTHING BELOW HERE
#===============================================================================

#--------------------------------------------
# Control de Versión.
#--------------------------------------------

set bgversion "3.10"; set bgrelease "BETA1";

#--------------------------------------------
# Caracteres de contol de colores.
#--------------------------------------------
set b "\002";		# Negritas
set u "\037";		# Subrayado
set bl "\0030";		# Blanco
set n "\0031";		# Negro
set az "\0032";		# Azul Marino
set v "\0033";		# Verde
set rj "\0034";		# Rojo
set m "\0035";		# Marrón
set l "\0036";		# Lila
set na "\0037";		# Naranja
set am "\0038";		# Amarillo
set vc "\0039";		# Verde Claro
set tu "\00310";	# Turquesa
set c "\00311";		# Celeste
set a "\00312";		# Azul
set rs "\00313";	# Rosa
set go "\00314";	# Gris Oscuro
set gc "\00315";	# Gris Claro

#-------------------------------------------------------------------------------
# Binds
#-------------------------------------------------------------------------------

bind pubm - "$bguess(chan) $bguess(cmdplay)*" bguess_play
bind pubm - "$bguess(chan) $bguess(cmdrange)" bguess_range
#bind pubm - "$bguess(chan) $bguess(cmdweb)" bguess_web
bind pubm - "$bguess(chan) $bguess(cmdstats)*" bguess_stats
bind time - "00 01 *" bguess_cleaning
bind join - "$bguess(chan) $botnick*" join_version
bind pub - !bote bgbote

#-------------------------------------------------------------------------------
# Procedures
#-------------------------------------------------------------------------------

# Anuncio del bot al entrar al canal donde esta activo el juego.
proc join_version {nick host hand chan} {
	global bgversion 
	if {[string equal -nocase $::botnick $nick]} {
		putchan $chan [bold [brown "Juego [black Vguess] Cargado - Versión\
			[black "$bgversion [lred "€"]lite"]. by Shiryu & Uru."]]
	}
}

# Muestra en el canal los puntos acumulados en el bote usando el comando !bote
proc bgbote {nick host hand chan text} {
	global bgcan_on  bguess
	# El bote está activado.
	if {$bgcan_on} {
		if {$bguess(can) == 0} {
			putmsg $chan [brown "-> [black [bold $nick]] <- No\
				hay ningún punto acumulado en el bote."]
		} else {
			putmsg $chan [brown "-> [black [bold $nick]] <- Hay [black [bold $bguess(can)]]\
				punto[if {$bguess(can) > 1} {list s}] acumulado[if {$bguess(can) > 1}\
				{list s}] en el bote del juego Vguess."]
		}
	} else {
		putmsg $chan [brown "-> [black [bold $nick]] <- Actualmente el bote está desactivado."]
		utimer 2 [list putnotc $nick [brown "\t\Habla con el/los administrador/es para consensuar su activación. Gracias."]]
	}
}

proc bgusage {chan nick clase} {
	global bgmaxrange
	switch $clase {
		1 { # La ayuda de estadisticas
			putnotc $nick [bold "\t\U  S  O :"]
			utimer 1 [list putnotc $nick "\t[bold !vgstats][brown ": Muestra tus estadísticas en el juego."]"]
			utimer 2 [list putnotc $nick "\t[bold !vgstats][brown " <[black [bold nick]]>: Muestra las estadísticas en el juego de un nick en concreto."]"]
			utimer 3 [list putnotc $nick "\t[bold !vgstats][brown " \[[black [bold -a]]|[black [bold -i]]|[black [bold -p]]|[black [bold -r]]\]: Muestra las estadísticas de los tres primeros en Aciertos ([black [bold -a]]), Intentos ([black [bold -i]]), Puntos ([black [bold -p]]) o Porcentaje de Aciertos ([black [bold -r]]) respectivamente. Por ejemplo [black [bold "!vgstats -p"]] mostrará los tres primeros, clasificados por [black [bold P]]untuación."]"]
			utimer 4 [list putnotc $nick "\t[bold !vgstats][brown " \[[black [bold -a]]|[black [bold -i]]|[black [bold -p]]|[black [bold -r]]\] seguido de un número, muestra las estadísticas de los primeros clasificados hasta ese número si no es mayor de [black [bold $bgmaxrange]], en caso contrario mostrará los  [black [bold $bgmaxrange]] clasificados hasta tu número. Por ejemplo [black [bold "!vgstats -p 6]] mostrará del primero al sexto, clasificados por [black [bold P]]untuación."]"]
			utimer 5 [list putnotc $nick "\t[bold !vgstats][brown " \[[black [bold -a]]|[black [bold -i]]|[black [bold -p]]|[black [bold -r]]\] seguido de dos números, muestra un rango de clasificados desde el primer número hasta el segundo. Por ejemplo [black [bold "!vgstats -p 3 8"]] mostrará del tercero al octavo, clasificados por [black [bold P]]untuación. :-)"]"]
		}
		2 { # para posibles ampliaciones de la ayuda
		}
	}
}

proc bguess_load {} {
	global bguess bglow_num bghigh_num
	if {[file exists $bguess(file)]&&[file size $bguess(file)]>6} {
		set _bguess [open $bguess(file) r]
		gets $_bguess bguess(game)
		gets $_bguess bguess(target)
		gets $_bguess bguess(intentos)
		gets $_bguess bguess(low)
		gets $_bguess bguess(high)
		gets $_bguess bguess(last_winner)
		gets $_bguess bguess(max_one_winner)
		gets $_bguess bguess(max_one_points)
		gets $_bguess bguess(max_one_game)
		gets $_bguess bguess(max_one_time)
		gets $_bguess bguess(can)
		gets $_bguess bguess(in_a_row)
		close $_bguess
	} else {
		set bguess(game) 1
		set bguess(target) [rand_2 $bglow_num $bghigh_num]
		set bguess(intentos) 0
		set bguess(low) $bglow_num
		set bguess(high) $bghigh_num
		set bguess(last_winner) "Por Descubrir :P"
		set bguess(max_one_winner) "Nadie :o)"
		set bguess(max_one_points) 0
		set bguess(max_one_game) 0
		set bguess(max_one_time) [unixtime]
		set bguess(can) 0
		set bguess(in_a_row) 0
		bguess_save
	}
	return
}

proc bguess_save {} {
	global bguess
	set _bguess [open $bguess(file) w]
	puts $_bguess $bguess(game)
	puts $_bguess $bguess(target)
	puts $_bguess $bguess(intentos)
	puts $_bguess $bguess(low)
	puts $_bguess $bguess(high)
	puts $_bguess $bguess(last_winner)
	puts $_bguess $bguess(max_one_winner)
	puts $_bguess $bguess(max_one_points)
	puts $_bguess $bguess(max_one_game)
	puts $_bguess $bguess(max_one_time)
	puts $_bguess $bguess(can)
	puts $_bguess $bguess(in_a_row)
	close $_bguess
	return
}

proc bgstats_load {} {
	global bgstats
	if {[file exists $bgstats(file)]&&[file size $bgstats(file)]>6} {
		set _bgstats [open $bgstats(file) r]
		set bgstats(records) [lsort -dictionary [read $_bgstats]]
		close $_bgstats
		set bgstats(count) [llength $bgstats(records)]
	} else {
		set bgstats(records) ""
		set bgstats(count) 0
	}
	return
}

proc bgstats_save {} {
	global bgstats
	set _bgstats [open $bgstats(file) w]
	puts $_bgstats $bgstats(records)
	close $_bgstats
	return
}

proc bgtarget_load {} {
	global bgtarget
	if {[file exists $bgtarget(file)]&&[file size $bgtarget(file)]>2} {
		set _bgtarget [open $bgtarget(file) r]
		set bgtarget(records) [lsort -dictionary -decreasing [read $_bgtarget]]
		close $_bgtarget
		set bgtarget(count) [llength $bgtarget(records)]
	} else {
		set bgtarget(records) ""
		set bgtarget(count) 0
	}
	return
}

proc vgsearch {lista buscado indice} {
	set i 0
	foreach ficha $lista {
		if {[string equal -nocase $buscado [lindex $ficha $indice]]} {
			return $i
		}
		incr i
	}
	return -1
}

#-------------------------------------------------------------------------------
# Retorna la puntuación de un jugador dado.
#-------------------------------------------------------------------------------

proc bgpuntos {nick} {
	global bgstats
	set found [vgsearch $bgstats(records) $nick 0]
	if {$found >= 0} {
		set ficha [lindex $bgstats(records) $found]
		return [lindex $ficha 3]
	} else {
		return 0
	}
}

#-------------------------------------------------------------------------------
# Update Player Stats
#-------------------------------------------------------------------------------

proc player_stats_update {nick {try 0} {win 0} {score 0}} {
	global bgstats
	set found [vgsearch $bgstats(records) $nick 0]
	if {$found >= 0} {
		# Nick está en las estadísticas, actualizamos su registro.
		set ficha [lindex $bgstats(records) $found]
		set bgstats(records) [lreplace $bgstats(records) $found $found "$nick [expr {[lindex $ficha 1] + $try}] [expr {[lindex $ficha 2] + $win}] [expr {[lindex $ficha 3] + $score}] [unixtime]"]
	} else {
		# Nick no está en la lista, metemos un registro nuevo.
		lappend bgstats(records) "$nick $try $win $score [unixtime]"
		incr bgstats(count)
		# Y ordenamos la lista por orden alfabético.
		set bgstats(records) [lsort -dictionary $bgstats(records)]
	}
	bgstats_save
	return
}

#-------------------------------------------------------------------------------
# Update Target Stats
#-------------------------------------------------------------------------------

proc target_stats_update {target win} {
	global bgtarget
	set found [vgsearch $bgtarget(records) $target 0]
	if {$found >= 0} {
		set ficha [lindex $bgtarget(records) $found]
		set bgtarget(records) [lreplace $bgtarget(records) $found $found "$target [expr {[lindex $ficha 1] + 1}] [expr {[lindex $ficha 2] + $win}]"]
	} else {
		lappend bgtarget(records) "$target 1 $win"
		incr bgtarget(count)
		set bgtarget(records) [lsort -integer -index 0 $bgtarget(records)]
	}
	set _bgtarget [open $bgtarget(file) w]
	puts $_bgtarget $bgtarget(records)
	close $_bgtarget
	return
}

#-------------------------------------------------------------------------------
# Display Stats Nick
#-------------------------------------------------------------------------------

proc display_s_nick { chan args } {
	global bgstats b n az v rj m
	set found [vgsearch $bgstats(records) [lindex $args end] 0]
	if {$found >= 0} {
		set ficha [lindex $bgstats(records) $found]
		set intentos [lindex $ficha 1]
		set aciertos [lindex $ficha 2]
		set puntos [lindex $ficha 3]
		if {[llength $args] == 1} {
			#En "$args" tenemos el nick que ha escrito el comando.
			putmsg $chan [format "$m->$b$v %s$b$m, Con$b$az %d$b$m Intentos,$b$rj\
				%s$b$m[]Has Ganado$b$az %d$b$m Juegos Para Conseguir Un Total De$b$az\
				%d$b$m Puntos. Tienes un porcentaje del$b %0.2f%%$b de punteria :P"\
				[lindex $args 0] $intentos [lindex $args 1] $aciertos $puntos\
				[expr {$aciertos * 100.0 / $intentos}]]
		} else {
			#[lindex $args 0] busca el record de otro jugador [lindex $args 1].
			putmsg $chan [format "$m->$b$v %s$b$m, Con$b$az %d$b$m Intentos,$b$rj\
				%s$b$m Ha Ganado$b$az %d$b$m Juegos Para Conseguir Un Total De$b$az\
				%d$b$m Puntos. Tiene un porcentaje del$b %0.2f%%$b de punteria :P"\
				[lindex $args 0] $intentos [rr1_coder [lindex $args 1]]\
				$aciertos $puntos [expr {$aciertos * 100.0 / $intentos}]]
		}
	} else {
		if {[llength $args] == 1} {
			putnotc $args "$m->$b$v $args$b$m, Aún No Tienes Estadísticas En El Juego. Empieza a Jugar y Diviértete (Escribe <$b$n!vgstats -h$b$m> Para Obtener Ayuda)."
		} else {
			putnotc [lindex $args 0] "$m->$b$rj [rr1_coder [lindex $args 1]]$b$m Aún No Tiene Estadísticas En El Juego."
		}
	}
	return
}

#-------------------------------------------------------------------------------
# Display Stats Range
#-------------------------------------------------------------------------------

proc display_s_range { chan nick tipo text } {
	global bgstats bgtarget bgmaxrange b n az v rj m l
	set args [lrange $text 2 3]
	switch -- [llength $args] {
		0 { # No ha escrito ningún número, ponemos los tres primeros del ranking.
			set desde 1
			set hasta 3
		}
		1 { # Se ha pasado un parámetro, será un número?
			if {[string is int $args]} {
				set hasta $args
				set desde [expr {$hasta - $bgmaxrange + 1}]
			} else {
				putnotc $nick "$m-> $b$rj[]ERROR$b$m: Escribe <$b$n!vgstats -h$b$m> Para Obtener Ayuda."
				return
			}
		}
		2 { # Se han pasado dos parametros, son numeros enteros los dos?
			set desde [lindex $args 0]
			set hasta [lindex $args 1]
			if {[string is int $desde]&&[string is int $hasta]} {
				if {$desde > $hasta} {
					set desde $hasta
					set hasta [lindex $args 0]
				}
			} else {
				putnotc $nick "$m-> $b$rj[]ERROR$b$m: Escribe <$b$n!vgstats -h$b$m> Para Obtener Ayuda."
				return
			}
		}
	}
	decr desde; decr hasta;		#decrementa desde y hasta en 1
	if {$desde < 0} { set desde 0}
	if {$hasta < 0} { set hasta 0}
	if { [expr {$hasta - $desde}] >= $bgmaxrange} { set hasta [expr {$desde + $bgmaxrange - 1}]}
	if {$desde >= $bgstats(count) } {
		putnotc $nick "$m-> No hay nadie en ese rango."
	} else {
		set mensaje "Records de usuarios ordenados por "
		switch -- $tipo {
			1 {set strtipo "intentos"}
			2 {set strtipo "aciertos"}
			3 {set strtipo "puntos"}
			4 {set strtipo "porcentaje de aciertos"}
		}
		append mensaje "$b$strtipo$b."
		set i [expr {$desde + 1}]
		if {$tipo == 4} {
			foreach ficha [lrange [lsort -command cmd_comp $bgstats(records) ] $desde $hasta ] {
				append mensaje [format " Puesto$b %d$b:$b %s$b con un$b %0.2f%%$b; " $i\
					[rr1_coder [lindex $ficha 0]] [expr {[lindex $ficha 2]*100.0/[lindex $ficha 1]}]]
				incr i
			}
		} else {
			foreach ficha [lrange [lsort -decreasing -integer -index $tipo $bgstats(records) ] $desde $hasta] {
				append mensaje [format " Puesto$b %d$b:$b %s $b-$b %d$b[]." $i\
				[rr1_coder [lindex $ficha 0]] [lindex $ficha $tipo]]
			incr i
			}
		}
		putact $chan "$m-> $b$nick $b- $mensaje"
	}
}

#-------------------------------------------------------------------------------
# Display Stats Percent
#-------------------------------------------------------------------------------

proc cmd_comp {a b} {
	# aciertos * 100 / intentos
	set p1 [format "%0.2f" [expr {[lindex $a 2] * 100.0 / [lindex $a 1]}]]
	set p2 [format "%0.2f" [expr {[lindex $b 2] * 100.0 / [lindex $b 1]}]]
	if {$p1 < $p2} { return 1 }
	if {$p1 > $p2} { return -1 }
	return [string compare -nocase [lindex $a 0] [lindex $b 0]]
}

#-------------------------------------------------------------------------------
# Display Stats
#-------------------------------------------------------------------------------

proc bguess_stats {nick uhost hand chan text} {
	set arg1 [lindex $text 1]
	switch -- [string tolower $arg1] {
		"-a" { # Rango por número de aciertos.
			set tipo 2
		}
		"-i" { # Rango por número de intentos.
			set tipo 1
		}
		"-p" { # Rango por puntuación.
			set tipo 3
		}
		"-r" { # Rango por porcentaje.
			set tipo 4
		}
		"-t" { # Rango de estadísticas de números.
			# POR HACER :P
			return
		}
		"-h" { # El usuario pide ayuda.
			bgusage $chan $nick 1
			return
		}
		"" { # No hay argumentos.
			display_s_nick $chan $nick
			return
		}
		default { # El argumento puede ser un nick.
			display_s_nick $chan $nick $arg1
			return
		}
	}
	display_s_range $chan $nick $tipo $text
}

#-------------------------------------------------------------------------------
# Display Range
#-------------------------------------------------------------------------------

proc bguess_range {nick uhost hand chan text} {
	global bguess b v m l msgduck
	set msgd [lindex $msgduck [rand [llength $msgduck]]]
	if { $bguess(low) == $bguess(high) } {
		putact $chan "-> $b$chan$b -> $msgd ->$b\
			$bguess(low)$b <- $msgd ->$b $bguess(low)$b <- $msgd\
			->$b $bguess(low)$b <- $msgd"
	} else {
		set mensaje "$m-> $b$l$nick$b $m- Juego Nº$b$v $bguess(game) $b$m-\
			Un número entre$b$l $bguess(low)$b$m y$b$l $bguess(high)$b$m\
			- Puntos:$b$l [ expr $bguess(high) - $bguess(low) + 1 ]$b$m."
		if {$bguess(max_one_points) != 0} {
			set mensaje "$mensaje $m[]La puntuación más alta fue conseguida\
				por$b$l [rr1_coder $bguess(max_one_winner)]$b$m,\
				con$b$l $bguess(max_one_points)$b$m puntos\
				en el juego N°$b$l $bguess(max_one_game)$b$m."
		}
		putact $chan "$mensaje"
	}
	return
}

#-------------------------------------------------------------------------------
# Display Web Page - Display the web page on the bguess channel
#-------------------------------------------------------------------------------

proc bguess_web {nick uhost hand chan text} {
	global bgstats bgtarget
	putact $chan "-> $nick - http://unapagina.com/ - Página del juego Vguess."
	return
}

#-------------------------------------------------------------------------------
# Inicia el siguiente juego.
#-------------------------------------------------------------------------------

proc bgnext {nick {win false}} {
	global bguess bglow_num bghigh_num
	incr bguess(game) 1
	set bguess(target) [rand_2 $bglow_num $bghigh_num]
	set bguess(intentos) 0
	set bguess(low) $bglow_num
	set bguess(high) $bghigh_num
	set bguess(high) $bghigh_num
	if {$win} {
		set bguess(last_winner) $nick
	}
}

#-------------------------------------------------------------------------------
# Check for ducks
#-------------------------------------------------------------------------------

proc check_duck {chan nick bghi bglo} {
	global b bgcan_on bgcan_increase msgduck bgduck_granted bguess
	if { $bghi == $bglo } {
		set msgd [lindex $msgduck [rand [llength $msgduck]]]
		putact $chan "-> $b$chan$b -> $msgd ->$b $bglo$b <- $msgd ->$b $bglo$b <- $msgd ->$b $bglo$b <- $msgd"
		#Se ha seleccionado no permitir jugar el último número ?.
		if { !$bgduck_granted } {
			# No permite jugar el último número.
			if { $bgcan_on } {
				# El bote está activado, sumamos un punto o lo iniciamos.
				incr bguess(can) $bgcan_increase
				if {$bguess(can) == $bgcan_increase} {
					set msg "Primer[if {$bgcan_increase > 1} {lindex "os $bgcan_increase"}]\
						punto[if {$bgcan_increase > 1} {list s}] para el bote"
				} else {
					set msg "Se añade[if {$bgcan_increase > 1} {list n}]$b $bgcan_increase$b\
						punto[if {$bgcan_increase > 1} {list s}] al bote. Hay$b\
						$bguess(can)$b puntos acumulados"
				}
				putact $chan "-> $msg."
			} else {
				putact $chan "-> Punto perdido :P"	
			}
			bgnext $nick
		} else {
			# Se permite jugar el ultimo número.
		}
	}
}

#-------------------------------------------------------------------------------
# Manejo del bote
#-------------------------------------------------------------------------------

proc bgcan {nick chan} {
	global bgcan_on bgduck_granted bgrow_can bguess b m n
	if {!$bgduck_granted} {
		if {$bgcan_on && $bguess(can) > 0} {
			if {[string equal -nocase $bguess(last_winner) $nick]} {
				# El último ganador repite acierto.
				incr bguess(in_a_row)
			} else {
				set bguess(in_a_row) 1
			}
			if {$bguess(in_a_row) == [expr $bgrow_can - 1]} {
				# Un acierto más y nick se lleva el bote.
				putact $chan "$m-> $b$n$nick$b $m- Un acierto\
					más y el bote de$n$b $bguess(can)$b$m punto[if {$bguess(can) > 1} {list s}] será tuyo."
			} elseif {$bguess(in_a_row) >= $bgrow_can} {
				# Bote para el ganador.
				putact $chan "$m-> $b$n$nick$b $m- Felicidades!!\
					Has ganado$n$b $bguess(in_a_row)$b$m juegos de forma consecutiva.\
					Te llevas el bote de$b$n $bguess(can)$m$b punto[if {$bguess(can) > 1} {list s}]."
				player_stats_update $nick 0 0 $bguess(can)
				set bguess(in_a_row) 0
				set bguess(can) 0
			}
		}
	}
}

#-------------------------------------------------------------------------------
# Play the game
#-------------------------------------------------------------------------------

proc bguess_play {nick uhost hand chan text} {
	global bguess bghosts bgstats bgtarget a b n az v rj m l
	global msgduck bglow_num bghigh_num
	# Ignora todo excepto la primera parte del texto después del comando !vguess.
	set text [lindex [split $text] 1]
	# Establece el tiempo de la última respuesta para éste jugador.
	if { [catch { set t $bghosts($uhost) }] != 0 } { set t 0 }
	# Diversos controles para asegurar una respuesta válida.
	if { [lsearch -regexp [split $bguess(banned_nicks)] "(?i)$nick" ] >= 0 } {
		# Filtra los nicks no permitidos para jugar.
		putnotc $nick "$m-> Tienes prohibido jugar al $b$n[]Vguess$b$m."
	} elseif { $text == "" } {
		# Se debe introducir un número tras el comando.
		putnotc $nick "$m-> $b$rj[]ERROR$b$m: es $b$n!vguess$b$m <$b$n[]número$b$m> (ej.: $b$n!vguess 50$b$m - Elige un número entre el$b$v 0 $b$m[]y el$b$v 99$b$m)."
		bgusage $chan $nick 2
	} elseif { ![string is int $text] } {
		# El número tiene que ser un entero.
		putnotc $nick "$m-> El número $b$n[]Vguess$b$m debe ser una combinación de dígitos ($v$b[]0$b$m...$v$b[]9$b$m).$b$n $text$b$m no es válido."
	} elseif { $text < $bglow_num || $text > $bghigh_num } {
		# Tiene que ser un número entre $bglow_num y $bghigh_num.
		putnotc $nick "$m-> El número $b$n[]Vguess $b$m[]debe ser un número entre el$b$v $bglow_num $b$m[]y el$b$v $bghigh_num$b$m."
	} elseif { [expr {[unixtime] - $t}] < $bguess(period) } {
		# 1 intento cada $bguess(period) segundos.
		putnotc $nick "$m-> Sólo un intento cada$b$az [expr {$bguess(period) / 60}] $b$m[]minutos o bien hasta que otro jugador lo intente también. :)"
		# Actualiza el temporizador - El jugador no puede probar de nuevo hasta pasados $bguess(period) segundos.
		set bghosts($uhost) [unixtime]
	} else {
		# Si hemos llegado aqui, el intento es válido, incrementamos el contador de intentos.
		incr bguess(intentos) 1
		# Limpiamos el tiempo entre intentos
		unset -nocomplain bghosts
		global bghosts
		# ¿Cómo es la respuesta?
		if {$text == $bguess(target)} {
			# La respuesta es correcta.
			set puntos [expr {$bguess(high) - $bguess(low) + 1}]
			set mensaje "-> $b$nick$b - El $b[]$text$b es el correcto! ($b[]$bguess(intentos)$b intentos en juego Nº $b[]$bguess(game)$b - $b[]$puntos$b puntos obtenidos). Llevas un total de$b [expr {[bgpuntos $nick] + $puntos}]$b ganados. ¿Puedes acertar el siguiente a la primera? :P"
			if {[isvoice $nick $chan]} {
				putact $chan "$mensaje"
			} else {
				putact $chan "$mensaje$l$a - Tómate una rubia!!"
				putchan $chan "!beer $nick"
				puthelp "MODE $chan +v $nick"
			}
			if {[expr {$bguess(game) % 1000}] == 0} {
				putact $chan "$m-> $b$n$nick$b $m- Premio especial por el juego número$b$v $bguess(game)$b$l."
				puthelp "MODE $chan +h $nick"
			}
			if {$puntos == 100} {
				putact $chan "$m-> $b$n$nick$b $m- Anda, Anda!! No te lo crees ni tú. Tienes más suerte que Tarzán encontrando lianas... Ahí va tu premio..."
				utimer 2 [list puthelp "MODE $chan +o $nick"]
			}
			player_stats_update $nick 1 1 $puntos
#			file copy -force -- $bgstats(file) $bgstats(web)
			target_stats_update $text 1
#			file copy -force -- $bgtarget(file) $bgtarget(web)
			if {$puntos >= $bguess(max_one_points)} {
				set bguess(max_one_winner) $nick
				set bguess(max_one_points) $puntos
				set bguess(max_one_game) $bguess(game)
				set bguess(max_one_time) [unixtime]
			}
			bgcan $nick $chan
			bgnext $nick true
		} else {
			if {$text > $bguess(target)} {
				# El intento fue demasiado alto.
				set intento "alto"
				if { $text <= $bguess(high) } {
					set bguess(high) [expr $text - 1]
				}
			} else {
				# El intento fue demasiado bajo.
				set intento "bajo"
				if { $text >= $bguess(low) } {
					set bguess(low) [expr $text + 1]
				}
			}
			putact $chan "-> $b$nick$b - $b$text$b es $intento\
				($b$bguess(intentos)$b intentos en juego Nº $b$bguess(game)$b -\
				El último ganador es$b$v [rr1_coder $bguess(last_winner)]$b$l)."
			player_stats_update $nick 1
			target_stats_update $text 0
			check_duck $chan $nick $bguess(high) $bguess(low)
			# Actualiza el timer para este usuario.
			set bghosts($uhost) [unixtime]
		}
		# Guarda el timer.
		bguess_save
	}
}

#-------------------------------------------------------------------------------
# Elimina estadisticas de jugadores inactivos y puntuaciones máximas.
#-------------------------------------------------------------------------------

proc bguess_cleaning {m h dias args} {
	global bguess bgstats
	if {[expr {$dias % 10}] == 0} {
		set cambios 0
		set fechalimite [expr {[unixtime] - $bguess(preservetime)}]
		for {set i [expr {$bgstats(count) - 1}]} {$i >= 0} {decr i} {
			if {[lindex $bgstats(records) $i 4] < $fechalimite} {
				set bgstats(records) [lreplace $bgstats(records) $i $i]
				set cambios 1
			}
		}
		if {$cambios} {
			set bgstats(count) [llength $bgstats(records)]
			bgstats_save
		}
	}
	if {$bguess(max_one_time) < [expr {[unixtime] - $bguess(cleartime)}]} {
		set bguess(max_one_winner) "Nadie :o)"
		set bguess(max_one_points) 0
		set bguess(max_one_game) 0
		set bguess(max_one_time) [unixtime]
		bguess_save
	}
}

#-------------------------------------------------------------------------------
# Init
#-------------------------------------------------------------------------------

bguess_load
bgstats_load
bgtarget_load

#-------------------------------------------------------------------------------
# Log
#-------------------------------------------------------------------------------

putlog "$b$m[]Juego $n[]Vguess$m Cargado - Versión$n $bgversion $rj[]€$n[]lite$m."
