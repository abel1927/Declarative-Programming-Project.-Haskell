module Descripciones(
    describir_escenario
)where
import Datos
import Manager_Aux
import  qualified Data.List as Dlist
import  qualified Data.Char as Dchar

describir_escenario:: String -> Mensaje
describir_escenario lugar | lugar=="River Plate oficina manager" = describir_escenario' "River Plate oficina manager"
                          | lugar=="Club River Plate" = describir_escenario' "Club River Plate"
                          | lugar=="Mi Club Oficina manager" = describir_escenario' "Mi Club Oficina manager"
                          | lugar=="Mi Club Oficina director" = describir_escenario' "Mi Club Oficina director"
                          | lugar=="AeropuertoBrasil" = describir_escenario' "AeropuertoBrasil"
                          | lugar=="Santos oficina manager" = describir_escenario' "Santos oficina manager"
                          | lugar=="Club Real Madrid" = describir_escenario' "Club Real Madrid"
                          | lugar=="Real Madrid oficina manager" = describir_escenario' "Real Madrid oficina manager"
                          | lugar=="AeropuertoChina" = describir_escenario' "AeropuertoChina"
                          | lugar=="AeropuertoInglaterra" = describir_escenario' "AeropuertoInglaterra"
                          | lugar=="Plaza Manchester" = describir_escenario' "Plaza Manchester"
                          | lugar=="AeropuertoColombia" = describir_escenario' "AeropuertoColombia"
                          | lugar=="Playa Brasil" = describir_escenario' "Playa Brasil"
                          | lugar=="Mercado Argentina" = describir_escenario' "Mercado Argentina"
                          | lugar=="Ciudad de Milan" = describir_escenario' "Ciudad de Milan"
                          | lugar=="Plaza Tokio" = describir_escenario' "Plaza Tokio"
                          | lugar == "Ciudad Londres" = describir_escenario' "Ciudad Londres"
                          | Dlist.isSuffixOf "oficina manager" lugar = describir_oficina_manager_estandar lugar
                          | Dlist.isPrefixOf "Mercado" lugar = describir_mercado_estandar lugar
                          | Dlist.isPrefixOf "Aeropuerto" lugar = describir_aeropuerto_estandar lugar
                          | Dlist.isPrefixOf "Club" lugar = describir_club_estandar lugar
                          | otherwise = describir_lugar_estandar lugar
                          
describir_lugar_estandar::String -> Mensaje
describir_lugar_estandar lugar =
    "Estas en "++ lugar ++". Debes seguir adelante en la busqueda de \n"++
    "jugadores para tu equipo.\n\n"++
    obtener_rutas lugar lugares
describir_club_estandar :: String -> Mensaje
describir_club_estandar club=
    "Estas en las instalaciones de "++ club ++ ". Este es un gran club y esperas\n"++
    "encontrar buenos jugadores para unir a tu equipo.\n\n"++
    obtener_rutas club lugares 

describir_mercado_estandar:: String -> Mensaje
describir_mercado_estandar mercado = 
    "Estas en "++ mercado ++". Este es el lugar en donde puedes vender a los\n"++
    "jugadores de tu equipo que ya no necesites y obtener algo de dinero de vuelta por ellos.\n\n"++
    obtener_rutas mercado lugares

describir_aeropuerto_estandar :: String -> Mensaje
describir_aeropuerto_estandar aeropuerto = 
    "Estas en "++ aeropuerto ++". Hay muchos aviones entrando y saliendo constantemente.\n"++
    "Desde aqui puedes viajar a varios paises para continuar tu recorrido.\n\n"++
    obtener_rutas aeropuerto lugares


describir_oficina_manager_estandar:: String -> Mensaje
describir_oficina_manager_estandar oficina = 
    "Estas en " ++ oficina ++ ". Se ve que todo esta ordenado,\n" ++
    "el manager esta frente a ti en el escritorio.\n" ++
    "Toda la sala esta decorada con imagenes del equipo.\n" ++
    "!Cambia la mentalidad estas aqui para <negociar>!!!\n\n" ++
    obtener_rutas oficina lugares

describir_escenario'::String -> Mensaje
describir_escenario' "Mi Club Oficina manager" = 
    "Estas en tu oficina, eres bastante desastre con el orden.\n" ++
    "Tienes el escritorio lleno de documentos.\n" ++
    "En la pared de la derecha tienes tu titulo de manager.\n" ++
    "Te hace recordar a tus padres. Hay que trabajar fuerte ahora \n" ++
    "que llego la oportunidad esperada. !Sal a por los mejores!\n\n" ++
    obtener_rutas "Mi Club Oficina manager" lugares

describir_escenario' "Mi Club Oficina director" = 
    "Estas en la oficina del director, te ha dado un presupuestos y objetivos\n" ++
    "para armar un equipo que este a la altura requerida, sabes que puedes\n" ++
    "lograrlo si te esfuerzas.\n" ++
    "Te ha dicho que debes armar un equipo de mas de 7 jugadores\n" ++
    "y con una media de valoracion por encima de 4.2 . !Sal a por los mejores!\n\n" ++
    obtener_rutas "Mi Club Oficina director" lugares

describir_escenario' lugar   |lugar=="River Plate oficina manager" = "Llegas a las oficinas del River Plate,\n"++
                                                            "Desde niño has apoyado a este club y te has convertido en un gran fanatico.\n"++
                                                            "Tienes en cerca de ti a todos aquellos jugadores con los que te has ilusionado,"++
                                                            "pero no puedes dejar que eso nuble tu juicio, se trata de <negociar> por lo que tienes "++
                                                            "que concentrarte y utilizar la mente y no el corazon\n\n"++
                                                              obtener_rutas "River Plate oficina manager" lugares
                        |lugar=="Club River Plate" = "Estas en las afueras del club de tus sueños,"++
                                                "solo falta acceder a la oficina del manager para empezar a hacer negocios\n\n"++
                                                obtener_rutas "Club River Plate" lugares
                        |lugar=="AeropuertoBrasil" = "Acabas de llegar al Aeropuerto de Brasil, ya hasta el aire es diferente,"++
                                            "se nota que en este pais se respira futbol\n\n"++
                                            obtener_rutas "AeropuertoBrasil" lugares
                        |lugar=="Santos oficina manager"= "Estas en las oficinas del Santos FC, grandes jugadores han salido de este equipo,"++
                                                        "esperas encontrar un fichaje estrella que se ajuste a tu presupuesto.\n\n"++
                                                        obtener_rutas "Santos oficina manager" lugares
                        |lugar=="Club Real Madrid" = "Llegas al Club del Real Madrid, todo se ve espectacular aqui, "++
                                                "se nota de que hay mucho dinero invertido.\n"++
                                                "Hay un gran cartel iluminado con el nombre de <FlyEmirates>, patrocinador del club,\n"++
                                                "el representante de dicha marca se encuentra cerca del cartel, quizas puedas conseguir que "++
                                                "patrocine tu equipo tambien\n\n"++
                                                obtener_rutas "Club Real Madrid" lugares
                        |lugar=="Real Madrid oficina manager"="Mira lo lejos que has llegado, estas en las oficinas del manager del Real Madrid,"++
                                                            "uno de los equipos mas grandes de la historia del futbol. Mientras observas las vitrinas llenas"++
                                                            "de trofeos llega el manager del Club por lo que toca <negociar>\n\n"++
                                                            obtener_rutas "Real Madrid oficina manager" lugares
                        |lugar=="AeropuertoChina" = "Estas en China, recientemente el futbol chino a sido uno de los mas prometedores"++
                                        "esperas encontrar algun talento para unir a tu equipo\n\n"++
                                        "En el aeropuerto ves una tienda <Adidas>, y en el interior se encuentra el representante,"++
                                        "si hablas con el, es posible que consigas un patrocinio"++
                                        obtener_rutas "AeropuertoChina" lugares
                        |lugar=="AeropuertoInglaterra" = "Llegas a Inglaterra, el pais que creo el futbol y de donde han salido muchos de los jugadores"++
                                        "mas importantes de la historia, esperas encontrar"++
                                        obtener_rutas "AeropuertoInglaterra" lugares
                        |lugar=="Plaza Manchester" = "Estas en la plaza pricipal de la ciudad de Manchester, en esta ciudad residen dos"++
                                                    "de los mejores equipos del mundo, y esperas incorporar algunos de sus jugadores a tu plantilla"++
                                                    obtener_rutas "Plaza Manchester" lugares
                        |lugar=="AeropuertoColombia" = "Llegas a Colombia con la esperanza de encontrar alguntalento oculto en la liga de dicho pais.\n"++
                                            "A la salida del aeropuerto te encuentras al Representante de <CocaCola>, esta puede ser una buena oportunidad de patrocinio\n\n"++
                                            obtener_rutas "AeropuertoColombia" lugares
                        |lugar=="Playa Brasil" = "Hay mucho sol y la playa se ve espectacular, hay muchas personas jugando futbol en la arena,"++
                                            "se esta grabando un comercial para <EASport>, quizas pudas hablar con el representante para conseguir un patrocinio\n\n"++
                                            obtener_rutas "Playa Brasil" lugares
                        |lugar=="Mercado Argentina" = "Estas en el Mercado de Venta de Jugadores de Argentina, desde este lugar puedes vender a aquellos jugadores que"++
                                                    "no sean impresindibles en tu equipo. Hay unos establecimientos de la marca <Ford>, seria bueno hablar con el representante"++
                                                    "para conseguir una oportunidad de patrocinio\n\n"++
                                                    obtener_rutas "Mercado Argentina" lugares
                        |lugar=="Ciudad de Milan" = "Una bella ciudad y un bello dia, eso te llena de optimismo en la busqueda de jugadores para armar tu equipo.\n"++
                                                "Mientras caminas por las calles de Milan te encuentras con el representante de <Nike>, la suerte esta de tu lado "++
                                                "deberias hablar con el para conseguir un patrocinio\n\n"++
                                                obtener_rutas "Ciudad de Milan" lugares
                        |lugar=="Plaza Tokio" = "La plaza principal de la ciudad de Tokio, todo es tecnologia en este pais, hay muchas pantallas y luces.\n"++
                                            "En la plaza hay un  en donde se encuentra el representante de <Rakuten>, una de las marcas mas interesadas en el"++
                                            "patrocinio de equipos de futbol, y esperas que puedas conseguir que patrocinen el tuyo\n\n"++
                                            obtener_rutas "Plaza Tokio" lugares
                        |lugar=="Ciudad Londres" = "Estas en la Ciudad de Londres, una ciudad muy futbolera, hay muchas pancartas y carteles apoyando a sus jugadores.\n"++
                                                "Aqui tambien estan las oficinas de <Audi> , es posible que puedas hablar con su representante para conseguir un patrocinio\n\n"++
                                                obtener_rutas "Plaza Tokio" lugares
                                                
