
module Manager_Aux ( Mensaje,
                    Direccion,
                    ObjetoNombre,
                    Comando(..),
                    comandos,
                    Lugar(..),
                    lugares,
                    buscar_lugar_seguro,
                    aeropuertos,
                    es_aeropuerto,
                    Objeto(..),
                    objetos,
                    buscar_jugador_seguro,
                    buscar_sponsor_seguro,
                    cantidad_jugadores_mi_equipo,
                    ver_nivel_mi_equipo,
                    Ubicaciones,
                    ubicaciones,
                    hay_ruta,
                    obtener_rutas,
                    ver_equipo_cmd,
                    ver_nivel_cmd,
                    ver_sponsors_cmd,
                    jugadores_del_equipo, 
                    cantidad_sponsors,
                    ver_vuelos'
                    ) where

import Datos
import  qualified Data.List as Dlist
import  qualified Data.Char as Dchar

-- Busca el comando representatvo, recibe comando buscado, lista de comandos
determinar_comando:: String -> [Comando] -> String
determinar_comando _ [] = "Nada"
determinar_comando cmd (x:xs) | elem cmd (opciones x) = clave x
                              | otherwise = determinar_comando cmd xs


buscar_lugar_seguro::String -> [Lugar] -> Lugar
buscar_lugar_seguro _ [] = Plaza {nombre="", rutas=[]}
buscar_lugar_seguro lugar (x:xs) | lugar==(nombre x) = x
                          | otherwise = buscar_lugar_seguro lugar xs

buscar_lugar:: String -> [Lugar] -> Maybe Lugar
buscar_lugar _ [] = Nothing
buscar_lugar lugar (x:xs) | lugar==(nombre x) = Just x
                          | otherwise = buscar_lugar lugar xs

hay_ruta':: Lugar -> Direccion -> Bool
hay_ruta' plaza d = case Dlist.find (\(a,b)-> d==a) (rutas plaza) of
                     Just result -> True
                     Nothing -> False

hay_ruta:: String -> Direccion -> [Lugar] -> Bool
hay_ruta n_lugar d lugares = 
    let lugar = buscar_lugar_seguro n_lugar lugares
        in hay_ruta' lugar d

es_aeropuerto:: String -> Bool
es_aeropuerto plaza = elem plaza aeropuertos

es_aeropuerto':: Lugar -> Bool
es_aeropuerto' (Aeropuerto nombre rutas vuelos) = True
es_aeropuerto' _ = False

ver_vuelos:: Lugar -> [AeropuertoDestino]
ver_vuelos aeropuerto = vuelos aeropuerto

ver_vuelos':: Lugar -> Mensaje
ver_vuelos' aeropuerto = 
            let mensaje = ["Tenemos vuelos a:"] ++ (vuelos aeropuerto)
            in Dlist.intercalate "\n" mensaje 

buscar_objeto:: String -> [Objeto] -> Maybe Objeto
buscar_objeto _ [] = Nothing
buscar_objeto objeto (x:xs) | objeto == (nombre_o x) = Just x
                            | otherwise = buscar_objeto objeto xs

buscar_jugador_seguro::String -> [Objeto] -> Objeto
buscar_jugador_seguro _ [] = Jugador {nombre_o="", valor=0, nivel=0}
buscar_jugador_seguro jugador (x:xs) | jugador==(nombre_o x) = x
                          | otherwise = buscar_jugador_seguro jugador xs

buscar_sponsor_seguro::String -> [Objeto] -> Objeto
buscar_sponsor_seguro _ [] = Sponsor {nombre_o="", ingreso=0}
buscar_sponsor_seguro sponsor (x:xs) | sponsor==(nombre_o x) = x
                          | otherwise = buscar_sponsor_seguro sponsor xs



ver_equipo_cmd :: Ubicaciones ->[Objeto] -> Mensaje
ver_equipo_cmd ubicaciones objetos = 
    let jugadores = [jugador | (jugador, "Mi Equipo") <- ubicaciones]
        mi_equipo = [(show_jugador jg_obj) | jg_obj <- objetos, elem (nombre_o jg_obj) jugadores]
        in if mi_equipo == []
            then "No tienes jugadores en el equipo."
            else Dlist.intercalate "\n" mi_equipo

show_jugador:: Objeto -> String
show_jugador (Jugador nombre valor nivel) = nombre ++ "  valor:" ++ show valor ++ "  nivel:" ++ show nivel 
show_jugador _ = "Nada"

ver_equipo' :: Ubicaciones ->[Objeto] -> [Objeto]
ver_equipo' ubicaciones objetos = 
    let jugadores = [jugador | (jugador, "Mi Equipo") <- ubicaciones]
        mi_equipo = [jg_obj | jg_obj <- objetos, elem (nombre_o jg_obj) jugadores]
        in mi_equipo

cantidad_jugadores_mi_equipo::Ubicaciones -> [Objeto] -> Int
cantidad_jugadores_mi_equipo ubicaciones objetos = length (ver_equipo' ubicaciones objetos)

jugadores_del_equipo::String -> Ubicaciones -> [Objeto] -> Mensaje
jugadores_del_equipo n_lugar ubicaciones objetos = 
    let jugadores = [jugador | (jugador, equipo) <- ubicaciones,equipo==n_lugar, (Dlist.isSuffixOf "manager" jugador)==False]
        equipo = [show_jugador jg_obj | jg_obj <- objetos, elem (nombre_o jg_obj) jugadores]
        in if equipo == []
            then "No tienemos jugadores en venta."
            else Dlist.intercalate "\n" (["En venta:"] ++ equipo)

ver_nivel_cmd:: Ubicaciones -> [Objeto] -> Mensaje
ver_nivel_cmd ubicaciones objetos =
    let mi_equipo = ver_equipo' ubicaciones objetos
        mis_niveles = [(nivel jug) | jug <- mi_equipo]
        in if mis_niveles == []
            then "tu nivel actual es: 0"
            else "tu nivel actual es: " ++ show ( fromIntegral(sum mis_niveles) / fromIntegral(length mis_niveles))

ver_nivel_mi_equipo:: Ubicaciones -> [Objeto] -> Float
ver_nivel_mi_equipo ubicaciones objetos =
    let mi_equipo = ver_equipo' ubicaciones objetos
        mis_niveles = [(nivel jug) | jug <- mi_equipo]
        in if mis_niveles == []
            then 0
            else fromIntegral(sum mis_niveles) / fromIntegral(length mis_niveles)

ver_sponsors_cmd:: Ubicaciones -> [Objeto] -> Mensaje
ver_sponsors_cmd ubicaciones objetos = 
    let sponsors = [sponsor | (sponsor, "Mis Sponsors") <- ubicaciones]
        mis_sponsors = [(show_sponsor sp_obj) | sp_obj <- objetos, elem (nombre_o sp_obj) sponsors]
        in if mis_sponsors == []
            then "No tienes contrato con sponsors actualmente."
            else Dlist.intercalate "\n" mis_sponsors

ver_sponsors':: Ubicaciones -> [Objeto] -> [Objeto]
ver_sponsors' ubicaciones objetos =
    let sponsors = [sponsor | (sponsor, "Mis Sponsors") <- ubicaciones]
        mis_sponsors = [sp_obj | sp_obj <- objetos, elem (nombre_o sp_obj) sponsors]
        in mis_sponsors

show_sponsor:: Objeto -> String
show_sponsor (Sponsor nombre ingreso) = nombre ++ "  ingreso:" ++ show ingreso

cantidad_sponsors:: Ubicaciones -> [Objeto] -> Int
cantidad_sponsors ubicaciones objetos = length (ver_sponsors' ubicaciones objetos)

obtener_rutas:: String -> [Lugar] -> Mensaje
obtener_rutas n_lugar lugares = 
    let lugar =  buscar_lugar_seguro n_lugar lugares
        rutas =  ["Rutas:"] ++ [f++":"++s | (f,s) <- rutas' lugar]
        in Dlist.intercalate "\n" rutas

rutas'::Lugar -> [(String,String)]
rutas' (Plaza _ rutas) = rutas
rutas' (Aeropuerto _ rutas _) = rutas