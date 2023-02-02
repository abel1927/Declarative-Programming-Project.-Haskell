import Datos
import Manager_Aux
import Descripciones

import  qualified Data.List as Dlist
import  qualified Data.Char as Dchar


type Presupuesto = Int
type Operaciones = Int

type Partida = (Ubicaciones, Presupuesto, Operaciones, Mensaje)

main :: IO (String)
main = do
    putStrLn "\nComienza tu aventura Manager!\n"
    putStrLn instrucciones
    jugar $ return (ubicaciones, 20000, 15, "")
    return "Adios!! Esperamos que hayas disfrutado."


instrucciones =
    "Para el desarrollo del juego utilice los comandos que se habilitan\n" ++
    "Comandos:\n" ++
    "ayuda                  -- para mostrar los comandos disponibles.\n" ++
    "n  s  e  o             -- para moverse en las direcciones.\n" ++
    "viajar <destino>       -- para viajar al aeropuerto de destino.\n" ++
    "vuelos                 -- para ver los destinos del aeropuerto actual.\n" ++
    "firmar <sponsor>       -- para firmar con el sponsor seleccionado.\n" ++
    "despedir <jugador>     -- para despedir a tu jugador seleccionado.\n" ++
    "vender <jugador>       -- para vender a tu jugador seleccionado.\n" ++
    "negociar               -- para ver los jugadores que otros manager tengan en venta.\n" ++
    "contratar <jugador>    -- para contratar al jugador seleccionado.\n" ++
    "escenario              -- Para observar donde te encuentras.\n" ++
    "equipo                 -- para ver tu equipo.\n" ++
    "nivel                  -- para saber el nivel actual del equipo.\n" ++
    "presupuesto            -- para saber tu presupuesto actual.\n" ++
    "operaciones            -- para saber tus operaciones restantes.\n" ++
    "sponsors               -- para ver tus patrocinadores.\n" ++  
    "completar              -- para presentarle tu equipo al director\n"++
    "salir                  -- para finalizar el juego."

jugar :: IO (Partida) -> IO (Partida)
jugar partida = do
    (ubicaciones, presupuesto, operaciones, mensaje) <- partida
    putStrLn mensaje
    putStrLn ""
    if fin operaciones 
        then do
            putStrLn $ escena_final (ver_nivel_mi_equipo ubicaciones objetos) (cantidad_jugadores_mi_equipo ubicaciones objetos) ubicaciones
            getLine
            return ([], -1, -1,"") 
        else do
            putStr "> "
            comando <- getLine
            if comando == "salir"
                then return (ubicaciones, presupuesto, operaciones, "Saliendo.")
                else  jugar $ return (ejecutar_comando comando ubicaciones presupuesto operaciones)

fin:: Int -> Bool
fin op = op == 0

escena_final::Float -> Int -> Ubicaciones -> Mensaje
escena_final nivel cant_jug ub| nivel > 4.2 && cant_jug >= 7 = 
    "Lo has logrado, has armado un equipo potente, equilibrado, \n" ++
    "con sus estrellas y sus guerraros. Nos espera una gran temporada gracias a ti!!!\n\n"++
    "Tu equipo\n"++
    ver_equipo_cmd ub objetos
                            | otherwise = 
    "Recibes una llamada de tu madre: Tengo una carta de despido a tu nombre\n" ++
    " como puede ser!!!!. Escuchas a tu madre llorar, es un momento triste\n" ++
    " las espectativas eran altas, hiciste lo que pudiste. La proxima sera mejor"


ejecutar_comando:: String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
ejecutar_comando "n" ubicaciones p op = 
    let cambio = movimiento_cmd "n" ubicaciones
    in (fst cambio, p, op, snd cambio)
ejecutar_comando "s" ubicaciones p op = 
    let cambio = movimiento_cmd "s" ubicaciones
    in (fst cambio, p, op, snd cambio)
ejecutar_comando "e" ubicaciones p op = 
    let cambio = movimiento_cmd "e" ubicaciones
    in (fst cambio, p, op, snd cambio)
ejecutar_comando "o" ubicaciones p op = 
    let cambio = movimiento_cmd "o" ubicaciones
    in (fst cambio, p, op, snd cambio)
ejecutar_comando "ayuda" ubicaciones p op = (ubicaciones, p, op, instrucciones)
ejecutar_comando "escenario" ubicaciones p op = (ubicaciones, p, op, (escenario_cmd ubicaciones))
ejecutar_comando "equipo" ub p op = (ub, p, op, (ver_equipo_cmd ub objetos))
ejecutar_comando "nivel" ub p op = (ub, p, op, (ver_nivel_cmd ub objetos))
ejecutar_comando "presupuesto" ub p op = (ub, p, op, "Tu presupuesto actual es: " ++ show p)
ejecutar_comando "operaciones" ub p op = (ub, p, op, "Te quedan " ++ show op ++ " operaciones")
ejecutar_comando "completar" ub p op = completar_cmd ub p op
ejecutar_comando "sponsors" ub p op = (ub, p, op, (ver_sponsors_cmd ub objetos))
ejecutar_comando "negociar" ub p op = (ub, p, op, negociar_cmd ub)
ejecutar_comando "vuelos" ub p op = (ub, p, op, ver_vuelos_cmd ub)
ejecutar_comando cmd ubicaciones p op = ejecutar_comando' cmd ubicaciones p op

ejecutar_comando'::String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
ejecutar_comando' cmd ubicaciones p op
          | Dlist.isPrefixOf "contratar" cmd = contratar_cmd (Dlist.tail $ snd $ Dlist.span Dchar.isLetter cmd) ubicaciones p op
          | Dlist.isPrefixOf "despedir" cmd = despedir_cmd (Dlist.tail $ snd $ Dlist.span Dchar.isLetter cmd) ubicaciones p op
          | Dlist.isPrefixOf "vender" cmd = vender_cmd (Dlist.tail $ snd $ Dlist.span Dchar.isLetter cmd) ubicaciones p op
          | Dlist.isPrefixOf "firmar" cmd = firmar_cmd (Dlist.tail $ snd $ Dlist.span Dchar.isLetter cmd) ubicaciones p op
          | Dlist.isPrefixOf "viajar" cmd = viajar_cmd (Dlist.tail $ snd $ Dlist.span Dchar.isLetter cmd) ubicaciones p op
          | otherwise = (ubicaciones, p, op, "No entiendo el comando")

movimiento_cmd:: Direccion -> Ubicaciones -> (Ubicaciones,Mensaje)
movimiento_cmd d ubicaciones = do
    let pos_actual = buscar_por_llave "Manager" ubicaciones
    if hay_ruta pos_actual d lugares
        then do
            let mi_lugar = buscar_lugar_seguro pos_actual lugares
            let nueva_pos = buscar_por_llave d (rutas mi_lugar)
            let ubicaciones_act = actualizar_ubicaciones "Manager" nueva_pos ubicaciones
            let mensaje = describir_escenario nueva_pos
            (ubicaciones_act, mensaje)
        else (ubicaciones,"No hay camino por esa ruta")

escenario_cmd:: Ubicaciones -> Mensaje
escenario_cmd ubicaciones = 
    let mi_lugar = buscar_por_llave "Manager" ubicaciones
    in describir_escenario mi_lugar

negociar_cmd:: Ubicaciones -> Mensaje
negociar_cmd ubicaciones = 
    let mi_lugar = buscar_por_llave "Manager" ubicaciones
    in if mi_lugar /= "Mi club oficina manager" && Dlist.isSuffixOf "oficina manager" mi_lugar
       then jugadores_del_equipo mi_lugar ubicaciones objetos
       else "No hay nada que negociar aqui"

contratar_cmd:: String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
contratar_cmd n_jugador ub p op
     | n_jugador == "Ronaldo" = contratar_especial "Ronaldo" ub p op 
     | n_jugador == "Mbappe" = contratar_especial "Mbappe" ub p op
     | n_jugador == "Messi" = contratar_especial "Messi" ub p op
     | otherwise = contratar_normal n_jugador ub p op 

contratar_normal:: String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
contratar_normal n_jugador ub p op = 
    let mi_lugar = buscar_por_llave "Manager" ub
        jug_lugar = buscar_por_llave n_jugador ub
        jug_obj = buscar_jugador_seguro n_jugador objetos
        in if mi_lugar == jug_lugar
            then if p >= (valor jug_obj)
                 then ((actualizar_ubicaciones n_jugador "Mi Equipo" ub), p - valor jug_obj, op -1, "Fichado!!")
                 else (ub,p,op,"No tienes suficiente presupuesto para ficharlo. Se aprieta el mercado!!!")
            else if jug_lugar == "Mi Equipo"
                 then (ub,p,op,"Este jugador ya esta en tu equipo. Centrate, Vamos!!!")
                 else (ub,p,op,"No esta este jugador aqui.")

contratar_especial::String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
contratar_especial "Mbappe" ub p op  =
    let mi_lugar = buscar_por_llave "Manager" ub
        jug_lugar = buscar_por_llave "Mbappe" ub
        jug_obj = buscar_jugador_seguro "Mbappe" objetos
        c_sponsors = cantidad_sponsors ub objetos
        nike = buscar_por_llave "Nike" ub == "Mis Sponsors"
        in if mi_lugar == jug_lugar
           then if c_sponsors > 6
                then if nike
                     then if p >= (valor jug_obj)
                          then ((actualizar_ubicaciones "Mbappe" "Mi Equipo" ub), p - valor jug_obj, op -1, "Fichado!!")
                          else (ub,p,op,"No tienes suficiente presupuesto para ficharlo. Se aprieta el mercado!!!")
                     else (ub,p,op,"No puedo separarme de mi marca.\nSabias que soy imagen de Nike?!!!")
                else (ub,p,op,"Tu equipo aun tiene poco cache. Piensa en asociarte con nuevas marcas")
           else if jug_lugar == "Mi Equipo"
                 then (ub,p,op,"Este jugador ya esta en tu equipo. Centrate, Vamos!!!")
                 else (ub,p,op,"No esta este jugador aqui.")
contratar_especial "Ronaldo" ub p op  =
    let mi_lugar = buscar_por_llave "Manager" ub
        jug_lugar = buscar_por_llave "Ronaldo" ub
        jug_obj = buscar_jugador_seguro "Ronaldo" objetos
        c_sponsors = cantidad_sponsors ub objetos
        nike = buscar_por_llave "Nike" ub == "Mis Sponsors"
        in if mi_lugar == jug_lugar
           then if c_sponsors == 8
                then ((actualizar_ubicaciones "Ronaldo" "Mi Equipo" ub), p - valor jug_obj, op -1, "Fichado el Bicho!!")
                else (ub,p,op,"Domina el mercado de las marcas, el Rey espera. Debemos mejorar el marketing")
           else if jug_lugar == "Mi Equipo"
                 then (ub,p,op,"Este jugador ya esta en tu equipo. Centrate, Vamos!!!")
                 else (ub,p,op,"No esta este jugador aqui.")
contratar_especial "Messi" ub p op  =
    let mi_lugar = buscar_por_llave "Manager" ub
        jug_lugar = buscar_por_llave "Messi" ub
        jug_obj = buscar_jugador_seguro "Messi" objetos
        c_sponsors = cantidad_sponsors ub objetos
        adidas = buscar_por_llave "Adidas" ub == "Mis Sponsors"
        in if mi_lugar == jug_lugar
           then if c_sponsors > 4
                then if adidas
                     then if p >= (valor jug_obj)
                          then ((actualizar_ubicaciones "Messi" "Mi Equipo" ub), p - valor jug_obj, op -1, "Fichado Lio!!")
                          else (ub,p,op,"No tienes suficiente presupuesto para ficharlo. Se aprieta el mercado!!!")
                     else (ub,p,op,"No puedo separarme de mi marca.\nSabias que soy imagen de Adidas?!!!")
                else (ub,p,op,"Tu equipo aun tiene poco cache. Piensa en asociarte con nuevas marcas")
           else if jug_lugar == "Mi Equipo"
                 then (ub,p,op,"Este jugador ya esta en tu equipo. Centrate, Vamos!!!")
                 else (ub,p,op,"No esta este jugador aqui.")

despedir_cmd::String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
despedir_cmd n_jugador ub p op =
    let jug_lugar = buscar_por_llave n_jugador ub
        jug_obj = buscar_jugador_seguro n_jugador objetos
        in if jug_lugar == "Mi Equipo"
           then ((actualizar_ubicaciones n_jugador "Fuera" ub), p, op - 1, "Despedido. No se fue contento!!")
           else (ub,p,op,"No esta en tu equipo.")

vender_cmd::String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
vender_cmd n_jugador ub p op =
    let mi_lugar = buscar_por_llave "Manager" ub
        jug_lugar = buscar_por_llave n_jugador ub
        jug_obj = buscar_jugador_seguro n_jugador objetos
        in if Dlist.isPrefixOf "Mercado" mi_lugar
            then if jug_lugar == "Mi Equipo"
                 then ((actualizar_ubicaciones n_jugador "Fuera" ub), p + valor jug_obj, op -1, "Vendido. Sabes que este ya no vuelve!!")
                 else (ub,p,op,"Pero no tienes a este jugador!!!")
            else (ub,p,op,"Pero como intentas vender aqui. Busca un mercado.")

firmar_cmd::String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
firmar_cmd n_sponsor ub p op =
    let mi_lugar = buscar_por_llave "Manager" ub
        sp_lugar = buscar_por_llave n_sponsor ub
        sp_obj = buscar_sponsor_seguro n_sponsor objetos
        in if mi_lugar == sp_lugar
            then ((actualizar_ubicaciones n_sponsor  "Mis Sponsors" ub), p + ingreso sp_obj, op, "Acabas de firmar un nuevo patrocinio!!")
            else if sp_lugar == "Mis Sponsors"
                 then (ub,p,op,"Este sponsor es tuyo. Centrate, Vamos!!!")
            else (ub,p,op,"Quien es ese sponsor que acabas de inventar?? Centrate. Vamos!!!")

viajar_cmd::String -> Ubicaciones -> Presupuesto -> Operaciones -> Partida
viajar_cmd aer_destino ub p op = 
    let pos_actual = buscar_por_llave "Manager" ub
        aer_lugar = buscar_lugar_seguro pos_actual lugares
        in if es_aeropuerto pos_actual
           then if elem aer_destino (vuelos aer_lugar)
                then ((actualizar_ubicaciones "Manager" aer_destino ub), p, op, describir_escenario aer_destino)
                else (ub, p, op, "No hay viajes a ese destino")
           else (ub, p, op, "Hay que viajar desde los aeropuertos")

buscar_por_llave:: String -> [(String,String)] -> String
buscar_por_llave llave dicc = case Dlist.lookup llave dicc of
                                         Just valor -> valor
                                         Nothing -> "No se encuentra"

actualizar_ubicaciones:: ObjetoNombre -> String -> Ubicaciones -> Ubicaciones
actualizar_ubicaciones objeto posicion ubicaciones =
    let resto = filter (\(x, y) -> x /= objeto) ubicaciones
    in (objeto, posicion) : resto

ver_vuelos_cmd:: Ubicaciones -> Mensaje
ver_vuelos_cmd ubicaciones =
    let pos_actual = buscar_por_llave "Manager" ubicaciones
        aer_lugar = buscar_lugar_seguro pos_actual lugares
        in if es_aeropuerto pos_actual
           then ver_vuelos' aer_lugar
           else "Que fumas! No estamos en un aeropuerto"

completar_cmd:: Ubicaciones -> Presupuesto -> Operaciones -> Partida
completar_cmd ub p op= 
    let pos_actual = buscar_por_llave "Manager" ub
    in if pos_actual == "Mi Club Oficina director"
        then if ver_nivel_mi_equipo ub objetos > 4.2 && cantidad_jugadores_mi_equipo ub objetos >= 7
            then (ub,p,0,"Felicidades, el director esta muy contento con el equipo que has formado y espera grandes\n"++
                "resultados la proxima temporada.\n")
            else (ub,p,op,"Vaya basura de equipo, largate de aqui antes de que el director lo vea")
        else (ub,p,op,"Debes estar en la oficina del director para presentarle tu equipo")



