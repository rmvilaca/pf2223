
data Hora = H Int Int
          deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

v :: Viagem
v = [ (H 9 30, H 10 25)
    , (H 11 20, H 12 45)
    , (H 13 30, H 14 45)
    ]

horaValida :: Hora -> Bool
horaValida (H h m) = h >= 0 && h < 24 && m >= 0 && m < 60 

--true se a 1 é maior q a 2.
maiorHora :: Hora -> Hora -> Bool
maiorHora (H h1 m1) (H h2 m2) | h1 > h2              = True 
                              | h1 == h2 && m1 > m2  = True
                              | otherwise            = False

etapaValida :: Etapa -> Bool
etapaValida (hi, hf) = horaValida hi && horaValida hf
                     && maiorHora hf hi 

horaParaMinutos :: Hora -> Int
horaParaMinutos (H h m) = h * 60 + m 

-- tempoEmEspera :: Viagem -> Int 
-- tempoEmEspera [] = 0 
-- tempoEmEspera [e] = 0 
-- tempoEmEspera (e:e1:es) = 
--         horaParaMinutos (fst e1) - (snd e) + tempoEmEspera (e1:es) 

--5

data Movimento = Credito Float 
               | Debito Float
               deriving Show
data Data = D Int Int Int
          deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

outubro :: Extracto 
outubro = Ext 120
            [ (D 14 10 2022, "MB", Debito 10)
            , (D 13 10 2022, "EDP", Debito 30)
            , (D 1 10 2022, "Salario", Credito 500)
            ]            

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ mvs) v = movimentosSuperiores mvs v 

movimentosSuperiores :: [(Data,String,Movimento)] -> Float -> [Movimento]
movimentosSuperiores [] _ = []
movimentosSuperiores ((_,_,Credito x):t) v 
            | x > v = Credito x : movimentosSuperiores t v
            | otherwise = movimentosSuperiores t v  
movimentosSuperiores ((_,_,Debito x):t) v 
            | x > v = Debito x : movimentosSuperiores t v
            | otherwise = movimentosSuperiores t v  


filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ mvs ) descs = filtroMovimentos mvs descs

filtroMovimentos :: [(Data,String,Movimento)] -> [String] -> [(Data,Movimento)]
filtroMovimentos [] _ = [] 
filtroMovimentos ((d,desc,m):t) descs
            | elem desc descs = (d,m) : filtroMovimentos t descs 
            |otherwise        = filtroMovimentos t descs 

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ mvs) = credDebMovimentos mvs

credDebMovimentos :: [(Data,String,Movimento)] -> (Float,Float)
credDebMovimentos [] = (0,0)
credDebMovimentos ((_,_, Credito x):mvs) = (x+cds,dbs)
        where (cds,dbs) = credDebMovimentos mvs 
credDebMovimentos ((_,_, Debito  x):mvs) =  (cds,x+dbs)
        where (cds,dbs) = credDebMovimentos mvs 

