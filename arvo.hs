data ArvBin a = Null
     | No a (ArvBin a) (ArvBin a)


elementoPreordem :: ArvBin a -> [a]
elementoPreordem Null = []
elementoPreordem (No x esq dir) = [x] ++ (elementoPreordem esq) ++ (elementoPreordem dir)

elementoOrdem :: ArvBin a -> [a]
elementoOrdem Null = []
elementoOrdem (No x esq dir) =  (elementoOrdem esq) ++ [x] ++(elementoOrdem  dir)

elementoPosordem :: ArvBin a -> [a]
elementoPosordem Null = []
elementoPosordem (No x esq dir) =  (elementoPosordem esq)  ++(elementoPosordem  dir)++ [x]

verificaAltura :: ArvBin a -> Int
verificaAltura Null = 0
verificaAltura (No x esq dir) = 1 + max  (verificaAltura esq) (verificaAltura dir)

balanciada :: ArvBin a -> Bool
balanciada Null = True
balanciada (No x esq dir) = ( abs ((verificaAltura esq)-(verificaAltura dir))) <= 1 && (balanciada esq) && (balanciada dir)


somaA Null = 0
somaA (No x esq dir) = x + (somaA esq) + (somaA dir)


multiplicaA Null = 1
multiplicaA (No x esq dir) = x * (multiplicaA esq) * (multiplicaA dir)



arvg = (No 10 (No 5 (No 2 Null Null)(No 7 Null Null))(No 15 (No 14 Null Null)(No 19 Null Null)))

arvb = (No 2 (No 1 Null Null) (No 3 Null Null))



busca f (No x esq dir) = f `elem` (elementoPreordem (No x esq dir))