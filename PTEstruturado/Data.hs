module PTEstruturado.Data where 

{-
  modulo que contem a definicação das estruturas da Linguagem de programação 
-}

data Variavel = Variavel {nome::String, tipo::Tipo} deriving (Show)

data Tipo = Inteiro
          | Fracionario
          | Logico
          deriving (Eq)
          
instance Show Tipo where
  show Inteiro     = "int"
  show Fracionario = "real"
  show Logico      = "logico"


data Expr = Arit ExpArit
          | Logica ExpLogica
          deriving (Show)

data ExpLogica = VarLogica String
               | ConsLogica Bool
               | Negacao ExpLogica
               | LogicoBin OpLogico ExpLogica ExpLogica 
               | RelacianalBin OpRelacional ExpArit ExpArit
               deriving (Show)
              
data OpLogico = E | Ou deriving (Show) 

data OpRelacional = Maior
                  | MaiorIgual
                  | Menor
                  | MenorIgual
                  | Igual
                  | Diferente
                  deriving (Show)

data ExpArit = VarArit String
             | ConsArit (Either Integer Double)
             | Neg ExpArit
             | AritBin OpArit ExpArit ExpArit
             deriving (Show)
             
data OpArit = Soma
            | Subt
            | Divi
            | Mult
            | Rest
            deriving (Show)
            
data Instr = Seq [Instr]
           | Atrib String Expr
           | Se ExpLogica Instr Instr
           | Enquanto ExpLogica Instr
           | Escreva Expr
           | Ler String
           deriving (Show)
           
data Algoritimo = Algoritimo String [Variavel] Instr
                deriving (Show)
