module Exerciciosfinais where

-- 1
type Nome = String
type Preco = Float
type Categoria = String
type Produto = (Nome, Preco, Categoria)

filtraProdutos :: [Produto] -> Preco -> [(Nome, Preco)]
filtraProdutos lista limite =
    [(nome, preco) | (nome, preco, categoria) <- lista, preco > limite, categoria == "eletronico" || categoria == "informatica"]

--2
type Titulo = String
type Nota = Float
type Genero = String
type Tupla = (Titulo, Nota, Genero)

filtraFilmes :: [Tupla] -> [(Titulo, Nota)]
filtraFilmes lista = 
    [(titulo, nota) | (titulo, nota, genero) <- lista, nota >= 8, genero == "terror" || genero == "sci-fi"]

--3
type Nome = String
type Nota = Float
type Curso = String
type Alunos = (Nome, Nota, Curso)

filtraAprovados :: [Alunos] -> [(Nome)]
filtraAprovados lista =
    [(nome) | (nome, nota, curso) <- lista, nota >= 7, curso == "computacao"]

--4
type Nome = String
type Salario = Float
type Departamento = String
type Funcionario = (Nome, Salario, Departamento)

filtraFuncionarios :: [Funcionario] -> Float -> [(Nome, Salario)]
filtraFuncionarios lista limite =
    [(nome, salario) | (nome, salario, departamento) <- lista, salario > limite, departamento == "RH" || departamento == "TI"]

--5
type Destino = String
type Preco = Float
type Tipo = String
type Pacote = (Destino, Preco, Tipo)

filtraPacotes :: [Pacote] -> [(Destino)]
filtraPacotes lista =
    [(destino) | (destino, preco, tipo) <- lista, preco >= 1500 && preco <= 3000, tipo == "internacional"]


--6
type Modelo = String
type Km = Float
type Tipo = String
type Veiculo = (Modelo, Km, Tipo)

filtraVeiculos :: [Veiculo] -> [String]
filtraVeiculos lista =
    [ modelo | (modelo, km,  tipo) <- lista, km < 50000, tipo == "carro" || tipo == "SUV"]

--7
type Nome = String
type Nota = Float
type Tipo = String
type Restaurante = (Nome, Nota, Tipo)

filtraRestaurantes :: [Restaurante] -> Float -> [(Nome, Nota)]
filtraRestaurantes lista valorMin =
    [(nome, nota) | (nome, nota, tipo) <- lista, nota >= valorMin, tipo == "pizza" || tipo == "churrascaria"]

--8
type Item = String
type Peso = Float
type Material = String
type Objeto = (Item, Peso, Material)

filtraObjetos :: [Objeto] -> Float -> [Item]
filtraObjetos lista limite =
    [(item) | (item, peso, material) <- lista, peso > limite, material == "metal"]

--9
type Nome = String
type Nota = Int
type Genero = String
type Jogo = (Nome, Nota, Genero)

filtraJogos :: [Jogo] -> [Nome]
filtraJogos lista =
    [nome | (nome, nota, genero) <- lista, nota >= 85, genero == "acao" || genero == "RPG"]


--10
type Curso = (Nome, Int, String)

filtraCursos :: [Curso] -> [Nome]
filtraCursos lista =
    [nome | (nome, carga, nivel) <- lista, carga >= 40, nivel == "avancado"]
    



