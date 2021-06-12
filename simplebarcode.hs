import Text.Printf

totalWidth :: Int
totalWidth = 500

totalHeight :: Int
totalHeight = 100

-- Gera string representando um documento SVG completo
-- dadas suas dimensões e uma lista de strings com tags
svgDoc :: Int -> Int -> [String] -> String
svgDoc w h ss = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
  "<svg width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\"" ++
  " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
  concat ss ++ "</svg>\n"
  

-- Retorna String representando uma cor aceita como atributo em uma linha SVG (ver svgLine)
svgColor :: Char -> String
svgColor bit
  | bit == '0'  = "blue"
  | bit == '1'  = "black"
  | otherwise   = error "Invalid character"


-- Gera String representando uma linha em SVG, dados 2 pontos e uma cor
-- A espessura da linha é fixa
-- Veja mais em sobre linhas SVG: https://www.w3schools.com/graphics/svg_line.asp
svgLine :: ((Int,Int),(Int,Int)) -> String -> String
svgLine ((x1,y1),(x2,y2)) color =
  printf "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" style=\"stroke-width:3;stroke:%s\"/>\n" x1 y1 x2 y2 color
  

-- Calcula pontos (x1,y1) e (x2,y2) para representar uma barra "\",
-- dado um valor de x e um intervalo que determina a inclinação da barra.
-- Usa valores fixos para y: a barra sempre começa em 0 e vai até totalHeight
-- Poderia usar let ou where para evitar repetir x*gap e para dar nome às expressões (x1,y1..)
-- No lugar da tupla, poderia ser definido um novo tipo para representar a barra
bar :: Int -> Int -> ((Int,Int),(Int,Int))
bar x gapx = ((x*gapx, 0),(x*gapx+gapx, totalHeight))

flipbar :: Int -> Int -> ((Int,Int),(Int,Int))
flipbar x gapx = ((x*gapx+gapx,0),(x*gapx, totalHeight))

-- Gera uma lista de Strings, cada uma representando uma linha SVG, 
-- com coordenadas calculadas pelas funções acima (bar, flipbar...)
-- O tamanho da lista é dado pela String de entrada, onde cada caracter representa um bit
-- As cores são definidas numa lista separada (cs), de acordo com os bits da String de entrada
-- A função zipWith aplica svgLine a cada par de coordenadas e cores
-- O gap (intervalo) é um valor inteiro para facilitar, mas por isso as barras
-- nem sempre vão preencher bem o espaço da figura (mesmo com arredondamento)
svgBars :: String -> [String]
svgBars bits =
  let len = length bits
      gap = div totalWidth len           -- divisao inteira (não preenche bem o espaço)
      --gap = round (fromIntegral totalWidth / fromIntegral len) -- divisao com arredondamento preenche melhor
      bars = [bar x gap | x <- [0..len]] -- aqui são calculadas todas as coordenadas
      cs = map svgColor bits             -- aqui são definidas as cores conforme os bits
   in zipWith svgLine bars cs            -- aqui as coordenadas são combinadas com as cores para produzir linhas
   
   
-- Gera strings na saída padrão representando um "código de barras" simples
-- Cada barra desenhada representa um bit
-- Para visualizar a imagem, é preciso direcionar a saída para um arquivo
-- Visualizar as strings que compõem o SVG é útil para compreender a saída das funções
main :: IO()
main = do
  let bits = "101111000101010101" 
      stringList = svgBars bits
  putStr (svgDoc totalWidth totalHeight stringList)

