import Text.Printf

{-

Size Bits    Description
11   0-10    Left quiet zone
3    11-13   Left guard bar pattern
42   14-55   Left half character
5    56-60   Center guard bar pattern
42   61-102  Right half character 
3    103-105 Right guard bar pattern
7    106-112 Right quiet zone

Bar height:
Guard bar:     25.40
Character bar: 22.85
Quiet zone:    22.85
-}


totalWidth :: Float
totalWidth = 37.29 -- in mm

totalHeight :: Float
totalHeight = 25.93 -- in mm

totalBars :: Integer
totalBars = 113 -- 11+95+7

lineWidth :: Float
lineWidth = totalWidth / fromInteger totalBars -- 0.33

-- Tabela de esquemas de codificação, segundo o padrão EAN-13
leftHalfCodes :: [String]
leftHalfCodes = ["AAAAAA", "AABABB", "AABBAB", "AABBBA", "ABAABB",
                "ABBAAB", "ABBBAA", "ABABAB", "ABABBA", "ABBABA"]


bitCodes :: Char -> [String]
bitCodes code 
  | code == 'A' = ["0001101", "0011001", "0010011", "0111101", "0100011",
                   "0110001", "0101111", "0111011", "0110111", "0001011"]
  | code == 'B' = ["0100111", "0110011", "0011011", "0100001", "0011101",
                   "0111001", "0000101", "0010001", "0001001", "0010111"]
  | code == 'C' = ["1110010", "1100110", "1101100", "1000010", "1011100",
                   "1001110", "1010000", "1000100", "1001000", "1110100"]
  | otherwise   = []

applyBitCode :: Char -> Int -> String
applyBitCode c d = bitCodes c !! d

-- Exemplo de uso:
-- encodeWith "ABBABA" [7,8,8,5,3,6]
encodeWith :: [Char] -> [Int] -> [String]
encodeWith code digits =
   zipWith applyBitCode code digits

-- Aplica a codificação EAN-13 a uma lista de dígitos
-- O resultado vai ser uma string de 0's e 1's
encodeDigits :: [Int] -> [String]
encodeDigits (first:others) =
  let lcode = leftHalfCodes !! first
      rcode = "CCCCCC"
      lrguard = ["101"]
      cguard = ["01010"]
      lquiet = [replicate 11 '0']
      rquiet = [replicate 7 '0']
      (left,right) = splitAt 6 others
   in lquiet ++ 
      lrguard ++ 
      (encodeWith lcode left) ++ 
      cguard ++ 
      (encodeWith rcode right) ++ 
      lrguard ++ 
      rquiet

-- Algumas barras têm altura maior
barHeight :: Int -> Float
barHeight pos
  | pos >= 11  && pos <= 13  = 25.40 -- left guard bar
  | pos >= 56  && pos <= 60  = 25.40 -- center guard bar
  | pos >= 103 && pos <= 105 = 25.40 -- right guard bar
  | otherwise                = 22.85


svgColor :: Char -> String
svgColor bit
  | bit == '0'  = "white"
  | bit == '1'  = "black"
  | otherwise   = error "Invalid character"

-- Produz um retângulo representando uma barra no padrão EAN-13
-- Espessura hard-coded, mas poderia usar uma constante
svgBar :: Float -> Float -> String -> String
svgBar x height color =
  printf "<rect x=\"%.3f\" y=\"0\" width=\"0.33\" height=\"%.2f\" style=\"stroke-width:0;fill:%s\"/>\n" x height color

-- Produz uma figura SVG dentro do tamanho/proporção definido pelo padrão EAN-13
-- Valores hard-code podem ser substituídos por constantes
svgDoc :: [String] -> String
svgDoc ss = 
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
  "<svg width=\"37.29mm\" height=\"25.93mm\" viewBox=\"0 0 37.29 25.93\"" ++
  " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
  concat ss ++ "</svg>\n"

main :: IO()
main = do
  let code = [9,7,8,8,5,7,7,8,0,7,9,1,8] -- um codigo para teste
      bits = foldr1 (++) (encodeDigits code)
      xs = [x*lineWidth | x <- [0..112]]
      hs = [barHeight x | x <- [0..112]]
      cs = map svgColor bits
      stringList = zipWith3 svgBar xs hs cs
  putStr (svgDoc stringList)

