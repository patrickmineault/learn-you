import Data.Complex
import Debug.Trace

-- This file implements polynomial multiplication, first naively, then via FFT.
type Cf = Complex Float
type Poly = [Cf]

-- Polynomial evaluation
eval :: Poly -> Cf -> Cf
eval p x = sum $ zipWith (*) p (map (x^) [0..])

-- Pointwise addition of polynomials
infixr 5 .+
(.+) :: Poly -> Poly -> Poly
a .+ [] = a
[] .+ b = b
(a:as) .+ (b:bs) = a+b:(as .+ bs)

-- Subtraction
infixr 5 .-
(.-) :: Poly -> Poly -> Poly
a .- [] = a
[] .- b = [-b_ | b_ <- b]
(a:as) .- (b:bs) = a-b:(as .- bs)

-- elementwise multiplication
elemProd :: Poly -> Poly -> Poly
elemProd a [] = []
elemProd [] b = []
elemProd (a:as) (b:bs) = (a*b):(elemProd as bs)

-- Naive polynomial multiplication
infixr 5 .*
(.*) :: Poly -> Poly -> Poly
[] .* b = []
a .* [] = []
(a:as) .* bv = [a * b | b <- bv] .+ (0:(as .* bv))

-- Calculate the FFT of the polynomial p
fft :: Poly -> Poly
fft [] = []
fft [a] = [a]
fft p = y where
    n = length p
    pe = [p !! i | i <- [0..n-1], even i]
    po = [p !! i | i <- [0..n-1], odd i]
    ye = fft pe
    yo = fft po
    omegaj = [ cis((-2.0) * pi * (fromIntegral x) / (fromIntegral n)) | x <- [ 0 .. ((length yo) - 1) ] ]
    yp = zipWith (*) omegaj yo
    yl = zipWith (+) ye yp
    yr = zipWith (-) ye yp
    y = (yl ++ yr)

main :: IO()
main = putStr (show $ fft [1, 1, 0, 0])

